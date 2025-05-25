;;; test-multichar.el --- Test multi-character functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test multi-character search patterns and jump label detection

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun test-multichar-search ()
  "Test multi-character search before showing labels."
  (with-temp-buffer
    (insert "test testing tester tests testament")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      ;; Test that single character 't' finds many matches
      (let ((matches-t (flash-emacs--search-pattern "t")))
        (message "Found %d matches for 't'" (length matches-t))
        (cl-assert (> (length matches-t) 5) nil "Should find many 't' matches"))
      
      ;; Test that 'te' narrows it down
      (let ((matches-te (flash-emacs--search-pattern "te")))
        (message "Found %d matches for 'te'" (length matches-te))
        (cl-assert (> (length matches-te) 3) nil "Should find several 'te' matches")
        (cl-assert (< (length matches-te) (length (flash-emacs--search-pattern "t"))) 
                  nil "'te' should find fewer matches than 't'"))
      
      ;; Test that 'test' narrows it down further
      (let ((matches-test (flash-emacs--search-pattern "test")))
        (message "Found %d matches for 'test'" (length matches-test))
        (cl-assert (> (length matches-test) 2) nil "Should find multiple 'test' matches")
        (cl-assert (< (length matches-test) (length (flash-emacs--search-pattern "te"))) 
                  nil "'test' should find fewer matches than 'te'")))))

(defun test-jump-label-detection ()
  "Test jump label detection logic."
  (with-temp-buffer
    (insert "apple banana cherry date elderberry")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      ;; Get matches and assign labels
      (let* ((matches (flash-emacs--search-pattern "e"))
             (current-window (selected-window))
             (current-point (point))
             (labeled-matches (flash-emacs--assign-labels matches flash-emacs-labels
                                                         current-window current-point)))
        (message "Found %d labeled matches for 'e'" (length labeled-matches))
        
        ;; Test jump label detection
        (let ((first-label (plist-get (car labeled-matches) :label)))
          (when first-label
            ;; Test that "e" + first-label is detected as a jump
            (let ((jump-label (flash-emacs--check-jump-label 
                              (concat "e" first-label) "e" labeled-matches)))
              (cl-assert (string= jump-label first-label) nil 
                        "Should detect jump label correctly"))
            
            ;; Test that "ex" (if x is not a label) is not detected as a jump
            (let ((non-jump (flash-emacs--check-jump-label "ex" "e" labeled-matches)))
              (unless (cl-some (lambda (m) (string= (plist-get m :label) "x")) labeled-matches)
                (cl-assert (null non-jump) nil "Should not detect non-label as jump")))))))))

(defun test-pattern-building ()
  "Test that patterns build correctly before jumping."
  (let ((state (flash-emacs--create-state)))
    
    ;; Test initial state
    (cl-assert (string= (flash-emacs--get-pattern state) "") nil "Initial pattern should be empty")
    
    ;; Test pattern building
    (flash-emacs--set-pattern state "t")
    (cl-assert (string= (flash-emacs--get-pattern state) "t") nil "Pattern should be 't'")
    
    (flash-emacs--set-pattern state "te")
    (cl-assert (string= (flash-emacs--get-pattern state) "te") nil "Pattern should be 'te'")
    
    (flash-emacs--set-pattern state "test")
    (cl-assert (string= (flash-emacs--get-pattern state) "test") nil "Pattern should be 'test'")))

(defun demo-multichar-behavior ()
  "Demonstrate the multi-character behavior."
  (interactive)
  (with-temp-buffer
    (insert "This is a test of the flash-emacs multi-character functionality.
We have test, testing, tester, tests, and testament.
Also some other words like the, then, there, these, them.
Try typing 'te' first, then a label to jump!")
    (goto-char (point-min))
    
    (switch-to-buffer (current-buffer))
    (flash-emacs-mode 1)
    
    (message "Multi-character demo ready!

Try this sequence:
1. Press C-c j to start flash jump
2. Type 'te' (you'll see it narrow down the matches)
3. Then type a label (like 'a', 's', 'd') to jump

Notice how:
- Typing 't' shows many matches
- Typing 'te' shows fewer, more specific matches  
- Only then do you use labels to jump")))

(defun run-multichar-tests ()
  "Run all multi-character tests."
  (message "Running multi-character flash-emacs tests...")
  
  (condition-case err
      (progn
        (test-pattern-building)
        (message "✓ Pattern building test passed")
        
        (test-multichar-search)
        (message "✓ Multi-character search test passed")
        
        (test-jump-label-detection)
        (message "✓ Jump label detection test passed")
        
        (message "✓ All multi-character tests passed successfully!"))
    (error
     (message "✗ Multi-character test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-multichar-tests)

(provide 'test-multichar)

;;; test-multichar.el ends here
