;;; test-basic-functionality.el --- Test basic flash-emacs functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test basic flash-emacs functionality to ensure evil integration didn't break anything

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun test-basic-search ()
  "Test basic search functionality."
  (message "=== Testing Basic Search ===")
  
  (with-temp-buffer
    (insert "test testing tester tests")
    (goto-char (point-min))
    
    ;; Test pattern search
    (let ((matches (flash-emacs--search-pattern "test")))
      (cl-assert (= (length matches) 4) nil
                "Should find 4 matches for 'test'")
      (cl-assert (= (plist-get (car matches) :pos) 1) nil
                "First match should be at position 1")))
  
  (message "✓ Basic search tests passed"))

(defun test-label-assignment ()
  "Test label assignment functionality."
  (message "=== Testing Label Assignment ===")
  
  (with-temp-buffer
    (insert "apple banana cherry date")
    (goto-char (point-min))
    
    (let* ((matches (flash-emacs--search-pattern "a"))
           (labeled-matches (flash-emacs--assign-labels matches 
                                                       flash-emacs-labels
                                                       (selected-window)
                                                       (point))))
      (cl-assert (> (length labeled-matches) 0) nil
                "Should have labeled matches")
      (cl-assert (plist-get (car labeled-matches) :label) nil
                "First match should have a label")))
  
  (message "✓ Label assignment tests passed"))

(defun test-state-management ()
  "Test state management functionality."
  (message "=== Testing State Management ===")
  
  (let ((state (flash-emacs--create-state)))
    (cl-assert (string= (flash-emacs--get-pattern state) "") nil
              "Initial pattern should be empty")
    
    (flash-emacs--set-pattern state "test")
    (cl-assert (string= (flash-emacs--get-pattern state) "test") nil
              "Pattern should be set correctly")
    
    (cl-assert (eq (plist-get state :current-window) (selected-window)) nil
              "Should record current window")
    
    (cl-assert (numberp (plist-get state :start-point)) nil
              "Should record start point"))
  
  (message "✓ State management tests passed"))

(defun test-conflict-avoidance ()
  "Test label conflict avoidance."
  (message "=== Testing Conflict Avoidance ===")
  
  (with-temp-buffer
    (insert "tea test team")
    (goto-char (point-min))
    
    ;; For pattern "te", should avoid labels 'a' and 's' because "tea" and "tes" exist
    (let* ((pattern "te")
           (windows (list (selected-window)))
           (filtered-labels (flash-emacs--filter-labels-for-pattern 
                           flash-emacs-labels pattern windows)))
      
      ;; Should not contain 'a' (because "tea" exists)
      (cl-assert (not (cl-find ?a filtered-labels)) nil
                "Should filter out 'a' label due to 'tea' conflict")
      
      ;; Should contain safe labels like 'd', 'f', 'g'
      (cl-assert (cl-find ?d filtered-labels) nil
                "Should keep safe label 'd'")))
  
  (message "✓ Conflict avoidance tests passed"))

(defun test-multi-character-search ()
  "Test multi-character search patterns."
  (message "=== Testing Multi-character Search ===")
  
  (with-temp-buffer
    (insert "test testing tester tests testament")
    (goto-char (point-min))
    
    ;; Test progressive narrowing
    (let ((matches-t (flash-emacs--search-pattern "t"))
          (matches-te (flash-emacs--search-pattern "te"))
          (matches-tes (flash-emacs--search-pattern "tes"))
          (matches-test (flash-emacs--search-pattern "test")))
      
      (cl-assert (>= (length matches-t) (length matches-te)) nil
                "Matches should narrow with longer patterns")
      (cl-assert (>= (length matches-te) (length matches-tes)) nil
                "Matches should narrow with longer patterns")
      (cl-assert (>= (length matches-tes) (length matches-test)) nil
                "Matches should narrow with longer patterns")
      
      (cl-assert (= (length matches-test) 5) nil
                "Should find exactly 5 'test' matches (including 'testament')")))
  
  (message "✓ Multi-character search tests passed"))

(defun test-jump-label-detection ()
  "Test jump label detection logic."
  (message "=== Testing Jump Label Detection ===")
  
  (with-temp-buffer
    (insert "test testing tester")
    (goto-char (point-min))
    
    (let* ((pattern "te")
           (matches (flash-emacs--search-pattern pattern))
           (labeled-matches (flash-emacs--assign-labels matches 
                                                       "dfg" ; Use simple labels
                                                       (selected-window)
                                                       (point))))
      
      ;; Test that 'd' is detected as a jump label
      (let ((label-char (flash-emacs--check-jump-label "ted" "te" labeled-matches)))
        (cl-assert (string= label-char "d") nil
                  "Should detect 'd' as jump label"))
      
      ;; Test that 'x' is not detected as a jump label
      (let ((label-char (flash-emacs--check-jump-label "tex" "te" labeled-matches)))
        (cl-assert (not label-char) nil
                  "Should not detect 'x' as jump label"))))
  
  (message "✓ Jump label detection tests passed"))

(defun run-basic-functionality-tests ()
  "Run all basic functionality tests."
  (message "=== Running Basic Functionality Tests ===")
  
  (condition-case err
      (progn
        (test-basic-search)
        (test-label-assignment)
        (test-state-management)
        (test-conflict-avoidance)
        (test-multi-character-search)
        (test-jump-label-detection)
        
        (message "✓ All basic functionality tests passed!"))
    (error
     (message "✗ Basic functionality test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-basic-functionality-tests)

(provide 'test-basic-functionality)

;;; test-basic-functionality.el ends here 