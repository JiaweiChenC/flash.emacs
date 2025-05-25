;;; verify-multichar.el --- Verify multi-character functionality works correctly -*- lexical-binding: t; -*-

;;; Commentary:
;; This file verifies that the multi-character functionality works as expected

;;; Code:

(load-file "flash-emacs.el")

(defun verify-multichar-step-by-step ()
  "Step-by-step verification of multi-character functionality."
  (interactive)
  
  ;; Create test buffer
  (let ((test-buffer (get-buffer-create "*Flash Multi-char Verification*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (insert "Testing multi-character functionality:

test testing tester tests testament
function definition defun defvar defcustom
the then there these them they
apple application apply approach
hello world help health

Try this step by step:
1. Press C-c j to start flash jump
2. Type 't' - you should see MANY matches highlighted
3. Type 'e' - should narrow to 'te' matches (fewer)
4. Type 's' - should narrow to 'tes' matches (even fewer)
5. Type 't' - should narrow to 'test' matches (very specific)
6. Now type a LABEL (like 'a', 's', 'd') to jump!

The key insight: 
- Characters build the search pattern until you type a LABEL
- Labels only work AFTER you have matches with labels assigned
- 'test' + 'a' = jump to label 'a' (if 'a' is a label)
- 'test' + 'e' = search for 'teste' (if 'e' is not a label)
")
      (goto-char (point-min)))
    
    (switch-to-buffer test-buffer)
    (flash-emacs-mode 1)
    
    (message "Multi-character verification ready! Follow the instructions in the buffer.")))

(defun test-specific-multichar-scenario ()
  "Test a specific multi-character scenario programmatically."
  (interactive)
  
  (with-temp-buffer
    (insert "test testing tester tests testament")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      ;; Simulate the multi-character input process
      (let ((state (flash-emacs--create-state)))
        
        ;; Step 1: Start with empty pattern
        (message "Step 1: Initial state - pattern: '%s'" (flash-emacs--get-pattern state))
        
        ;; Step 2: Add 't'
        (flash-emacs--set-pattern state "t")
        (let ((matches-t (flash-emacs--update-search state)))
          (message "Step 2: After 't' - pattern: '%s', matches: %d" 
                   (flash-emacs--get-pattern state) (length matches-t)))
        
        ;; Step 3: Add 'e' to make 'te'
        (flash-emacs--set-pattern state "te")
        (let ((matches-te (flash-emacs--update-search state)))
          (message "Step 3: After 'te' - pattern: '%s', matches: %d" 
                   (flash-emacs--get-pattern state) (length matches-te)))
        
        ;; Step 4: Add 's' to make 'tes'
        (flash-emacs--set-pattern state "tes")
        (let ((matches-tes (flash-emacs--update-search state)))
          (message "Step 4: After 'tes' - pattern: '%s', matches: %d" 
                   (flash-emacs--get-pattern state) (length matches-tes)))
        
        ;; Step 5: Add 't' to make 'test'
        (flash-emacs--set-pattern state "test")
        (let ((matches-test (flash-emacs--update-search state)))
          (message "Step 5: After 'test' - pattern: '%s', matches: %d" 
                   (flash-emacs--get-pattern state) (length matches-test))
          
          ;; Show the labels assigned
          (when matches-test
            (message "Labels assigned:")
            (dolist (match matches-test)
              (when (plist-get match :label)
                (message "  Position %d: label '%s' for text '%s'" 
                         (plist-get match :pos)
                         (plist-get match :label)
                         (plist-get match :text)))))
          
          ;; Step 6: Test jump label detection
          (when matches-test
            (let ((first-label (plist-get (car matches-test) :label)))
              (when first-label
                (let ((jump-detected (flash-emacs--check-jump-label 
                                     (concat "test" first-label) "test" matches-test)))
                  (message "Step 6: Jump label detection - 'test%s' detected as jump: %s" 
                           first-label (if jump-detected "YES" "NO")))))))
        
        (flash-emacs--clear-overlays)))))

(defun demonstrate-multichar-difference ()
  "Demonstrate the difference between search chars and jump labels."
  (interactive)
  
  (with-temp-buffer
    (insert "test testing tester tests testament apple banana")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      (let* ((state (flash-emacs--create-state))
             (current-pattern "test")
             (matches (progn 
                       (flash-emacs--set-pattern state current-pattern)
                       (flash-emacs--update-search state))))
        
        (message "=== Multi-character Behavior Demonstration ===")
        (message "Current pattern: '%s'" current-pattern)
        (message "Found %d matches" (length matches))
        
        (when matches
          (message "Available labels:")
          (dolist (match matches)
            (when (plist-get match :label)
              (message "  Label '%s' at position %d" 
                       (plist-get match :label)
                       (plist-get match :pos))))
          
          ;; Test different next characters
          (let ((test-chars '("a" "e" "i" "s")))
            (message "\nTesting what happens with different next characters:")
            (dolist (char test-chars)
              (let* ((new-pattern (concat current-pattern char))
                     (is-jump (flash-emacs--check-jump-label new-pattern current-pattern matches)))
                (if is-jump
                    (message "  '%s' + '%s' = JUMP to label '%s'" current-pattern char is-jump)
                  (message "  '%s' + '%s' = SEARCH for '%s'" current-pattern char new-pattern))))))
        
        (flash-emacs--clear-overlays)))))

(defun run-all-verifications ()
  "Run all verification tests."
  (interactive)
  (message "=== Running Multi-character Verification Tests ===")
  
  (test-specific-multichar-scenario)
  (message "")
  (demonstrate-multichar-difference)
  (message "")
  (message "=== Verification Complete ===")
  (message "Now try the interactive test: M-x verify-multichar-step-by-step"))

;; Auto-run when loaded
(when (called-interactively-p 'any)
  (run-all-verifications))

(provide 'verify-multichar)

;;; verify-multichar.el ends here 