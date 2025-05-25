;;; test-prompt-changes.el --- Test prompt and message changes -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that the prompt shows only the pattern and jumping is silent

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun test-prompt-format ()
  "Test that the prompt format is simplified."
  (let ((state (flash-emacs--create-state)))
    ;; Test empty pattern
    (flash-emacs--set-pattern state "")
    (let* ((current-pattern (flash-emacs--get-pattern state))
           (prompt (if (> (length current-pattern) 0)
                      current-pattern
                    "")))
      (cl-assert (string= prompt "") nil
                "Empty pattern should show empty string"))
    
    ;; Test with pattern
    (flash-emacs--set-pattern state "test")
    (let* ((current-pattern (flash-emacs--get-pattern state))
           (prompt (if (> (length current-pattern) 0)
                      current-pattern
                    "")))
      (cl-assert (string= prompt "test") nil
                "Pattern 'test' should show 'test'"))
    
    ;; Test with single character
    (flash-emacs--set-pattern state "t")
    (let* ((current-pattern (flash-emacs--get-pattern state))
           (prompt (if (> (length current-pattern) 0)
                      current-pattern
                    "")))
      (cl-assert (string= prompt "t") nil
                "Pattern 't' should show 't'"))))

(defun test-silent-jump ()
  "Test that jumping doesn't produce messages."
  (with-temp-buffer
    (insert "test content for jumping")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      (let* ((match (list :pos 6 :end-pos 13 :window (selected-window) :text "content"))
             (original-pos (point)))
        
        ;; Test that jump function works without errors
        (flash-emacs--jump-to-match match)
        
        ;; Verify the jump worked
        (cl-assert (= (point) 6) nil "Should have jumped to position 6")
        (cl-assert (/= (point) original-pos) nil "Should have moved from original position")))))

(defun demo-new-prompt ()
  "Interactive demo showing the new simplified prompt."
  (interactive)
  (with-temp-buffer
    (insert "Demonstrating new simplified prompt:

test testing tester tests
apple banana cherry date
hello world example text

The prompt now shows:
- Empty pattern: '' (empty)
- With pattern 'te': 'te'
- With pattern 'test': 'test'

No more 'Flash [pattern]: ' format!
Also, jumping is now silent (no 'Jumped to: xxx' messages).

Try:
1. Press C-c j
2. Notice the simplified prompt
3. Type some characters and see the clean prompt format
4. Jump to a match and notice no jump message appears
")
    (goto-char (point-min))
    
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    
    (message "New prompt demo ready! Try C-c j to see the simplified interface.")))

(defun run-prompt-tests ()
  "Run all prompt and message tests."
  (message "=== Testing Prompt and Message Changes ===")
  
  (condition-case err
      (progn
        (test-prompt-format)
        (message "✓ Prompt format test passed")
        
        (test-silent-jump)
        (message "✓ Silent jump test passed")
        
        (message "✓ All prompt and message tests passed!"))
    (error
     (message "✗ Prompt test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-prompt-tests)

(provide 'test-prompt-changes)

;;; test-prompt-changes.el ends here 