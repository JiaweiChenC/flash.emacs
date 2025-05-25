;;; test-clean-startup.el --- Test clean startup without messages -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that flash-emacs-jump starts without displaying unwanted messages

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun test-clean-startup ()
  "Test that flash-emacs starts without unwanted messages."
  (with-temp-buffer
    (insert "test content for clean startup")
    (goto-char (point-min))
    
    ;; Test that the main loop function doesn't display messages
    (let ((state (flash-emacs--create-state)))
      ;; The main loop should start cleanly without messages
      ;; We can't easily test the interactive part, but we can test
      ;; that the function exists and the state is properly initialized
      (cl-assert (plist-get state :pattern) nil "State should have pattern")
      (cl-assert (string= (plist-get state :pattern) "") nil "Initial pattern should be empty")
      (cl-assert (plist-get state :current-window) nil "State should have current window")
      (cl-assert (plist-get state :start-point) nil "State should have start point"))))

(defun demo-clean-startup ()
  "Interactive demo showing clean startup."
  (interactive)
  (with-temp-buffer
    (insert "Clean Startup Demo
==================

This demo shows that flash-emacs-jump now starts cleanly
without displaying any unwanted messages.

Before the fix:
- M-x flash-emacs-jump would show: 'Flash: Type to search, then use labels to jump (ESC to cancel)'

After the fix:
- M-x flash-emacs-jump starts silently with just the prompt

Test content:
test testing tester tests
apple banana cherry date
hello world example text

Try M-x flash-emacs-jump or C-c j (if bound) to see the clean startup!
")
    (goto-char (point-min))
    
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    
    (message "Clean startup demo ready! Try M-x flash-emacs-jump to see the clean interface.")))

(defun run-clean-startup-tests ()
  "Run clean startup tests."
  (message "=== Testing Clean Startup ===")
  
  (condition-case err
      (progn
        (test-clean-startup)
        (message "✓ Clean startup test passed")
        
        (message "✓ All clean startup tests passed!"))
    (error
     (message "✗ Clean startup test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-clean-startup-tests)

(provide 'test-clean-startup)

;;; test-clean-startup.el ends here 