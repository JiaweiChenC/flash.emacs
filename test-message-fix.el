;;; test-message-fix.el --- Test that unwanted message is gone -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple test to verify the message fix

;;; Code:

(load-file "flash-emacs.el")

(defun test-no-unwanted-message ()
  "Test that flash-emacs-jump doesn't show unwanted messages."
  (with-temp-buffer
    (insert "test content here")
    (goto-char (point-min))
    
    ;; Check that the main loop function doesn't contain the old message
    (let ((main-loop-source (symbol-function 'flash-emacs--main-loop)))
      (message "Main loop function: %S" main-loop-source)
      
      ;; The function should not contain any message calls
      (let ((source-string (format "%S" main-loop-source)))
        (if (string-match-p "Flash.*Type to search" source-string)
            (error "❌ FOUND unwanted message in main loop!")
          (message "✅ No unwanted message found in main loop"))))))

(message "=== Testing Message Fix ===")
(test-no-unwanted-message)
(message "=== Test Complete ===")

;;; test-message-fix.el ends here 