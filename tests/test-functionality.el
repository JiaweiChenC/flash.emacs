;;; test-functionality.el --- Automated tests for flash-emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Automated tests to verify flash-emacs functionality

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun test-search-functionality ()
  "Test the search functionality."
  (with-temp-buffer
    (insert "This is a test buffer with test content.
More test content here.
The quick brown fox jumps over the lazy dog.
Another test line with different content.")
    (goto-char (point-min))
    
    ;; Test exact search - need to be in a window for search to work
    (let ((flash-emacs-multi-window nil)) ; Search only current window
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (let ((matches (flash-emacs--search-pattern "test")))
          (message "Found %d matches for 'test'" (length matches))
          (cl-assert (> (length matches) 0) nil "Should find test matches")
          
          ;; Check first match
          (let ((first-match (car matches)))
            (cl-assert (plist-get first-match :pos) nil "Match should have position")
            (cl-assert (plist-get first-match :end-pos) nil "Match should have end position")
            (cl-assert (plist-get first-match :text) nil "Match should have text")
            (cl-assert (string= (plist-get first-match :text) "test") nil "Match text should be 'test'")))))))

(defun test-label-assignment ()
  "Test label assignment functionality."
  (with-temp-buffer
    (insert "test test test test test")
    (goto-char (point-min))
    
    (let ((flash-emacs-multi-window nil)) ; Search only current window
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (let* ((matches (flash-emacs--search-pattern "test"))
               (labeled-matches (flash-emacs--assign-labels matches 
                                                           flash-emacs-labels
                                                           (selected-window)
                                                           (point))))
          (message "Assigned labels to %d matches" (length labeled-matches))
          (cl-assert (> (length labeled-matches) 0) nil "Should have labeled matches")
          
          ;; Check that labels are assigned
          (dolist (match labeled-matches)
            (cl-assert (plist-get match :label) nil "Each match should have a label"))
          
          ;; Check that first match gets first label
          (let ((first-match (car labeled-matches)))
            (cl-assert (string= (plist-get first-match :label) "a") nil "First match should get label 'a'")))))))

(defun test-state-management ()
  "Test state management functions."
  (let ((state (flash-emacs--create-state)))
    (cl-assert (listp state) nil "State should be a list")
    (cl-assert (string= (flash-emacs--get-pattern state) "") nil "Initial pattern should be empty")
    
    ;; Test pattern setting
    (flash-emacs--set-pattern state "test")
    (cl-assert (string= (flash-emacs--get-pattern state) "test") nil "Pattern should be set to 'test'")
    
    ;; Test matches setting
    (let ((test-matches '((:pos 1 :end-pos 5 :text "test"))))
      (flash-emacs--set-matches state test-matches)
      (cl-assert (equal (flash-emacs--get-matches state) test-matches) nil "Matches should be set correctly"))))

(defun test-basic-search-in-window ()
  "Test basic search in window functionality."
  (with-temp-buffer
    (insert "hello world test hello test world")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let ((matches (flash-emacs--search-in-window "test" (selected-window))))
        (message "Found %d matches for 'test' in window" (length matches))
        (cl-assert (= (length matches) 2) nil "Should find exactly 2 test matches")
        
        ;; Check positions
        (let ((first-match (car matches))
              (second-match (cadr matches)))
          (cl-assert (< (plist-get first-match :pos) (plist-get second-match :pos)) 
                    nil "Matches should be in order"))))))

(defun run-all-tests ()
  "Run all automated tests."
  (message "Running flash-emacs tests...")
  
  (condition-case err
      (progn
        (test-basic-search-in-window)
        (message "✓ Basic search in window test passed")
        
        (test-search-functionality)
        (message "✓ Search functionality test passed")
        
        (test-label-assignment)
        (message "✓ Label assignment test passed")
        
        (test-state-management)
        (message "✓ State management test passed")
        
        (message "✓ All tests passed successfully!"))
    (error
     (message "✗ Test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-all-tests)

(provide 'test-functionality)

;;; test-functionality.el ends here 