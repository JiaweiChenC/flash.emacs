;;; test-label-conflicts.el --- Test label conflict avoidance -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that labels don't conflict with search pattern continuation

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun test-label-conflict-avoidance ()
  "Test that labels avoid conflicts with search pattern continuation."
  (with-temp-buffer
    (insert "test testing tester tests testament tea team")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      ;; Test with pattern "te" - should avoid labels that would conflict with "tea", "tes", etc.
      (let* ((pattern "te")
             (all-labels "asdfghjklqwertyuiopzxcvbnm")
             (windows (list (selected-window)))
             (filtered-labels (flash-emacs--filter-labels-for-pattern all-labels pattern windows)))
        
        (message "Original labels: %s" all-labels)
        (message "Pattern: '%s'" pattern)
        (message "Filtered labels: %s" filtered-labels)
        
        ;; Check that 'a' and 's' are removed (because "tea" and "tes" exist)
        (cl-assert (not (string-match-p "a" filtered-labels)) nil 
                  "Label 'a' should be filtered out (conflicts with 'tea')")
        (cl-assert (not (string-match-p "s" filtered-labels)) nil 
                  "Label 's' should be filtered out (conflicts with 'tes')")
        
        ;; Check that other labels remain
        (cl-assert (string-match-p "d" filtered-labels) nil 
                  "Label 'd' should remain (no conflict)")))))

(defun test-skip-pattern-creation ()
  "Test skip pattern creation."
  (let ((pattern1 (flash-emacs--create-skip-pattern "test"))
        (pattern2 (flash-emacs--create-skip-pattern "te"))
        (pattern3 (flash-emacs--create-skip-pattern "")))
    
    (message "Skip pattern for 'test': %s" pattern1)
    (message "Skip pattern for 'te': %s" pattern2)
    (message "Skip pattern for '': %s" pattern3)
    
    (cl-assert (string= pattern1 "test.") nil "Skip pattern for 'test' should be 'test.'")
    (cl-assert (string= pattern2 "te.") nil "Skip pattern for 'te' should be 'te.'")
    (cl-assert (null pattern3) nil "Skip pattern for empty string should be nil")))

(defun test-conflicting-labels-detection ()
  "Test detection of conflicting labels."
  (with-temp-buffer
    (insert "test testing tester tests testament tea team")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      (let* ((pattern "te")
             (labels '("a" "s" "d" "f" "g"))
             (conflicting (flash-emacs--find-conflicting-labels pattern labels (selected-window))))
        
        (message "Pattern: '%s'" pattern)
        (message "Test labels: %s" labels)
        (message "Conflicting labels: %s" conflicting)
        
        ;; Should find 'a' and 's' as conflicting
        (cl-assert (member "a" conflicting) nil "Should detect 'a' as conflicting (tea)")
        (cl-assert (member "s" conflicting) nil "Should detect 's' as conflicting (tes)")
        (cl-assert (not (member "d" conflicting)) nil "Should not detect 'd' as conflicting")))))

(defun demonstrate-conflict-avoidance ()
  "Demonstrate how conflict avoidance works."
  (interactive)
  (with-temp-buffer
    (insert "Testing conflict avoidance:

test testing tester tests testament
tea team teach teacher
apple application apply

Try this:
1. Press C-c j
2. Type 'te' 
3. Notice which labels are available
4. Labels 'a' and 's' should be MISSING because:
   - 'te' + 'a' would search for 'tea' (not jump)
   - 'te' + 's' would search for 'tes' (not jump)
5. Other labels like 'd', 'f', 'g' should be available for jumping
")
    (goto-char (point-min))
    
    (switch-to-buffer (current-buffer))
    (flash-emacs-mode 1)
    
    (message "Conflict avoidance demo ready! Try the steps above.")))

(defun run-conflict-tests ()
  "Run all conflict avoidance tests."
  (message "=== Testing Label Conflict Avoidance ===")
  
  (condition-case err
      (progn
        (test-skip-pattern-creation)
        (message "✓ Skip pattern creation test passed")
        
        (test-conflicting-labels-detection)
        (message "✓ Conflicting labels detection test passed")
        
        (test-label-conflict-avoidance)
        (message "✓ Label conflict avoidance test passed")
        
        (message "✓ All conflict avoidance tests passed!"))
    (error
     (message "✗ Conflict test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-conflict-tests)

(provide 'test-label-conflicts)

;;; test-label-conflicts.el ends here 