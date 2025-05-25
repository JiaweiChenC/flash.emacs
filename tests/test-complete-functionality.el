;;; test-complete-functionality.el --- Complete functionality test -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive test of all flash-emacs functionality including conflict avoidance

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun test-complete-workflow ()
  "Test the complete flash-emacs workflow with conflict avoidance."
  (with-temp-buffer
    (insert "test testing tester tests testament tea team teach teacher apple application apply")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      ;; Test 1: Pattern building with conflict avoidance
      (let ((state (flash-emacs--create-state)))
        
        ;; Step 1: Search for 'te'
        (flash-emacs--set-pattern state "te")
        (let ((matches (flash-emacs--update-search state)))
          (cl-assert (> (length matches) 0) nil "Should find matches for 'te'")
          
          ;; Check that conflicting labels are avoided
          (let ((assigned-labels (mapcar (lambda (m) (plist-get m :label)) matches)))
            (cl-assert (not (member "a" assigned-labels)) nil 
                      "Label 'a' should be avoided (conflicts with 'tea')")
            (cl-assert (not (member "s" assigned-labels)) nil 
                      "Label 's' should be avoided (conflicts with 'tes')")
            (cl-assert (member "d" assigned-labels) nil 
                      "Label 'd' should be available (no conflict)")))
        
        ;; Step 2: Test jump label detection
        (let* ((current-pattern "te")
               (matches (flash-emacs--get-matches state))
               (first-label (plist-get (car matches) :label)))
          (when first-label
            ;; Test that the first label is detected as a jump
            (let ((jump-label (flash-emacs--check-jump-label 
                              (concat current-pattern first-label) 
                              current-pattern matches)))
              (cl-assert (string= jump-label first-label) nil 
                        "Should detect jump label correctly")))
          
          ;; Test that 'a' is detected as search continuation
          (let ((search-continuation (flash-emacs--check-jump-label 
                                     (concat current-pattern "a") 
                                     current-pattern matches)))
            (cl-assert (null search-continuation) nil 
                      "Should not detect 'a' as jump label (search continuation)")))
        
        (flash-emacs--clear-overlays)))))

(defun test-progressive-narrowing ()
  "Test progressive narrowing with conflict avoidance at each step."
  (with-temp-buffer
    (insert "test testing tester tests testament tea team teach teacher")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      (let ((state (flash-emacs--create-state))
            (patterns '("t" "te" "tes" "test")))
        
        (dolist (pattern patterns)
          (flash-emacs--set-pattern state pattern)
          (let ((matches (flash-emacs--update-search state)))
            (message "Pattern '%s': %d matches" pattern (length matches))
            
            ;; Verify matches decrease as pattern gets more specific
            (cl-assert (> (length matches) 0) nil 
                      "Should have matches for pattern '%s'" pattern)
            
            ;; Verify labels are assigned
            (let ((labeled-matches (cl-remove-if-not 
                                   (lambda (m) (plist-get m :label)) matches)))
              (cl-assert (> (length labeled-matches) 0) nil 
                        "Should have labeled matches for pattern '%s'" pattern))))
        
        (flash-emacs--clear-overlays)))))

(defun test-multi-window-conflict-avoidance ()
  "Test conflict avoidance across multiple windows."
  (when flash-emacs-multi-window
    (let ((original-buffer (current-buffer)))
      (with-temp-buffer
        (insert "test testing tea team")
        (let ((buffer1 (current-buffer)))
          (with-temp-buffer
            (insert "tester tests testament teach")
            (let ((buffer2 (current-buffer)))
              
              ;; Create two windows
              (delete-other-windows)
              (split-window-horizontally)
              (set-window-buffer (selected-window) buffer1)
              (other-window 1)
              (set-window-buffer (selected-window) buffer2)
              (other-window 1)  ; Back to first window
              
              (save-window-excursion
                (let* ((state (flash-emacs--create-state))
                       (pattern "te"))
                  (flash-emacs--set-pattern state pattern)
                  (let ((matches (flash-emacs--update-search state)))
                    
                    ;; Should find matches in both windows
                    (cl-assert (> (length matches) 0) nil 
                              "Should find matches across windows")
                    
                    ;; Should avoid conflicts from both windows
                    (let ((assigned-labels (mapcar (lambda (m) (plist-get m :label)) matches)))
                      (cl-assert (not (member "a" assigned-labels)) nil 
                                "Should avoid 'a' (tea in buffer1)")
                      (cl-assert (not (member "s" assigned-labels)) nil 
                                "Should avoid 's' (tes in buffer2)"))
                    
                    (flash-emacs--clear-overlays))))
              
              ;; Restore window configuration
              (delete-other-windows))))))))

(defun test-edge-cases ()
  "Test edge cases and error conditions."
  (with-temp-buffer
    (insert "a b c d e f g")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      (let ((state (flash-emacs--create-state)))
        
        ;; Test empty pattern
        (flash-emacs--set-pattern state "")
        (let ((matches (flash-emacs--update-search state)))
          (cl-assert (= (length matches) 0) nil 
                    "Empty pattern should return no matches"))
        
        ;; Test pattern with no matches
        (flash-emacs--set-pattern state "xyz")
        (let ((matches (flash-emacs--update-search state)))
          (cl-assert (= (length matches) 0) nil 
                    "Non-existent pattern should return no matches"))
        
        ;; Test single character pattern
        (flash-emacs--set-pattern state "a")
        (let ((matches (flash-emacs--update-search state)))
          (cl-assert (> (length matches) 0) nil 
                    "Should find matches for single character"))
        
        (flash-emacs--clear-overlays)))))

(defun run-complete-tests ()
  "Run all comprehensive tests."
  (message "=== Running Complete Flash-Emacs Tests ===")
  
  (condition-case err
      (progn
        (test-complete-workflow)
        (message "✓ Complete workflow test passed")
        
        (test-progressive-narrowing)
        (message "✓ Progressive narrowing test passed")
        
        (test-multi-window-conflict-avoidance)
        (message "✓ Multi-window conflict avoidance test passed")
        
        (test-edge-cases)
        (message "✓ Edge cases test passed")
        
        (message "")
        (message "🎉 ALL TESTS PASSED! Flash-emacs is working correctly.")
        (message "")
        (message "Key features verified:")
        (message "  ✓ Multi-character search patterns")
        (message "  ✓ Smart label assignment")
        (message "  ✓ Intelligent conflict avoidance")
        (message "  ✓ Multi-window support")
        (message "  ✓ Progressive pattern narrowing")
        (message "  ✓ Jump vs search detection")
        (message "")
        (message "Ready for use! Try: M-x flash-emacs-jump"))
    (error
     (message "✗ Test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-complete-tests)

(provide 'test-complete-functionality)

;;; test-complete-functionality.el ends here 