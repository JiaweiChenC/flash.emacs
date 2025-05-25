;;; test-distance-priority.el --- Test distance-based label assignment

;; Test that labels are assigned based on distance from cursor

(require 'cl-lib)

;; Load flash-emacs
(load-file (expand-file-name "../flash-emacs.el" (file-name-directory load-file-name)))

;; Mock evil-mode functions for testing
(defun flash-emacs--evil-available-p () nil)
(defun flash-emacs--evil-visual-state-p () nil)

(defun test-distance-priority ()
  "Test that labels are assigned by distance from cursor."
  (with-temp-buffer
    (insert "Line 1: test here\n")
    (insert "Line 2: test there\n") 
    (insert "Line 3: cursor position\n")
    (insert "Line 4: test nearby\n")
    (insert "Line 5: test faraway\n")
    
    ;; Position cursor on line 3
    (goto-char (point-min))
    (forward-line 2)
    (move-to-column 8) ; Position at "cursor"
    
    (let* ((current-window (selected-window))
           (current-point (point))
           ;; Search for "test" - should find 4 matches
           (matches (flash-emacs--search-pattern "test"))
           ;; Assign labels with distance priority
           (labeled-matches (flash-emacs--assign-labels 
                           matches 
                           "asdfghjkl" 
                           current-window 
                           current-point)))
      
      ;; Verify we found 4 matches
      (unless (= (length labeled-matches) 4)
        (error "Expected 4 matches, got %d" (length labeled-matches)))
      
      ;; Get positions and labels
      (let ((match-info (mapcar (lambda (match)
                                 (list (plist-get match :pos)
                                       (plist-get match :label)
                                       (line-number-at-pos (plist-get match :pos))))
                               labeled-matches)))
        
        ;; Sort by label assignment order (first label = closest)
        (setq match-info (sort match-info (lambda (a b) 
                                           (string< (nth 1 a) (nth 1 b)))))
        
        ;; Verify closest match gets first label
        ;; Line 4 "test nearby" should be closest to cursor on line 3
        (let ((first-match-line (nth 2 (nth 0 match-info)))
              (first-label (nth 1 (nth 0 match-info))))
          (unless (= first-match-line 4)
            (error "Expected closest match on line 4, got line %d" first-match-line))
          (unless (string= first-label "a")
            (error "Expected first label 'a', got '%s'" first-label)))
        
        ;; Print results for verification
        (message "Distance-based label assignment:")
        (dolist (info match-info)
          (message "  Line %d: label '%s'" (nth 2 info) (nth 1 info)))
        
        (message "✓ Distance-based label assignment test passed!")))))

;; Run the test
(condition-case err
    (test-distance-priority)
  (error 
   (message "✗ Distance-based label assignment test failed: %s" (error-message-string err))
   (kill-emacs 1)))

(message "All distance-based label assignment tests completed successfully!") 