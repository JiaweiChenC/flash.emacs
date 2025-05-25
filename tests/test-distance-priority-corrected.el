;;; test-distance-priority-corrected.el --- Test distance-based label assignment (corrected)

;; Test that labels are assigned based on distance from cursor,
;; with position-based fallback when distances are equal

(require 'cl-lib)

;; Load flash-emacs
(load-file (expand-file-name "../flash-emacs.el" (file-name-directory load-file-name)))

;; Mock evil-mode functions for testing
(defun flash-emacs--evil-available-p () nil)
(defun flash-emacs--evil-visual-state-p () nil)

(defun test-distance-priority-corrected ()
  "Test that labels are assigned by distance from cursor, with position fallback."
  (with-temp-buffer
    (insert "Line 1: test here\n")      ; Line 1, distance = 160
    (insert "Line 2: test there\n")     ; Line 2, distance = 80 (closer)
    (insert "Line 3: cursor position\n") ; Line 3 - cursor here
    (insert "Line 4: test nearby\n")    ; Line 4, distance = 80 (same as line 2)
    (insert "Line 5: test faraway\n")   ; Line 5, distance = 160
    
    ;; Position cursor on line 3, column 8
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
                                       (line-number-at-pos (plist-get match :pos))
                                       (flash-emacs--distance-from-cursor match current-window current-point)))
                               labeled-matches)))
        
        ;; Sort by label assignment order (first label = highest priority)
        (setq match-info (sort match-info (lambda (a b) 
                                           (string< (nth 1 a) (nth 1 b)))))
        
        ;; Verify sorting: 
        ;; 1. Line 2 and 4 both have distance 80 (closest)
        ;; 2. Line 2 comes first due to position-based fallback
        ;; 3. Line 4 comes second
        ;; 4. Line 1 and 5 both have distance 160 (farther)
        ;; 5. Line 1 comes before line 5 due to position
        
        (let ((first-match (nth 0 match-info))
              (second-match (nth 1 match-info))
              (third-match (nth 2 match-info))
              (fourth-match (nth 3 match-info)))
          
          ;; First match should be line 2 with label 'a'
          (unless (and (= (nth 2 first-match) 2)
                      (string= (nth 1 first-match) "a")
                      (= (nth 3 first-match) 80))
            (error "Expected first match: line 2, label 'a', distance 80. Got: line %d, label '%s', distance %d"
                   (nth 2 first-match) (nth 1 first-match) (nth 3 first-match)))
          
          ;; Second match should be line 4 with label 's'
          (unless (and (= (nth 2 second-match) 4)
                      (string= (nth 1 second-match) "s")
                      (= (nth 3 second-match) 80))
            (error "Expected second match: line 4, label 's', distance 80. Got: line %d, label '%s', distance %d"
                   (nth 2 second-match) (nth 1 second-match) (nth 3 second-match)))
          
          ;; Third match should be line 1 with label 'd'
          (unless (and (= (nth 2 third-match) 1)
                      (string= (nth 1 third-match) "d")
                      (= (nth 3 third-match) 160))
            (error "Expected third match: line 1, label 'd', distance 160. Got: line %d, label '%s', distance %d"
                   (nth 2 third-match) (nth 1 third-match) (nth 3 third-match)))
          
          ;; Fourth match should be line 5 with label 'f'
          (unless (and (= (nth 2 fourth-match) 5)
                      (string= (nth 1 fourth-match) "f")
                      (= (nth 3 fourth-match) 160))
            (error "Expected fourth match: line 5, label 'f', distance 160. Got: line %d, label '%s', distance %d"
                   (nth 2 fourth-match) (nth 1 fourth-match) (nth 3 fourth-match))))
        
        ;; Print results for verification
        (message "Distance-based label assignment (corrected):")
        (dolist (info match-info)
          (message "  Line %d: label '%s', distance %d" (nth 2 info) (nth 1 info) (nth 3 info)))
        
        (message "✓ Distance-based label assignment test passed!")))))

;; Run the test
(condition-case err
    (test-distance-priority-corrected)
  (error 
   (message "✗ Distance-based label assignment test failed: %s" (error-message-string err))
   (kill-emacs 1)))

(message "All distance-based label assignment tests completed successfully!") 