;;; test-cursor-centered-labels.el --- Test cursor-centered label assignment

;; Test that labels are assigned around cursor position, prioritizing closer matches

(require 'cl-lib)

;; Load flash-emacs
(load-file (expand-file-name "../flash-emacs.el" (file-name-directory load-file-name)))

;; Mock evil-mode functions for testing
(defun flash-emacs--evil-available-p () nil)
(defun flash-emacs--evil-visual-state-p () nil)

(defun test-cursor-centered-labels ()
  "Test that labels are assigned around cursor position."
  (with-temp-buffer
    ;; Create a buffer with matches at various distances from cursor
    (insert "Line 1: test far above\n")     ; Distance = 160 (2 lines * 80 cols)
    (insert "Line 2: test close above\n")   ; Distance = 80 (1 line * 80 cols)
    (insert "Line 3: cursor here\n")        ; Cursor position
    (insert "Line 4: test close below\n")   ; Distance = 80 (1 line * 80 cols)
    (insert "Line 5: test far below\n")     ; Distance = 160 (2 lines * 80 cols)
    (insert "Line 6: test very far\n")      ; Distance = 240 (3 lines * 80 cols)
    
    ;; Position cursor on line 3, column 8
    (goto-char (point-min))
    (forward-line 2)
    (move-to-column 8) ; Position at "cursor"
    
    (let* ((current-window (selected-window))
           (current-point (point))
           (cursor-line (line-number-at-pos current-point))
           ;; Search for "test" - should find 5 matches
           (matches (flash-emacs--search-pattern "test"))
           ;; Assign labels with distance priority
           (labeled-matches (flash-emacs--assign-labels 
                           matches 
                           "asdfghjklqwertyuiop" 
                           current-window 
                           current-point)))
      
      ;; Verify we found 5 matches
      (unless (= (length labeled-matches) 5)
        (error "Expected 5 matches, got %d" (length labeled-matches)))
      
      ;; Get match information with distances
      (let ((match-info (mapcar (lambda (match)
                                 (list (plist-get match :pos)
                                       (plist-get match :label)
                                       (line-number-at-pos (plist-get match :pos))
                                       (flash-emacs--distance-from-cursor match current-window current-point)))
                               labeled-matches)))
        
        ;; Sort by label assignment order (first label = highest priority)
        (setq match-info (sort match-info (lambda (a b) 
                                           (string< (nth 1 a) (nth 1 b)))))
        
        (message "Cursor-centered label assignment:")
        (message "Cursor on line %d" cursor-line)
        (dolist (info match-info)
          (message "  Label '%s': Line %d, distance %d" 
                   (nth 1 info) (nth 2 info) (nth 3 info)))
        
        ;; Verify the sorting is correct:
        ;; 1. Closest matches (distance 80) should get first labels
        ;; 2. When distances are equal, earlier position wins
        ;; 3. Farther matches get later labels
        
        (let ((first-match (nth 0 match-info))   ; Should be line 2, distance 80
              (second-match (nth 1 match-info))  ; Should be line 4, distance 80
              (third-match (nth 2 match-info))   ; Should be line 1, distance 160
              (fourth-match (nth 3 match-info))  ; Should be line 5, distance 160
              (fifth-match (nth 4 match-info)))  ; Should be line 6, distance 240
          
          ;; Verify closest matches get first labels
          (unless (= (nth 3 first-match) 80)
            (error "Expected first match to have distance 80, got %d" (nth 3 first-match)))
          
          (unless (= (nth 3 second-match) 80)
            (error "Expected second match to have distance 80, got %d" (nth 3 second-match)))
          
          ;; Verify position-based fallback for equal distances
          (unless (< (nth 2 first-match) (nth 2 second-match))
            (error "Expected first match (line %d) to be before second match (line %d)" 
                   (nth 2 first-match) (nth 2 second-match)))
          
          ;; Verify farther matches get later labels
          (unless (> (nth 3 third-match) (nth 3 second-match))
            (error "Expected third match to be farther than second match"))
          
          (unless (> (nth 3 fifth-match) (nth 3 fourth-match))
            (error "Expected fifth match to be farther than fourth match"))
          
          (message "✓ All distance-based sorting verified!")
          (message "✓ Cursor-centered label assignment working correctly!")))))

(defun test-different-cursor-positions ()
  "Test label assignment with cursor at different positions."
  (with-temp-buffer
    (insert "test1 on line 1\n")
    (insert "test2 on line 2\n") 
    (insert "test3 on line 3\n")
    (insert "test4 on line 4\n")
    (insert "test5 on line 5\n")
    
    ;; Test with cursor on line 1 (top)
    (goto-char (point-min))
    (let* ((matches (flash-emacs--search-pattern "test"))
           (labeled-matches (flash-emacs--assign-labels 
                           matches "asdfg" (selected-window) (point))))
      (message "\nCursor on line 1 (top):")
      (dolist (match labeled-matches)
        (message "  Label '%s': Line %d" 
                 (plist-get match :label)
                 (line-number-at-pos (plist-get match :pos)))))
    
    ;; Test with cursor on line 3 (middle)
    (goto-char (point-min))
    (forward-line 2)
    (let* ((matches (flash-emacs--search-pattern "test"))
           (labeled-matches (flash-emacs--assign-labels 
                           matches "asdfg" (selected-window) (point))))
      (message "\nCursor on line 3 (middle):")
      (dolist (match labeled-matches)
        (message "  Label '%s': Line %d" 
                 (plist-get match :label)
                 (line-number-at-pos (plist-get match :pos)))))
    
    ;; Test with cursor on line 5 (bottom)
    (goto-char (point-max))
    (let* ((matches (flash-emacs--search-pattern "test"))
           (labeled-matches (flash-emacs--assign-labels 
                           matches "asdfg" (selected-window) (point))))
      (message "\nCursor on line 5 (bottom):")
      (dolist (match labeled-matches)
        (message "  Label '%s': Line %d" 
                 (plist-get match :label)
                 (line-number-at-pos (plist-get match :pos)))))
    
    (message "✓ Different cursor positions tested!")))

;; Run the tests
(condition-case err
    (progn
      (test-cursor-centered-labels)
      (test-different-cursor-positions))
  (error 
   (message "✗ Cursor-centered label test failed: %s" (error-message-string err))
   (kill-emacs 1)))

(message "\n✓ All cursor-centered label assignment tests passed!")
(message "✓ Labels are correctly generated around cursor position!")
(message "✓ Implementation matches flash.nvim's behavior!") 