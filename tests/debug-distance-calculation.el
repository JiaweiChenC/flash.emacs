;;; debug-distance-calculation.el --- Debug distance calculation

;; Debug distance calculation to understand sorting

(require 'cl-lib)

;; Load flash-emacs
(load-file (expand-file-name "../flash-emacs.el" (file-name-directory load-file-name)))

;; Mock evil-mode functions for testing
(defun flash-emacs--evil-available-p () nil)
(defun flash-emacs--evil-visual-state-p () nil)

(defun debug-distance-calculation ()
  "Debug distance calculation to understand sorting."
  (with-temp-buffer
    (insert "Line 1: test here\n")      ; Line 1, col 9
    (insert "Line 2: test there\n")     ; Line 2, col 9  
    (insert "Line 3: cursor position\n") ; Line 3 - cursor here
    (insert "Line 4: test nearby\n")    ; Line 4, col 9
    (insert "Line 5: test faraway\n")   ; Line 5, col 9
    
    ;; Position cursor on line 3, column 8
    (goto-char (point-min))
    (forward-line 2)
    (move-to-column 8) ; Position at "cursor"
    
    (let* ((current-window (selected-window))
           (current-point (point))
           (cursor-line (line-number-at-pos current-point))
           (cursor-col (current-column))
           (window-width (window-total-width))
           ;; Search for "test" - should find 4 matches
           (matches (flash-emacs--search-pattern "test")))
      
      (message "Cursor position: line %d, col %d" cursor-line cursor-col)
      (message "Window width: %d" window-width)
      (message "Found %d matches:" (length matches))
      
      ;; Calculate and display distance for each match
      (dolist (match matches)
        (let* ((match-pos (plist-get match :pos))
               (match-line (line-number-at-pos match-pos))
               (match-col (save-excursion
                           (goto-char match-pos)
                           (current-column)))
               (distance (flash-emacs--distance-from-cursor match current-window current-point))
               ;; Manual calculation to verify
               (cursor-1d (+ (* cursor-line window-width) cursor-col))
               (match-1d (+ (* match-line window-width) match-col))
               (manual-distance (abs (- cursor-1d match-1d))))
          
          (message "  Line %d, col %d: distance=%d (manual=%d)" 
                   match-line match-col distance manual-distance)))
      
      ;; Now test the sorting
      (let ((labeled-matches (flash-emacs--assign-labels 
                            matches 
                            "asdfghjkl" 
                            current-window 
                            current-point)))
        
        (message "\nAfter sorting and labeling:")
        (dolist (match labeled-matches)
          (let* ((match-pos (plist-get match :pos))
                 (match-line (line-number-at-pos match-pos))
                 (match-col (save-excursion
                             (goto-char match-pos)
                             (current-column)))
                 (label (plist-get match :label))
                 (distance (flash-emacs--distance-from-cursor match current-window current-point)))
            (message "  Label '%s': Line %d, col %d, distance=%d" 
                     label match-line match-col distance)))))))

;; Run the debug
(debug-distance-calculation) 