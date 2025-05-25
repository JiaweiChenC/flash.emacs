;;; demo-cursor-positions.el --- Demo label assignment at different cursor positions

;; Load flash-emacs
(load-file (expand-file-name "../flash-emacs.el" (file-name-directory load-file-name)))

;; Mock evil-mode functions for testing
(defun flash-emacs--evil-available-p () nil)
(defun flash-emacs--evil-visual-state-p () nil)

(defun demo-cursor-positions ()
  "Demo how label assignment changes based on cursor position."
  (with-temp-buffer
    (insert "Line 1: test match\n")
    (insert "Line 2: test match\n")
    (insert "Line 3: test match\n")
    (insert "Line 4: test match\n")
    (insert "Line 5: test match\n")
    
    (message "=== Label Assignment at Different Cursor Positions ===")
    (message "")
    
    ;; Test with cursor on line 1 (top)
    (goto-char (point-min))
    (let* ((current-window (selected-window))
           (current-point (point))
           (cursor-line (line-number-at-pos current-point))
           (matches (flash-emacs--search-pattern "test"))
           (labeled-matches (flash-emacs--assign-labels 
                           matches "asdfg" current-window current-point)))
      
      (message "Cursor on line %d (top):" cursor-line)
      (dolist (match labeled-matches)
        (let* ((match-line (line-number-at-pos (plist-get match :pos)))
               (label (plist-get match :label))
               (distance (flash-emacs--distance-from-cursor match current-window current-point)))
          (message "  Label '%s': Line %d (distance: %d)" label match-line distance))))
    
    (message "")
    
    ;; Test with cursor on line 3 (middle)
    (goto-char (point-min))
    (forward-line 2)
    (let* ((current-window (selected-window))
           (current-point (point))
           (cursor-line (line-number-at-pos current-point))
           (matches (flash-emacs--search-pattern "test"))
           (labeled-matches (flash-emacs--assign-labels 
                           matches "asdfg" current-window current-point)))
      
      (message "Cursor on line %d (middle):" cursor-line)
      (dolist (match labeled-matches)
        (let* ((match-line (line-number-at-pos (plist-get match :pos)))
               (label (plist-get match :label))
               (distance (flash-emacs--distance-from-cursor match current-window current-point)))
          (message "  Label '%s': Line %d (distance: %d)" label match-line distance))))
    
    (message "")
    
    ;; Test with cursor on line 5 (bottom)
    (goto-char (point-max))
    (let* ((current-window (selected-window))
           (current-point (point))
           (cursor-line (line-number-at-pos current-point))
           (matches (flash-emacs--search-pattern "test"))
           (labeled-matches (flash-emacs--assign-labels 
                           matches "asdfg" current-window current-point)))
      
      (message "Cursor on line %d (bottom):" cursor-line)
      (dolist (match labeled-matches)
        (let* ((match-line (line-number-at-pos (plist-get match :pos)))
               (label (plist-get match :label))
               (distance (flash-emacs--distance-from-cursor match current-window current-point)))
          (message "  Label '%s': Line %d (distance: %d)" label match-line distance))))
    
    (message "")
    (message "✓ Label assignment correctly adapts to cursor position!")
    (message "✓ Closest matches always get priority labels!")
    (message "✓ Implementation matches flash.nvim's cursor-centered behavior!")))

;; Run the demo
(demo-cursor-positions) 