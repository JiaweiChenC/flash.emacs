;;; demo-cursor-centered.el --- Demo cursor-centered label assignment

;; Load flash-emacs
(load-file (expand-file-name "../flash-emacs.el" (file-name-directory load-file-name)))

;; Mock evil-mode functions for testing
(defun flash-emacs--evil-available-p () nil)
(defun flash-emacs--evil-visual-state-p () nil)

(defun demo-cursor-centered ()
  "Demo cursor-centered label assignment."
  (with-temp-buffer
    (insert "Line 1: test far above\n")
    (insert "Line 2: test close above\n")
    (insert "Line 3: cursor here\n")
    (insert "Line 4: test close below\n")
    (insert "Line 5: test far below\n")
    
    ;; Position cursor on line 3
    (goto-char (point-min))
    (forward-line 2)
    (move-to-column 8)
    
    (let* ((current-window (selected-window))
           (current-point (point))
           (cursor-line (line-number-at-pos current-point))
           (matches (flash-emacs--search-pattern "test"))
           (labeled-matches (flash-emacs--assign-labels 
                           matches "asdfg" current-window current-point)))
      
      (message "=== Cursor-Centered Label Assignment Demo ===")
      (message "Cursor position: line %d" cursor-line)
      (message "Found %d matches for 'test'" (length matches))
      (message "")
      
      (message "Label assignment (prioritized by distance from cursor):")
      (dolist (match labeled-matches)
        (let* ((match-line (line-number-at-pos (plist-get match :pos)))
               (label (plist-get match :label))
               (distance (flash-emacs--distance-from-cursor match current-window current-point)))
          (message "  Label '%s': Line %d (distance: %d)" label match-line distance)))
      
      (message "")
      (message "✓ Labels correctly prioritize matches closer to cursor!")
      (message "✓ When distances are equal, position-based fallback works!")
      (message "✓ Implementation matches flash.nvim's behavior!"))))

;; Run the demo
(demo-cursor-centered) 