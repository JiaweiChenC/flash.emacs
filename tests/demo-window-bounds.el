;;; demo-window-bounds.el --- Demo window bounds functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Demonstrate that flash-emacs now respects window bounds like flash.nvim

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun demo-window-bounds ()
  "Interactive demo showing window bounds functionality."
  (interactive)
  (with-temp-buffer
    (insert "# Flash-Emacs Window Bounds Demo

This demo shows how flash-emacs now respects window boundaries,
just like flash.nvim. Only content visible in the current window
will show labels, not content that's scrolled out of view.

## Test Content

Here's a large amount of content to demonstrate scrolling:

")
    
    ;; Add many lines with test patterns
    (dotimes (i 100)
      (insert (format "Line %02d: This line contains TEST patterns for jumping TEST around TEST\n" (1+ i))))
    
    (insert "\n## Instructions

1. Notice how many TEST labels appear in the current view
2. Scroll down (C-v or Page Down) to see different content
3. Use flash-emacs (C-c j) to see labels only on visible TEST occurrences
4. Try splitting windows (C-x 2) to see different bounds per window
5. Compare with old behavior that would label ALL occurrences

## Key Benefits

- **Performance**: Only processes visible content
- **Clarity**: No overwhelming number of labels
- **Intuitive**: Matches what you can actually see
- **Multi-window**: Each window shows its own visible matches

## Technical Details

Flash-emacs now uses:
- `window-start` and `window-end` to get visible bounds
- Line-based filtering to restrict search area
- Per-window bounds calculation for multi-window setups

This matches flash.nvim's behavior exactly!

Try it now with C-c j and type 'TEST' to see the difference!
")
    
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    
    ;; Set up key bindings
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    (local-set-key (kbd "C-c s") (lambda () 
                                   (interactive)
                                   (let* ((window (selected-window))
                                          (bounds (flash-emacs--get-window-bounds window))
                                          (matches (flash-emacs--search-pattern "TEST")))
                                     (message "Window bounds: lines %d-%d, visible TEST matches: %d" 
                                             (car bounds) (cdr bounds) (length matches)))))
    (local-set-key (kbd "C-c t") (lambda ()
                                   (interactive)
                                   (goto-char (point-min))
                                   (let ((total 0))
                                     (while (search-forward "TEST" nil t)
                                       (setq total (1+ total)))
                                     (message "Total TEST occurrences in buffer: %d" total))))
    
    (message "Window bounds demo ready! Try C-c j (flash-emacs), C-c s (show stats), C-c t (total count)")))

(defun demo-multi-window-bounds ()
  "Demo showing different bounds in multiple windows."
  (interactive)
  (with-temp-buffer
    (insert "# Multi-Window Bounds Demo\n\n")
    
    ;; Create content with clear sections
    (dotimes (section 10)
      (insert (format "=== SECTION %d ===\n" (1+ section)))
      (dotimes (line 10)
        (insert (format "Section %d Line %d: JUMP target JUMP here JUMP\n" 
                       (1+ section) (1+ line)))))
    
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    
    ;; Split window
    (split-window-below)
    (other-window 1)
    (goto-char (point-max))
    (recenter)
    (other-window 1)
    
    ;; Set up key bindings for both windows
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    (local-set-key (kbd "C-c b") (lambda ()
                                   (interactive)
                                   (let* ((win1 (selected-window))
                                          (win2 (next-window))
                                          (bounds1 (flash-emacs--get-window-bounds win1))
                                          (bounds2 (flash-emacs--get-window-bounds win2))
                                          (matches1 (flash-emacs--search-in-window "JUMP" win1))
                                          (matches2 (flash-emacs--search-in-window "JUMP" win2)))
                                     (message "Top window: lines %d-%d (%d matches) | Bottom window: lines %d-%d (%d matches)"
                                             (car bounds1) (cdr bounds1) (length matches1)
                                             (car bounds2) (cdr bounds2) (length matches2)))))
    
    (message "Multi-window demo ready! Try C-c j in each window, C-c b for bounds info")))

(defun demo-scrolling-bounds ()
  "Demo showing how bounds change with scrolling."
  (interactive)
  (with-temp-buffer
    (insert "# Scrolling Bounds Demo\n\n")
    
    ;; Create numbered content for easy reference
    (dotimes (i 200)
      (insert (format "%03d: SCROLL target here SCROLL and here SCROLL\n" (1+ i))))
    
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    (local-set-key (kbd "C-c n") (lambda ()
                                   (interactive)
                                   (scroll-up 5)
                                   (let* ((bounds (flash-emacs--get-window-bounds (selected-window)))
                                          (matches (flash-emacs--search-pattern "SCROLL")))
                                     (message "After scroll: lines %d-%d, %d SCROLL matches visible"
                                             (car bounds) (cdr bounds) (length matches)))))
    (local-set-key (kbd "C-c p") (lambda ()
                                   (interactive)
                                   (scroll-down 5)
                                   (let* ((bounds (flash-emacs--get-window-bounds (selected-window)))
                                          (matches (flash-emacs--search-pattern "SCROLL")))
                                     (message "After scroll: lines %d-%d, %d SCROLL matches visible"
                                             (car bounds) (cdr bounds) (length matches)))))
    
    (message "Scrolling demo ready! Try C-c j, C-c n (scroll down), C-c p (scroll up)")))

(defun demo-comparison ()
  "Demo comparing old vs new behavior."
  (interactive)
  (with-temp-buffer
    (insert "# Behavior Comparison Demo

This demo shows the difference between:
- OLD: Searching entire buffer (overwhelming)
- NEW: Searching only visible content (focused)

")
    
    ;; Create a lot of content
    (dotimes (i 150)
      (insert (format "Line %03d: COMPARE this COMPARE and COMPARE that COMPARE\n" (1+ i))))
    
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (scroll-up 20) ; Scroll to middle
    
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    (local-set-key (kbd "C-c o") (lambda ()
                                   (interactive)
                                   ;; Simulate old behavior - search entire buffer
                                   (let ((matches '()))
                                     (save-excursion
                                       (goto-char (point-min))
                                       (while (search-forward "COMPARE" nil t)
                                         (push (match-beginning 0) matches)))
                                     (message "OLD behavior would show %d labels (entire buffer)" 
                                             (length matches)))))
    (local-set-key (kbd "C-c n") (lambda ()
                                   (interactive)
                                   ;; Show new behavior - visible only
                                   (let* ((bounds (flash-emacs--get-window-bounds (selected-window)))
                                          (matches (flash-emacs--search-pattern "COMPARE")))
                                     (message "NEW behavior shows %d labels (lines %d-%d only)"
                                             (length matches) (car bounds) (cdr bounds)))))
    
    (message "Comparison demo ready! Try C-c o (old count), C-c n (new count), C-c j (flash-emacs)")))

;; Run the main demo
(demo-window-bounds)

(provide 'demo-window-bounds)

;;; demo-window-bounds.el ends here 