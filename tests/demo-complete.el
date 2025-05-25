;;; demo-complete.el --- Complete flash-emacs demonstration -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete demonstration of all flash-emacs features including window bounds

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun demo-complete ()
  "Complete demonstration of all flash-emacs features."
  (interactive)
  (with-temp-buffer
    (insert "# Complete Flash-Emacs Demonstration

Welcome to the complete flash-emacs demo! This showcases all features
that make flash-emacs equivalent to flash.nvim.

## Core Features Demonstrated

### 1. Multi-character Search with Progressive Narrowing
Type 'te' → 'tes' → 'test' to see matches narrow progressively

### 2. Smart Label Assignment  
Labels prioritize current window and distance from cursor

### 3. Intelligent Conflict Avoidance
For pattern 'te', labels 'a' and 's' are avoided if 'tea' and 'tes' exist

### 4. Window-Aware Search (NEW!)
Only visible content gets labels - no overwhelming label count

### 5. Evil-mode Integration
Full support for visual modes and operator-pending mode

### 6. Multi-window Support
Each window shows its own visible matches

## Test Content

Here's content to test all features:

### Section A: Basic Patterns
test testing tester tests testament
apple tea team tear
banana band bank

### Section B: Conflict Testing  
tea test team
sea set seat
pea pet peak

### Section C: Multi-character Patterns
function calculate(a, b) {
    return a + b;
}

const numbers = [1, 2, 3, 4, 5];
const doubled = numbers.map(x => x * 2);

### Section D: Large Content for Window Bounds
")
    
    ;; Add many lines to test window bounds
    (dotimes (i 50)
      (insert (format "Line %02d: SCROLL content SCROLL here SCROLL and SCROLL there\n" (1+ i))))
    
    (insert "
### Section E: Evil-mode Testing
visual character selection test
visual line selection test  
visual block selection test
operator pending test

## Instructions

### Basic Usage
1. Press C-c j to start flash-emacs
2. Type characters to search (try 'te', 'test', 'fun')
3. Press a label character to jump

### Multi-character Testing
1. Type 'te' - see all 'te' matches
2. Type 's' - if 's' is a label, jump; if not, search 'tes'
3. Notice how labels avoid conflicts

### Window Bounds Testing  
1. Scroll down (C-v) to see different content
2. Use C-c j - only visible matches get labels
3. Split window (C-x 2) - each shows its own matches

### Evil-mode Testing (if available)
1. Enter visual mode (v, V, or C-v)
2. Use C-c j to extend selection
3. Try operator + flash: d + C-c j

### Performance Testing
1. Use C-c s to see current window stats
2. Use C-c t to see total buffer stats  
3. Notice how window bounds improve performance

## Key Bindings for This Demo

- C-c j: flash-emacs-jump (main function)
- C-c s: Show window bounds and visible match stats
- C-c t: Show total buffer match count
- C-c w: Toggle multi-window mode
- C-c b: Show bounds info for all windows
- C-c r: Reset to top of buffer

## What Makes This Special

Flash-emacs now provides the exact same experience as flash.nvim:

✓ Multi-character search with progressive narrowing
✓ Smart label assignment avoiding conflicts
✓ Window-aware search for performance and clarity
✓ Evil-mode integration with visual selection support
✓ Multi-window navigation
✓ Configurable behavior

This is a complete, production-ready implementation!

---

Ready to try? Press C-c j and start typing!
")
    
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    
    ;; Set up comprehensive key bindings
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    
    (local-set-key (kbd "C-c s") (lambda () 
                                   (interactive)
                                   (let* ((window (selected-window))
                                          (bounds (flash-emacs--get-window-bounds window))
                                          (test-matches (flash-emacs--search-pattern "test"))
                                          (scroll-matches (flash-emacs--search-pattern "SCROLL")))
                                     (message "Window lines %d-%d: %d 'test' + %d 'SCROLL' = %d total visible matches" 
                                             (car bounds) (cdr bounds) 
                                             (length test-matches) (length scroll-matches)
                                             (+ (length test-matches) (length scroll-matches))))))
    
    (local-set-key (kbd "C-c t") (lambda ()
                                   (interactive)
                                   (let ((test-total 0) (scroll-total 0))
                                     (save-excursion
                                       (goto-char (point-min))
                                       (while (search-forward "test" nil t)
                                         (setq test-total (1+ test-total)))
                                       (goto-char (point-min))
                                       (while (search-forward "SCROLL" nil t)
                                         (setq scroll-total (1+ scroll-total))))
                                     (message "Total in buffer: %d 'test' + %d 'SCROLL' = %d total matches"
                                             test-total scroll-total (+ test-total scroll-total)))))
    
    (local-set-key (kbd "C-c w") (lambda ()
                                   (interactive)
                                   (setq flash-emacs-multi-window (not flash-emacs-multi-window))
                                   (message "Multi-window mode: %s" 
                                           (if flash-emacs-multi-window "ON" "OFF"))))
    
    (local-set-key (kbd "C-c b") (lambda ()
                                   (interactive)
                                   (if (= (length (window-list)) 1)
                                       (message "Only one window - try C-x 2 to split")
                                     (let ((info '()))
                                       (dolist (win (window-list))
                                         (let* ((bounds (flash-emacs--get-window-bounds win))
                                                (matches (flash-emacs--search-in-window "test" win)))
                                           (push (format "Win%d: lines %d-%d (%d matches)" 
                                                        (window-number win) (car bounds) (cdr bounds) (length matches))
                                                 info)))
                                       (message "%s" (string-join (nreverse info) " | "))))))
    
    (local-set-key (kbd "C-c r") (lambda ()
                                   (interactive)
                                   (goto-char (point-min))
                                   (message "Reset to top of buffer")))
    
    ;; Helper function to get window number
    (defun window-number (window)
      (let ((windows (window-list))
            (num 1))
        (while (and windows (not (eq window (car windows))))
          (setq windows (cdr windows)
                num (1+ num)))
        num))
    
    (message "Complete flash-emacs demo ready! Try C-c j, C-c s, C-c t, C-c w, C-c b, C-c r")))

(defun demo-performance-comparison ()
  "Demo showing performance benefits of window bounds."
  (interactive)
  (with-temp-buffer
    (insert "# Performance Comparison Demo\n\n")
    
    ;; Create a very large buffer
    (dotimes (i 1000)
      (insert (format "Line %04d: performance test performance here performance everywhere performance\n" (1+ i))))
    
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (scroll-up 100) ; Scroll to middle
    
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    (local-set-key (kbd "C-c p") (lambda ()
                                   (interactive)
                                   (let* ((start-time (current-time))
                                          (matches (flash-emacs--search-pattern "performance"))
                                          (end-time (current-time))
                                          (elapsed (float-time (time-subtract end-time start-time)))
                                          (bounds (flash-emacs--get-window-bounds (selected-window))))
                                     (message "Window bounds search: %d matches (lines %d-%d) in %.3f seconds"
                                             (length matches) (car bounds) (cdr bounds) elapsed))))
    
    (local-set-key (kbd "C-c o") (lambda ()
                                   (interactive)
                                   (let* ((start-time (current-time))
                                          (matches '()))
                                     (save-excursion
                                       (goto-char (point-min))
                                       (while (search-forward "performance" nil t)
                                         (push (match-beginning 0) matches)))
                                     (let* ((end-time (current-time))
                                            (elapsed (float-time (time-subtract end-time start-time))))
                                       (message "Full buffer search: %d matches in %.3f seconds (would be overwhelming!)"
                                               (length matches) elapsed)))))
    
    (message "Performance demo ready! Try C-c p (new method), C-c o (old method), C-c j (flash-emacs)")))

;; Run the complete demo
(demo-complete)

(provide 'demo-complete)

;;; demo-complete.el ends here 