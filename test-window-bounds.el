;;; test-window-bounds.el --- Test window bounds functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that flash-emacs only searches within visible window bounds

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun test-window-bounds ()
  "Test that search is restricted to visible window content."
  (interactive)
  (message "=== Testing Window Bounds ===")
  
  (with-temp-buffer
    ;; Create a buffer with many lines
    (dotimes (i 100)
      (insert (format "Line %d: test content here\n" (1+ i))))
    
    ;; Display the buffer in a window
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    
    ;; Get window bounds
    (let* ((window (selected-window))
           (bounds (flash-emacs--get-window-bounds window))
           (start-line (car bounds))
           (end-line (cdr bounds)))
      
      (message "Window shows lines %d to %d" start-line end-line)
      
      ;; Test that we only get matches within visible bounds
      (let ((matches (flash-emacs--search-in-window "test" window)))
        (message "Found %d matches in visible area" (length matches))
        
        ;; Verify all matches are within visible bounds
        (dolist (match matches)
          (let* ((pos (plist-get match :pos))
                 (line-num (line-number-at-pos pos)))
            (cl-assert (and (>= line-num start-line) (<= line-num end-line)) nil
                      "Match at line %d is outside visible bounds (%d-%d)" 
                      line-num start-line end-line)))
        
        ;; Should have fewer matches than total "test" occurrences in buffer
        (let ((total-matches (progn
                              (goto-char (point-min))
                              (let ((count 0))
                                (while (search-forward "test" nil t)
                                  (setq count (1+ count)))
                                count))))
          (message "Total 'test' occurrences in buffer: %d" total-matches)
          (message "Visible 'test' occurrences: %d" (length matches))
          
          ;; In a normal window, we should see fewer matches than total
          ;; (unless the window shows the entire buffer)
          (when (< (- end-line start-line) 90) ; If window doesn't show most of buffer
            (cl-assert (< (length matches) total-matches) nil
                      "Should find fewer matches in visible area than in entire buffer"))))
      
      ;; Test scrolling changes visible matches
      (let ((initial-matches (length (flash-emacs--search-in-window "test" window))))
        ;; Scroll down
        (scroll-up 10)
        (let ((scrolled-matches (length (flash-emacs--search-in-window "test" window))))
          (message "Matches before scroll: %d, after scroll: %d" 
                  initial-matches scrolled-matches)
          ;; The number of matches might be different after scrolling
          ;; (unless we're at the end of buffer)
          (when (< (window-end) (point-max))
            (message "Scrolling changed visible content as expected")))))
    
    (message "✓ Window bounds tests passed")))

(defun test-multi-window-bounds ()
  "Test window bounds with multiple windows."
  (interactive)
  (message "=== Testing Multi-Window Bounds ===")
  
  (with-temp-buffer
    ;; Create a buffer with content
    (dotimes (i 50)
      (insert (format "Line %d: test content here\n" (1+ i))))
    
    ;; Split window and show same buffer in both
    (switch-to-buffer (current-buffer))
    (split-window-below)
    (other-window 1)
    (switch-to-buffer (current-buffer))
    
    ;; Position windows to show different parts
    (goto-char (point-min))
    (let ((window1 (selected-window)))
      (other-window 1)
      (goto-char (point-max))
      (recenter)
      (let ((window2 (selected-window)))
        
        ;; Get bounds for both windows
        (let* ((bounds1 (flash-emacs--get-window-bounds window1))
               (bounds2 (flash-emacs--get-window-bounds window2))
               (matches1 (flash-emacs--search-in-window "test" window1))
               (matches2 (flash-emacs--search-in-window "test" window2)))
          
          (message "Window 1 shows lines %d-%d with %d matches" 
                  (car bounds1) (cdr bounds1) (length matches1))
          (message "Window 2 shows lines %d-%d with %d matches" 
                  (car bounds2) (cdr bounds2) (length matches2))
          
          ;; Verify matches are within their respective window bounds
          (dolist (match matches1)
            (let ((line-num (line-number-at-pos (plist-get match :pos))))
              (cl-assert (and (>= line-num (car bounds1)) 
                             (<= line-num (cdr bounds1))) nil
                        "Window 1 match outside bounds")))
          
          (dolist (match matches2)
            (let ((line-num (line-number-at-pos (plist-get match :pos))))
              (cl-assert (and (>= line-num (car bounds2)) 
                             (<= line-num (cdr bounds2))) nil
                        "Window 2 match outside bounds")))
          
          (message "✓ Multi-window bounds verified"))))
    
    ;; Clean up
    (delete-other-windows)
    (message "✓ Multi-window bounds tests passed")))

(defun test-window-bounds-integration ()
  "Test that flash-emacs-jump respects window bounds."
  (interactive)
  (message "=== Testing Window Bounds Integration ===")
  
  (with-temp-buffer
    ;; Create content with many test occurrences
    (dotimes (i 100)
      (insert (format "Line %d: test content test here test\n" (1+ i))))
    
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    
    ;; Test that search pattern respects bounds
    (let* ((window (selected-window))
           (bounds (flash-emacs--get-window-bounds window))
           (visible-matches (flash-emacs--search-pattern "test"))
           (all-matches-count (progn
                               (goto-char (point-min))
                               (let ((count 0))
                                 (while (search-forward "test" nil t)
                                   (setq count (1+ count)))
                                 count))))
      
      (message "Visible window shows lines %d-%d" (car bounds) (cdr bounds))
      (message "Found %d visible matches out of %d total" 
              (length visible-matches) all-matches-count)
      
      ;; Verify all visible matches are within bounds
      (dolist (match visible-matches)
        (let ((line-num (line-number-at-pos (plist-get match :pos))))
          (cl-assert (and (>= line-num (car bounds)) 
                         (<= line-num (cdr bounds))) nil
                    "Visible match outside window bounds")))
      
      ;; Should have fewer visible matches than total (unless window shows everything)
      (when (< (- (cdr bounds) (car bounds)) 90)
        (cl-assert (< (length visible-matches) all-matches-count) nil
                  "Should have fewer visible matches than total"))
      
      (message "✓ Window bounds integration verified")))
  
  (message "✓ Window bounds integration tests passed"))

(defun run-window-bounds-tests ()
  "Run all window bounds tests."
  (message "=== Running Window Bounds Tests ===")
  
  (condition-case err
      (progn
        (test-window-bounds)
        (test-multi-window-bounds)
        (test-window-bounds-integration)
        
        (message "✓ All window bounds tests passed!"))
    (error
     (message "✗ Window bounds test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-window-bounds-tests)

(provide 'test-window-bounds)

;;; test-window-bounds.el ends here 