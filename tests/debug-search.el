;;; debug-search.el --- Debug search functionality -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun debug-search-issue ()
  "Debug the search functionality issue."
  (with-temp-buffer
    (insert "test testing tester tests")
    (goto-char (point-min))
    
    (message "Buffer content: '%s'" (buffer-string))
    (message "Buffer length: %d" (length (buffer-string)))
    (message "Point: %d" (point))
    (message "Current buffer: %s" (current-buffer))
    (message "Selected window: %s" (selected-window))
    (message "Window buffer: %s" (window-buffer (selected-window)))
    (message "Window live: %s" (window-live-p (selected-window)))
    (message "Same buffer?: %s" (eq (current-buffer) (window-buffer (selected-window))))
    
    ;; Test the search function directly
    (let ((matches (flash-emacs--search-pattern "test")))
      (message "Found %d matches for 'test'" (length matches))
      (dolist (match matches)
        (message "Match: pos=%d end-pos=%d text='%s'" 
                (plist-get match :pos)
                (plist-get match :end-pos)
                (plist-get match :text))))
    
    ;; Test manual search
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "test" nil t)
        (setq count (1+ count))
        (message "Manual search found match %d at %d-%d" 
                count (match-beginning 0) (match-end 0)))
      (message "Manual search found %d total matches" count))
    
    ;; Test the search-in-window function directly with current buffer
    (let ((matches (flash-emacs--search-in-window "test" (selected-window))))
      (message "search-in-window found %d matches" (length matches)))
    
    ;; Test search-in-window with a window that points to current buffer
    (let ((temp-window (selected-window)))
      (message "Testing with temp window...")
      (message "Temp window buffer: %s" (window-buffer temp-window))
      
      ;; Force the window to show our buffer
      (set-window-buffer temp-window (current-buffer))
      (message "After set-window-buffer: %s" (window-buffer temp-window))
      
      (let ((matches (flash-emacs--search-in-window "test" temp-window)))
        (message "search-in-window with correct buffer found %d matches" (length matches))
        (dolist (match matches)
          (message "Corrected match: pos=%d end-pos=%d text='%s'" 
                  (plist-get match :pos)
                  (plist-get match :end-pos)
                  (plist-get match :text)))))
    
    ;; Test search-in-window without window switching
    (message "Testing search without window switching...")
    (goto-char (point-min))
    (let ((matches '())
          (case-fold-search (not flash-emacs-case-sensitive)))
      (while (search-forward "test" nil t)
        (let ((match-start (match-beginning 0))
              (match-end (match-end 0)))
          (push (list :pos match-start
                     :end-pos match-end
                     :window (selected-window)
                     :text (buffer-substring-no-properties match-start match-end))
                matches)))
      (setq matches (nreverse matches))
      (message "Direct search found %d matches" (length matches))
      (dolist (match matches)
        (message "Direct match: pos=%d end-pos=%d text='%s'" 
                (plist-get match :pos)
                (plist-get match :end-pos)
                (plist-get match :text))))))

(debug-search-issue)

(provide 'debug-search)

;;; debug-search.el ends here 