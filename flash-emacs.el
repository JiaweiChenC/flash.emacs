;;; flash-emacs.el --- Flash.nvim-like jump navigation for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Flash-Emacs Contributors
;; Maintainer: Flash-Emacs Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5"))
;; Keywords: navigation, jump, search, convenience
;; URL: https://github.com/flash-emacs/flash-emacs
;; Homepage: https://github.com/flash-emacs/flash-emacs

;;; Commentary:

;; Flash-emacs provides flash.nvim-like jump navigation for Emacs.
;; It allows you to quickly jump to any visible location by typing
;; a search pattern and then using single-character labels.
;;
;; Key features:
;; - Multi-character search patterns with progressive narrowing
;; - Smart label assignment prioritizing current window and distance
;; - Intelligent conflict avoidance between search and jump labels
;; - Multi-window support for searching across visible windows
;; - Visual feedback with highlighted matches and overlay labels
;; - Incremental search with real-time updates
;; - Evil-mode visual selection support
;; - Customizable appearance and behavior
;;
;; Usage:
;;   M-x flash-emacs-jump  ; Start flash jump
;;
;; To bind to a key (recommended):
;;   (global-set-key (kbd "C-c j") #'flash-emacs-jump)
;;
;; Evil-mode integration:
;;   (with-eval-after-load 'evil
;;     (define-key evil-normal-state-map (kbd "s") #'flash-emacs-jump)
;;     (define-key evil-visual-state-map (kbd "s") #'flash-emacs-jump)
;;     (define-key evil-operator-state-map (kbd "s") #'flash-emacs-jump))
;;
;; For more information, see the README.md file or visit:
;; https://github.com/flash-emacs/flash-emacs

;;; Code:

(require 'cl-lib)

;; Performance optimization variables
(defvar flash-emacs--popup-buffer-cache nil
  "Cache of popup buffers to avoid repeated detection.")

(defvar flash-emacs--popup-buffer-cache-time nil
  "Time when popup buffer cache was last updated.")

(defvar flash-emacs--cache-timeout 1.0
  "Timeout in seconds for popup buffer cache.")

(defvar flash-emacs--distance-cache nil
  "Cache for distance calculations to avoid recomputation.")

(defvar flash-emacs--last-search-pattern nil
  "Last search pattern to detect when to clear caches.")

(defvar flash-emacs--last-cursor-position nil
  "Last cursor position to detect when to clear distance cache.")

;;; Customization

(defgroup flash-emacs nil
  "Flash.nvim-like jump navigation for Emacs."
  :group 'navigation
  :prefix "flash-emacs-")

(defcustom flash-emacs-labels "asdfghjklqwertyuiopzxcvbnm"
  "Characters to use as jump labels."
  :type 'string
  :group 'flash-emacs)

(defcustom flash-emacs-uppercase-labels t
  "Whether to use uppercase labels after lowercase ones are exhausted.
When t, after using all lowercase labels, uppercase versions will be used."
  :type 'boolean
  :group 'flash-emacs)

(defcustom flash-emacs-search-mode 'exact
  "Search mode for pattern matching.
- exact: exact string match
- regex: regular expression match
- fuzzy: fuzzy matching (not implemented yet)"
  :type '(choice (const exact)
                 (const regex)
                 (const fuzzy))
  :group 'flash-emacs)

(defcustom flash-emacs-multi-window t
  "Whether to search in all visible windows."
  :type 'boolean
  :group 'flash-emacs)

(defcustom flash-emacs-case-sensitive nil
  "Whether search should be case sensitive."
  :type 'boolean
  :group 'flash-emacs)

(defcustom flash-emacs-min-pattern-length 1
  "Minimum pattern length before showing labels."
  :type 'integer
  :group 'flash-emacs)

(defcustom flash-emacs-max-matches 500
  "Maximum number of matches to process."
  :type 'integer
  :group 'flash-emacs)

(defcustom flash-emacs-evil-visual-extend t
  "Whether to extend evil visual selection when jumping.
When t, jumping in visual mode will extend the selection to the target.
When nil, jumping will move the cursor and exit visual mode."
  :type 'boolean
  :group 'flash-emacs)

;;; Faces

(defface flash-emacs-label
  '((t (:background "red" :foreground "white" :weight bold)))
  "Face for jump labels."
  :group 'flash-emacs)

(defface flash-emacs-match
  '((t (:background "yellow" :foreground "black")))
  "Face for search matches."
  :group 'flash-emacs)

(defface flash-emacs-current
  '((t (:background "green" :foreground "white" :weight bold)))
  "Face for the current/target match."
  :group 'flash-emacs)

;;; Internal variables

(defvar flash-emacs--state nil
  "Current flash state.")

(defvar flash-emacs--overlays nil
  "List of active overlays.")

;;; Evil-mode integration

(defun flash-emacs--evil-available-p ()
  "Check if evil-mode is available and loaded."
  (and (featurep 'evil) (bound-and-true-p evil-mode)))

(defun flash-emacs--evil-visual-state-p ()
  "Check if we're currently in evil visual state."
  (and (flash-emacs--evil-available-p)
       (eq evil-state 'visual)))

(defun flash-emacs--evil-operator-state-p ()
  "Check if we're currently in evil operator state."
  (and (flash-emacs--evil-available-p)
       (eq evil-state 'operator)))

(defun flash-emacs--evil-visual-type ()
  "Get the current evil visual selection type."
  (when (flash-emacs--evil-visual-state-p)
    evil-visual-selection))

(defun flash-emacs--evil-visual-range ()
  "Get the current evil visual selection range as (start . end)."
  (when (flash-emacs--evil-visual-state-p)
    (cons evil-visual-beginning evil-visual-end)))

(defun flash-emacs--evil-extend-selection (target-pos)
  "Extend evil visual selection to TARGET-POS."
  (when (and (flash-emacs--evil-visual-state-p) flash-emacs-evil-visual-extend)
    ;; Simply move cursor to target position
    ;; Evil-mode will automatically extend the selection appropriately
    (goto-char target-pos)))

;;; State management

(defun flash-emacs--create-state ()
  "Create a new flash state."
  (list :pattern ""
        :matches '()
        :labels flash-emacs-labels
        :current-window (selected-window)
        :start-point (point)
        :label-index 0
        :evil-visual-state (flash-emacs--evil-visual-state-p)
        :evil-visual-range (flash-emacs--evil-visual-range)
        :evil-visual-type (flash-emacs--evil-visual-type)))

(defun flash-emacs--get-pattern (state)
  "Get the current search pattern from STATE."
  (plist-get state :pattern))

(defun flash-emacs--set-pattern (state pattern)
  "Set the search pattern in STATE to PATTERN."
  (plist-put state :pattern pattern))

(defun flash-emacs--get-matches (state)
  "Get the current matches from STATE."
  (plist-get state :matches))

(defun flash-emacs--set-matches (state matches)
  "Set the matches in STATE to MATCHES."
  (plist-put state :matches matches))

(defun flash-emacs--get-all-labels (state)
  "Get all available labels including uppercase if enabled."
  (let ((base-labels (plist-get state :labels)))
    (if flash-emacs-uppercase-labels
        (concat base-labels (upcase base-labels))
      base-labels)))

;;; Search functions

(defun flash-emacs--get-window-bounds (window)
  "Get the visible line bounds for WINDOW.
Returns (start-line . end-line) where lines are 1-indexed."
  (let ((win-start (window-start window))
        (win-end (window-end window)))
    (with-current-buffer (window-buffer window)
      ;; Ensure positions are within buffer bounds
      (let ((max-pos (point-max))
            (min-pos (point-min)))
        (setq win-start (max min-pos (min win-start max-pos)))
        (setq win-end (max min-pos (min win-end max-pos)))
        (cons (line-number-at-pos win-start)
              (line-number-at-pos win-end))))))

(defun flash-emacs--search-in-window (pattern window &optional max-matches)
  "Search for PATTERN in WINDOW and return list of matches.
Only searches within the visible area of the window.
MAX-MATCHES limits the number of matches for performance."
  (let ((matches '())
        (case-fold-search (not flash-emacs-case-sensitive))
        (limit (or max-matches flash-emacs-max-matches)))
    (with-current-buffer (window-buffer window)
      (save-excursion
        ;; Get visible bounds for this window
        (let* ((bounds (flash-emacs--get-window-bounds window))
               (start-line (car bounds))
               (end-line (cdr bounds))
               (start-pos (progn (goto-line start-line) (line-beginning-position)))
               (end-pos (progn (goto-line end-line) (line-end-position)))
               (search-func (if (eq flash-emacs-search-mode 'regex)
                               #'re-search-forward
                             #'search-forward)))
          ;; Ensure positions are within buffer bounds
          (setq start-pos (max (point-min) (min start-pos (point-max))))
          (setq end-pos (max (point-min) (min end-pos (point-max))))
          
          ;; Search only within visible bounds
          (goto-char start-pos)
          (while (and (funcall search-func pattern end-pos t)
                     (< (length matches) limit))
            (let ((match-start (match-beginning 0))
                  (match-end (match-end 0)))
              (push (list :pos match-start
                         :end-pos match-end
                         :window window
                         :buffer (window-buffer window)
                         :text (buffer-substring-no-properties match-start match-end))
                    matches))))))
    (nreverse matches)))

(defun flash-emacs--is-doom-popup-buffer-p ()
  "Check if current buffer is a Doom Emacs popup buffer."
  (and (boundp '+popup-buffer-mode)
       +popup-buffer-mode))

(defun flash-emacs--is-popup-buffer-p ()
  "Check if current buffer is any kind of popup buffer."
  (or (flash-emacs--is-doom-popup-buffer-p)
      ;; Check for other popup indicators
      (and (boundp 'popper-popup-status) popper-popup-status)
      ;; Check if buffer is not displayed in selected window
      (not (eq (current-buffer) (window-buffer (selected-window))))))

(defun flash-emacs--search-in-buffer (pattern buffer window &optional max-matches)
  "Search for PATTERN in BUFFER and return list of matches.
WINDOW is the window associated with this buffer (may be nil for popup buffers).
MAX-MATCHES limits the number of matches to find for early termination."
  (let ((matches '())
        (case-fold-search (not flash-emacs-case-sensitive))
        (limit (or max-matches flash-emacs-max-matches)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((search-func (if (eq flash-emacs-search-mode 'regex)
                              #'re-search-forward
                            #'search-forward)))
          (while (and (funcall search-func pattern nil t)
                     (< (length matches) limit))
            (let ((match-start (match-beginning 0))
                  (match-end (match-end 0)))
              (push (list :pos match-start
                         :end-pos match-end
                         :window (or window (selected-window))
                         :buffer buffer
                         :text (buffer-substring-no-properties match-start match-end))
                    matches))))))
    (nreverse matches)))

(defun flash-emacs--get-popup-buffers ()
  "Get list of popup buffers that should be searched.
Uses caching to avoid repeated buffer scanning."
  (let ((current-time (float-time)))
    ;; Check if cache is valid
    (when (or (not flash-emacs--popup-buffer-cache-time)
              (> (- current-time flash-emacs--popup-buffer-cache-time) 
                 flash-emacs--cache-timeout))
      ;; Rebuild cache
      (setq flash-emacs--popup-buffer-cache '())
      (dolist (buffer (buffer-list))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (flash-emacs--is-popup-buffer-p)
              (push buffer flash-emacs--popup-buffer-cache)))))
      (setq flash-emacs--popup-buffer-cache-time current-time))
    
    ;; Filter out dead buffers from cache
    (setq flash-emacs--popup-buffer-cache
          (cl-remove-if-not #'buffer-live-p flash-emacs--popup-buffer-cache))
    
    flash-emacs--popup-buffer-cache))

(defun flash-emacs--search-pattern (pattern)
  "Find all matches for PATTERN in visible windows AND popup buffers.
Optimized with early termination and smart match limiting."
  (when (>= (length pattern) flash-emacs-min-pattern-length)
    (let ((matches '())
          (windows (if flash-emacs-multi-window
                      (window-list)
                    (list (selected-window))))
          (searched-buffers '())
          (current-buffer (current-buffer))
          (max-useful-matches 150)) ; Reasonable limit for performance
      
      ;; Handle batch mode specially
      (if noninteractive
          (let ((case-fold-search (not flash-emacs-case-sensitive)))
            (save-excursion
              (goto-char (point-min))
              (let ((search-func (if (eq flash-emacs-search-mode 'regex)
                                    #'re-search-forward
                                  #'search-forward)))
                (while (and (funcall search-func pattern nil t)
                           (< (length matches) max-useful-matches))
                  (let ((match-start (match-beginning 0))
                        (match-end (match-end 0)))
                    (push (list :pos match-start
                               :end-pos match-end
                               :window (selected-window)
                               :buffer (current-buffer)
                               :text (buffer-substring-no-properties match-start match-end))
                          matches))))))
        
        ;; Interactive mode: search both windows AND popup buffers with smart prioritization
        (progn
          ;; First, search current buffer more thoroughly
          (let ((current-window (selected-window)))
            (when (window-live-p current-window)
              (push current-buffer searched-buffers)
              (setq matches (append matches 
                                   (flash-emacs--search-in-window pattern current-window)))))
          
          ;; Then search other visible windows with limited matches per window
          (let ((matches-per-window (max 10 (/ max-useful-matches (length windows)))))
            (dolist (window windows)
              (when (and (window-live-p window)
                        (not (eq window (selected-window)))
                        (< (length matches) max-useful-matches))
                (let ((buffer (window-buffer window)))
                  (unless (member buffer searched-buffers)
                    (push buffer searched-buffers)
                    (setq matches (append matches 
                                         (flash-emacs--search-in-window 
                                          pattern window matches-per-window))))))))
          
          ;; Finally, search popup buffers with even more limited matches
          (when (< (length matches) max-useful-matches)
            (let ((popup-matches-limit (max 5 (/ (- max-useful-matches (length matches)) 
                                               (max 1 (length (flash-emacs--get-popup-buffers)))))))
              (dolist (popup-buffer (flash-emacs--get-popup-buffers))
                (when (and (< (length matches) max-useful-matches)
                          (not (member popup-buffer searched-buffers)))
                  (setq matches (append matches 
                                       (flash-emacs--search-in-buffer 
                                        pattern popup-buffer nil popup-matches-limit)))))))))
      
      (nreverse matches))))

;;; Label assignment

(defun flash-emacs--get-window-width (window)
  "Get the effective width for distance calculations in WINDOW."
  (condition-case err
      (cond
       ;; Normal case: window displays current buffer
       ((eq (current-buffer) (window-buffer window))
        (with-selected-window window
          (window-total-width)))
       
       ;; Popup buffer case: try to get window width safely
       ((flash-emacs--is-popup-buffer-p)
        (or (ignore-errors (window-total-width window))
            80))  ; fallback width
       
       ;; Fallback
       (t 80))
    (error 80)))  ; safe fallback

(defun flash-emacs--distance-from-cursor (match current-window current-point)
  "Calculate 2D distance of MATCH from cursor in CURRENT-WINDOW at CURRENT-POINT.
Uses flash.nvim's algorithm with caching for performance."
  (let* ((match-window (plist-get match :window))
         (match-buffer (plist-get match :buffer))
         (match-pos (plist-get match :pos))
         (current-buffer (current-buffer))
         (cache-key (format "%s-%d-%s-%d" 
                           (buffer-name match-buffer) match-pos
                           (buffer-name current-buffer) current-point)))
    
    ;; Check cache first
    (or (and flash-emacs--distance-cache
             (gethash cache-key flash-emacs--distance-cache))
        
        ;; Calculate and cache result
        (let ((distance
               (cond
                ;; Same buffer and window - calculate actual distance
                ((and (eq match-window current-window)
                      (eq match-buffer current-buffer))
                 (let* (;; Get line and column for cursor position
                        (cursor-line (line-number-at-pos current-point))
                        (cursor-col (save-excursion 
                                     (goto-char current-point)
                                     (current-column)))
                        ;; Get line and column for match position  
                        (match-line (line-number-at-pos match-pos))
                        (match-col (save-excursion
                                    (goto-char match-pos)
                                    (current-column)))
                        ;; Convert to 1D coordinates (like flash.nvim)
                        ;; Use safe window width calculation
                        (columns-per-line (flash-emacs--get-window-width current-window))
                        (cursor-1d (+ (* cursor-line columns-per-line) cursor-col))
                        (match-1d (+ (* match-line columns-per-line) match-col)))
                   (abs (- cursor-1d match-1d))))
                
                ;; Same buffer but different window - small penalty
                ((eq match-buffer current-buffer)
                 5000)
                
                ;; Different buffer but same window - medium penalty  
                ((eq match-window current-window)
                 10000)
                
                ;; Different buffer and window - large penalty
                (t 20000))))
          
          ;; Initialize cache if needed
          (unless flash-emacs--distance-cache
            (setq flash-emacs--distance-cache (make-hash-table :test 'equal)))
          
          ;; Cache the result
          (puthash cache-key distance flash-emacs--distance-cache)
          distance))))

(defun flash-emacs--sort-matches (matches current-window current-point)
  "Sort MATCHES by priority: current buffer+window first, then by distance, then by position.
Enhanced to handle matches from multiple buffers."
  (let ((current-buffer (current-buffer)))
    (sort matches
          (lambda (a b)
            (let ((a-window (plist-get a :window))
                  (b-window (plist-get b :window))
                  (a-buffer (plist-get a :buffer))
                  (b-buffer (plist-get b :buffer)))
              (cond
               ;; Priority 1: Current buffer + current window wins over everything
               ((and (eq a-buffer current-buffer) (eq a-window current-window)
                     (not (and (eq b-buffer current-buffer) (eq b-window current-window))))
                t)
               ((and (eq b-buffer current-buffer) (eq b-window current-window)
                     (not (and (eq a-buffer current-buffer) (eq a-window current-window))))
                nil)
               
               ;; Priority 2: Current buffer (any window) wins over other buffers
               ((and (eq a-buffer current-buffer) (not (eq b-buffer current-buffer)))
                t)
               ((and (eq b-buffer current-buffer) (not (eq a-buffer current-buffer)))
                nil)
               
               ;; Priority 3: Current window (any buffer) wins over other windows
               ((and (eq a-window current-window) (not (eq b-window current-window)))
                t)
               ((and (eq b-window current-window) (not (eq a-window current-window)))
                nil)
               
               ;; Priority 4: Within same context, sort by distance
               (t
                (let ((dist-a (flash-emacs--distance-from-cursor a current-window current-point))
                      (dist-b (flash-emacs--distance-from-cursor b current-window current-point)))
                  (cond
                   ;; Different distances - closer wins
                   ((not (= dist-a dist-b))
                    (< dist-a dist-b))
                   ;; Same distance - fall back to position within buffer
                   (t
                    (if (eq a-buffer b-buffer)
                        ;; Same buffer - sort by position
                        (let ((a-line (with-current-buffer a-buffer (line-number-at-pos (plist-get a :pos))))
                              (b-line (with-current-buffer b-buffer (line-number-at-pos (plist-get b :pos)))))
                          (cond
                           ;; Different lines - earlier line wins
                           ((not (= a-line b-line))
                            (< a-line b-line))
                           ;; Same line - earlier column wins
                           (t
                            (< (plist-get a :pos) (plist-get b :pos)))))
                      ;; Different buffers - sort by buffer name for consistency
                      (string< (buffer-name a-buffer) (buffer-name b-buffer)))))))))))))

(defun flash-emacs--create-skip-pattern (search-pattern)
  "Create a skip pattern to avoid label conflicts with search continuation.
This pattern matches the search pattern followed by any character."
  (when (and search-pattern (> (length search-pattern) 0))
    (concat (regexp-quote search-pattern) ".")))

(defun flash-emacs--find-conflicting-labels (search-pattern labels window)
  "Find labels that would conflict with continuing the search pattern.
Returns a list of labels to exclude."
  (let ((skip-pattern (flash-emacs--create-skip-pattern search-pattern))
        (conflicting-labels '()))
    (when skip-pattern
      (with-current-buffer (if (or noninteractive 
                                  (not (eq (current-buffer) (window-buffer window))))
                              (current-buffer)
                            (window-buffer window))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward skip-pattern nil t)
            (let* ((match-end (match-end 0))
                   (following-char (buffer-substring-no-properties 
                                   (1- match-end) match-end)))
              ;; Check if this following character is in our labels
              (when (and following-char 
                        (cl-find following-char labels :test #'string=))
                (push following-char conflicting-labels)))))))
    (delete-dups conflicting-labels)))

(defun flash-emacs--filter-labels-for-pattern (labels search-pattern windows)
  "Filter out labels that would conflict with search pattern continuation."
  (if (or (not search-pattern) (= (length search-pattern) 0))
      labels
    (let ((conflicting-labels '()))
      ;; Collect conflicting labels from all windows
      (dolist (window windows)
        (when (window-live-p window)
          (setq conflicting-labels 
                (append conflicting-labels 
                        (flash-emacs--find-conflicting-labels search-pattern 
                                                             (mapcar #'char-to-string 
                                                                    (string-to-list labels)) 
                                                             window)))))
      ;; Remove conflicting labels and their case variants
      (let ((label-chars (string-to-list labels)))
        (mapconcat #'char-to-string
                   (cl-remove-if (lambda (label-char)
                                   (let ((label-str (char-to-string label-char)))
                                     (or (cl-find label-str conflicting-labels :test #'string=)
                                         ;; Also remove the opposite case
                                         (cl-find (if (= label-char (upcase label-char))
                                                     (downcase label-str)
                                                   (upcase label-str))
                                                 conflicting-labels :test #'string=))))
                                 label-chars)
                   "")))))

(defun flash-emacs--match-priority (match current-window current-point current-buffer)
  "Calculate priority score for MATCH. Lower scores = higher priority."
  (let ((match-window (plist-get match :window))
        (match-buffer (plist-get match :buffer)))
    (cond
     ;; Priority 1: Current buffer + current window (score 0-999)
     ((and (eq match-buffer current-buffer) (eq match-window current-window))
      (flash-emacs--distance-from-cursor match current-window current-point))
     
     ;; Priority 2: Current buffer, different window (score 1000-1999)
     ((eq match-buffer current-buffer)
      (+ 1000 (flash-emacs--distance-from-cursor match current-window current-point)))
     
     ;; Priority 3: Different buffer, current window (score 2000-2999)
     ((eq match-window current-window)
      (+ 2000 (flash-emacs--distance-from-cursor match current-window current-point)))
     
     ;; Priority 4: Different buffer and window (score 3000+)
     (t
      (+ 3000 (flash-emacs--distance-from-cursor match current-window current-point))))))

(defun flash-emacs--partial-sort-matches (matches current-window current-point n)
  "Partially sort MATCHES to get the top N by priority.
More efficient than full sorting when N << length(matches)."
  (let ((current-buffer (current-buffer)))
    ;; Add priority scores to matches
    (dolist (match matches)
      (plist-put match :priority 
                 (flash-emacs--match-priority match current-window current-point current-buffer)))
    
    ;; Use partial sort to get top N matches
    (let ((sorted-matches (sort matches (lambda (a b)
                                         (< (plist-get a :priority)
                                            (plist-get b :priority))))))
      ;; Remove priority scores to clean up
      (dolist (match sorted-matches)
        (plist-put match :priority nil))
      
      ;; Return only the top N
      (cl-subseq sorted-matches 0 (min n (length sorted-matches))))))

(defun flash-emacs--assign-labels (matches labels current-window current-point)
  "Assign single-character labels to MATCHES.
Only assigns labels to the closest matches that can receive labels.
Optimized to only sort the number of matches we need."
  (let* ((max-labels (length labels))
         (matches-needed (min max-labels (length matches))))
    
    ;; If we have fewer matches than labels, no need to sort
    (if (<= (length matches) max-labels)
        (let ((labeled-matches '())
              (label-index 0))
          (dolist (match matches)
            (plist-put match :label (substring labels label-index (1+ label-index)))
            (setq label-index (1+ label-index))
            (push match labeled-matches))
          (nreverse labeled-matches))
      
      ;; Otherwise, use partial sorting to get only the top N matches
      (let* ((sorted-matches (flash-emacs--partial-sort-matches 
                             matches current-window current-point matches-needed))
             (labeled-matches '())
             (label-index 0))
        
        ;; Assign labels to the top matches
        (dolist (match sorted-matches)
          (when (< label-index max-labels)
            (plist-put match :label (substring labels label-index (1+ label-index)))
            (setq label-index (1+ label-index))
            (push match labeled-matches)))
        
        (nreverse labeled-matches)))))

;;; Visual feedback

(defun flash-emacs--create-label-overlay (match)
  "Create an overlay for the label of MATCH."
  (let* ((pos (plist-get match :pos))
         (window (plist-get match :window))
         (buffer (plist-get match :buffer))
         (label (plist-get match :label)))
    (when label
      (condition-case err
          ;; Create overlay in the correct buffer
          (let ((target-buffer (or buffer (window-buffer window) (current-buffer))))
            (with-current-buffer target-buffer
              (let ((overlay (make-overlay pos (1+ pos))))
                (overlay-put overlay 'display 
                            (propertize label 'face 'flash-emacs-label))
                (overlay-put overlay 'flash-emacs 'label)
                (overlay-put overlay 'window window)
                (overlay-put overlay 'buffer target-buffer)
                overlay)))
        (error
         ;; If overlay creation fails, log the error but don't crash
         (message "Flash-emacs: Failed to create label overlay: %s" (error-message-string err))
         nil)))))

(defun flash-emacs--create-match-overlay (match)
  "Create an overlay for highlighting MATCH."
  (let* ((pos (plist-get match :pos))
         (end-pos (plist-get match :end-pos))
         (window (plist-get match :window))
         (buffer (plist-get match :buffer)))
    (condition-case err
        ;; Create overlay in the correct buffer
        (let ((target-buffer (or buffer (window-buffer window) (current-buffer))))
          (with-current-buffer target-buffer
            (let ((overlay (make-overlay pos end-pos)))
              (overlay-put overlay 'face 'flash-emacs-match)
              (overlay-put overlay 'flash-emacs 'match)
              (overlay-put overlay 'window window)
              (overlay-put overlay 'buffer target-buffer)
              overlay)))
      (error
       ;; If overlay creation fails, log the error but don't crash
       (message "Flash-emacs: Failed to create match overlay: %s" (error-message-string err))
       nil))))

(defun flash-emacs--show-overlays (all-matches labeled-matches)
  "Display overlays for ALL-MATCHES (background) and LABELED-MATCHES (labels).
ALL-MATCHES get background highlighting, LABELED-MATCHES get both background and labels."
  (flash-emacs--clear-overlays)
  ;; Show background highlighting for ALL matches
  (dolist (match all-matches)
    (when-let* ((match-overlay (flash-emacs--create-match-overlay match)))
      (push match-overlay flash-emacs--overlays)))
  ;; Show labels only for labeled matches
  (dolist (match labeled-matches)
    (when-let* ((label-overlay (flash-emacs--create-label-overlay match)))
      (push label-overlay flash-emacs--overlays))))

(defun flash-emacs--clear-overlays ()
  "Remove all flash overlays."
  (dolist (overlay flash-emacs--overlays)
    (delete-overlay overlay))
  (setq flash-emacs--overlays nil))

;;; Input handling

(defun flash-emacs--find-match-by-label (label matches)
  "Find the match with the given LABEL in MATCHES."
  (cl-find-if (lambda (match)
                (string= (plist-get match :label) label))
              matches))

(defun flash-emacs--check-jump-label (new-pattern current-pattern matches)
  "Check if NEW-PATTERN represents a jump label after CURRENT-PATTERN.
Returns the label character if it's a jump, nil otherwise."
  (when (and (> (length new-pattern) (length current-pattern))
             (string-prefix-p current-pattern new-pattern)
             (= (length new-pattern) (1+ (length current-pattern))))
    (let ((label-char (substring new-pattern (length current-pattern))))
      (when (cl-some (lambda (match)
                       (string= (plist-get match :label) label-char))
                     matches)
        label-char))))

(defun flash-emacs--jump-to-match (match state)
  "Jump to the position of MATCH, handling evil visual mode appropriately."
  (let ((window (plist-get match :window))
        (pos (plist-get match :pos))
        (was-visual (plist-get state :evil-visual-state)))
    
    ;; Switch to target window if needed
    (unless (eq window (selected-window))
      (select-window window))
    
    ;; Handle evil visual mode
    (cond
     ;; If we were in evil visual mode and should extend selection
     ((and was-visual (flash-emacs--evil-available-p) flash-emacs-evil-visual-extend)
      (flash-emacs--evil-extend-selection pos))
     
     ;; If we were in evil visual mode but should not extend, exit visual mode
     ((and was-visual (flash-emacs--evil-available-p) (not flash-emacs-evil-visual-extend))
      (evil-exit-visual-state)
      (goto-char pos))
     
     ;; Normal jump - always position cursor at match
     (t
      (goto-char pos)))
    
    ;; Add to mark ring for non-evil or when not extending selection
    (unless (and was-visual flash-emacs-evil-visual-extend)
      (push-mark))))

(defun flash-emacs--clear-caches ()
  "Clear all performance caches."
  (setq flash-emacs--distance-cache nil
        flash-emacs--popup-buffer-cache nil
        flash-emacs--popup-buffer-cache-time nil
        flash-emacs--last-search-pattern nil
        flash-emacs--last-cursor-position nil))

(defun flash-emacs--maybe-clear-caches (pattern current-point)
  "Clear caches if pattern or cursor position changed significantly."
  (when (or (not (equal pattern flash-emacs--last-search-pattern))
            (not (equal current-point flash-emacs--last-cursor-position)))
    ;; Clear distance cache when cursor moves or pattern changes
    (setq flash-emacs--distance-cache nil)
    (setq flash-emacs--last-search-pattern pattern)
    (setq flash-emacs--last-cursor-position current-point)))

(defun flash-emacs--update-search (state)
  "Update search results for the current pattern in STATE."
  (let* ((pattern (flash-emacs--get-pattern state))
         (current-window (plist-get state :current-window))
         (current-point (plist-get state :start-point))
         (all-labels (flash-emacs--get-all-labels state))
         (windows (if flash-emacs-multi-window
                     (window-list)
                   (list (selected-window)))))
    
    ;; Clear caches if needed for consistency
    (flash-emacs--maybe-clear-caches pattern current-point)
    
    (let ((all-matches (flash-emacs--search-pattern pattern)))
      (when all-matches
        ;; Filter labels to avoid conflicts with search pattern
        (let* ((filtered-label-chars (flash-emacs--filter-labels-for-pattern 
                                     all-labels pattern windows))
               (filtered-labels (mapconcat #'char-to-string filtered-label-chars ""))
               (labeled-matches (flash-emacs--assign-labels all-matches filtered-labels 
                                                           current-window current-point)))
          (flash-emacs--set-matches state labeled-matches)
          (flash-emacs--show-overlays all-matches labeled-matches)))
      all-matches)))

(defun flash-emacs--handle-input (state)
  "Handle one character of user input for STATE.
Returns nil to exit, t to continue."
  (let* ((current-pattern (flash-emacs--get-pattern state))
         (prompt (if (> (length current-pattern) 0)
                    (concat ":" current-pattern)
                  ":"))
         (char (read-char-exclusive prompt))
         (matches (flash-emacs--get-matches state)))
    
    (cond
     ;; ESC or C-g - exit
     ((or (= char 27) (= char 7))
      (message "Flash cancelled")
      nil)
     
     ;; Enter - jump to first match
     ((= char 13)
      (when (car matches)
        (flash-emacs--jump-to-match (car matches) state))
      nil)
     
     ;; Backspace - remove last character
     ((or (= char 127) (= char 8))
      (if (> (length current-pattern) 0)
          (progn
            (flash-emacs--set-pattern state (substring current-pattern 0 -1))
            (flash-emacs--update-search state)
            t)
        nil))
     
     ;; Regular character - check if it's a jump label or add to pattern
     ((and (>= char 32) (<= char 126))  ; Printable ASCII
      (let* ((new-char (char-to-string char))
             (new-pattern (concat current-pattern new-char))
             (potential-label (flash-emacs--check-jump-label new-pattern current-pattern matches)))
        (if potential-label
            ;; It's a jump label - perform the jump
            (progn
              (let ((target-match (flash-emacs--find-match-by-label potential-label matches)))
                (when target-match
                  (flash-emacs--jump-to-match target-match state)))
              nil)
          ;; It's a regular character - add to search pattern
          (progn
            (flash-emacs--set-pattern state new-pattern)
            (flash-emacs--update-search state)
            t))))
     
     ;; Unknown character - ignore
     (t t))))

;;; Main functions

(defun flash-emacs--main-loop (state)
  "Main interaction loop for STATE."
  (unwind-protect
      (while (flash-emacs--handle-input state))
    (flash-emacs--clear-overlays)
    (flash-emacs--clear-caches)))

;;;###autoload
(defun flash-emacs-jump ()
  "Start flash jump mode.
Type characters to search, then use the displayed labels to jump.
In evil visual mode, jumping will extend the selection to the target."
  (interactive)
  (let ((state (flash-emacs--create-state)))
    (setq flash-emacs--state state)
    (flash-emacs--main-loop state)))

;;; Testing functions

(defun flash-emacs--test-search ()
  "Test the search functionality."
  (interactive)
  (let ((pattern (read-string "Test pattern: ")))
    (let ((matches (flash-emacs--search-pattern pattern)))
      (message "Found %d matches for '%s'" (length matches) pattern)
      (when matches
        (let ((current-window (selected-window))
              (current-point (point)))
          (setq matches (flash-emacs--assign-labels matches flash-emacs-labels
                                                   current-window current-point))
          (flash-emacs--show-overlays matches matches)
          (sit-for 3)
          (flash-emacs--clear-overlays))))))

(defun flash-emacs--test-labels ()
  "Test label assignment."
  (interactive)
  (let* ((test-matches (list
                       (list :pos 100 :end-pos 105 :window (selected-window) :text "test1")
                       (list :pos 200 :end-pos 205 :window (selected-window) :text "test2")
                       (list :pos 50 :end-pos 55 :window (selected-window) :text "test3")))
         (labeled-matches (flash-emacs--assign-labels test-matches 
                                                     flash-emacs-labels
                                                     (selected-window)
                                                     (point))))
    (dolist (match labeled-matches)
      (message "Match at %d: label '%s'" 
               (plist-get match :pos) 
               (plist-get match :label)))))

(defun flash-emacs--test-evil-integration ()
  "Test evil-mode integration."
  (interactive)
  (message "Evil available: %s" (flash-emacs--evil-available-p))
  (when (flash-emacs--evil-available-p)
    (message "Evil state: %s" evil-state)
    (message "Visual state: %s" (flash-emacs--evil-visual-state-p))
    (when (flash-emacs--evil-visual-state-p)
      (message "Visual type: %s" (flash-emacs--evil-visual-type))
      (message "Visual range: %s" (flash-emacs--evil-visual-range)))))

(provide 'flash-emacs)

;;; flash-emacs.el ends here 