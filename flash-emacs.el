;;; flash-emacs.el --- Simple flash.nvim-like jump navigation for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Flash-Emacs Contributors
;; Maintainer: Flash-Emacs Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5"))
;; Keywords: navigation, jump, search, convenience
;; URL: https://github.com/flash-emacs/flash-emacs

;;; Commentary:

;; Flash-emacs provides flash.nvim-like jump navigation for Emacs.
;; This is a simplified version that only searches in visible windows.

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup flash-emacs nil
  "Flash.nvim-like jump navigation for Emacs."
  :group 'navigation
  :prefix "flash-emacs-")

(defcustom flash-emacs-labels "asdfghjklqwertyuiopzxcvbnmASFGHJKLQWERTYUIOPZXCVBNM"
  "Characters to use as jump labels.
Includes both lowercase and uppercase letters for more available labels."
  :type 'string
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

;;; Faces

(defface flash-emacs-label
  '((t (:background "red" :foreground "white" :weight bold)))
  "Face for jump labels."
  :group 'flash-emacs)

(defface flash-emacs-match
  '((t (:background "yellow" :foreground "black")))
  "Face for search matches."
  :group 'flash-emacs)

;;; Internal variables

(defvar flash-emacs--overlays nil
  "List of active overlays.")

;;; Search functions

(defun flash-emacs--search-in-window (pattern window)
  "Search for PATTERN in WINDOW and return list of matches."
  (let ((matches '())
        (case-fold-search (not flash-emacs-case-sensitive)))
    (with-current-buffer (window-buffer window)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward pattern nil t)
          (let ((match-start (match-beginning 0))
                (match-end (match-end 0)))
            (push (list :pos match-start
                       :end-pos match-end
                       :window window
                       :buffer (current-buffer)
                       :text (buffer-substring-no-properties match-start match-end))
                  matches)))))
    (nreverse matches)))

(defun flash-emacs--search-pattern (pattern)
  "Find all matches for PATTERN in currently visible windows only."
  (when (>= (length pattern) flash-emacs-min-pattern-length)
    (let ((matches '())
          (windows (if flash-emacs-multi-window
                      (window-list)
                    (list (selected-window)))))
      
      ;; Search only in visible windows
      (dolist (window windows)
        (when (window-live-p window)
          (setq matches (append matches 
                               (flash-emacs--search-in-window pattern window)))))
      
      matches)))

;;; Label assignment

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
      (with-current-buffer (window-buffer window)
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

(defun flash-emacs--distance-from-cursor (match current-point)
  "Calculate distance of MATCH from cursor at CURRENT-POINT."
  (let ((match-pos (plist-get match :pos)))
    (abs (- match-pos current-point))))

(defun flash-emacs--sort-matches (matches current-point current-window)
  "Sort MATCHES by window priority (current window first) then by distance from CURRENT-POINT."
  (sort matches
        (lambda (a b)
          (let ((a-window (plist-get a :window))
                (b-window (plist-get b :window))
                (a-distance (flash-emacs--distance-from-cursor a current-point))
                (b-distance (flash-emacs--distance-from-cursor b current-point)))
            ;; First priority: current window
            (cond
             ;; Both in current window - sort by distance
             ((and (eq a-window current-window) (eq b-window current-window))
              (< a-distance b-distance))
             ;; A in current window, B not - A wins
             ((eq a-window current-window)
              t)
             ;; B in current window, A not - B wins
             ((eq b-window current-window)
              nil)
             ;; Neither in current window - sort by distance
             (t
              (< a-distance b-distance)))))))

(defun flash-emacs--assign-labels (matches labels current-point pattern windows current-window)
  "Assign single-character labels to MATCHES, filtering out conflicting labels.
Prioritizes matches in CURRENT-WINDOW."
  (let* ((filtered-labels (flash-emacs--filter-labels-for-pattern labels pattern windows))
         (sorted-matches (flash-emacs--sort-matches matches current-point current-window))
         (max-labels (length filtered-labels))
         (labeled-matches '())
         (label-index 0))
    
    ;; Assign labels to closest matches (current window first)
    (dolist (match sorted-matches)
      (when (< label-index max-labels)
        (plist-put match :label (substring filtered-labels label-index (1+ label-index)))
        (setq label-index (1+ label-index))
        (push match labeled-matches)))
    
    (nreverse labeled-matches)))

;;; Visual feedback

(defun flash-emacs--create-label-overlay (match)
  "Create an overlay for the label of MATCH."
  (let* ((pos (plist-get match :pos))
         (buffer (plist-get match :buffer))
         (label (plist-get match :label)))
    (when label
      (with-current-buffer buffer
        (let ((overlay (make-overlay pos (1+ pos))))
          (overlay-put overlay 'display 
                      (propertize label 'face 'flash-emacs-label))
          (overlay-put overlay 'flash-emacs 'label)
          overlay)))))

(defun flash-emacs--create-match-overlay (match)
  "Create an overlay for highlighting MATCH."
  (let* ((pos (plist-get match :pos))
         (end-pos (plist-get match :end-pos))
         (buffer (plist-get match :buffer)))
    (with-current-buffer buffer
      (let ((overlay (make-overlay pos end-pos)))
        (overlay-put overlay 'face 'flash-emacs-match)
        (overlay-put overlay 'flash-emacs 'match)
        overlay))))

(defun flash-emacs--show-overlays (all-matches labeled-matches)
  "Display overlays for ALL-MATCHES (background) and LABELED-MATCHES (labels)."
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

(defun flash-emacs--jump-to-match (match)
  "Jump to the position of MATCH."
  (let ((window (plist-get match :window))
        (pos (plist-get match :pos)))
    ;; Switch to target window if needed
    (unless (eq window (selected-window))
      (select-window window))
    ;; Jump to position
    (goto-char pos)
    ;; Add to mark ring
    (push-mark)))

;;; Main function

;;;###autoload
(defun flash-emacs-jump ()
  "Start flash jump mode - simple version."
  (interactive)
  (let ((pattern "")
        (matches '())
        (labeled-matches '()))
    
    (unwind-protect
        (catch 'flash-exit
          (while t
            ;; Get input
            (let* ((prompt (if (> (length pattern) 0)
                              (concat ":" pattern)
                            ":"))
                   (char (read-char-exclusive prompt)))
              
              (cond
               ;; ESC or C-g - exit
               ((or (= char 27) (= char 7))
                (message "Flash cancelled")
                (throw 'flash-exit nil))
               
               ;; Enter - jump to first match
               ((= char 13)
                (when (car labeled-matches)
                  (flash-emacs--jump-to-match (car labeled-matches)))
                (throw 'flash-exit nil))
               
               ;; Backspace - remove last character
               ((or (= char 127) (= char 8))
                (if (> (length pattern) 0)
                    (setq pattern (substring pattern 0 -1))
                  (throw 'flash-exit nil)))
               
               ;; Check if it's a jump label
               ((and (>= char 32) (<= char 126))  ; Printable ASCII
                (let* ((new-char (char-to-string char))
                       (target-match (flash-emacs--find-match-by-label new-char labeled-matches)))
                  (if target-match
                      ;; It's a jump label - perform the jump
                      (progn
                        (flash-emacs--jump-to-match target-match)
                        (throw 'flash-exit nil))
                    ;; It's a regular character - add to search pattern
                    (setq pattern (concat pattern new-char)))))
               
               ;; Unknown character - ignore
               (t nil))
              
              ;; Update search results
              (setq matches (flash-emacs--search-pattern pattern))
              (let ((windows (if flash-emacs-multi-window
                               (window-list)
                             (list (selected-window))))
                    (current-window (selected-window)))
                (setq labeled-matches (flash-emacs--assign-labels matches flash-emacs-labels (point) pattern windows current-window)))
              (flash-emacs--show-overlays matches labeled-matches))))
      
      ;; Cleanup
      (flash-emacs--clear-overlays))))

(provide 'flash-emacs)

;;; flash-emacs.el ends here 