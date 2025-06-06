;;; flash-emacs-ts.el --- Flash.nvim-like treesitter navigation for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Flash-Emacs Contributors
;; Maintainer: Flash-Emacs Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5"))
;; Keywords: navigation, jump, search, convenience, treesitter
;; URL: https://github.com/flash-emacs/flash-emacs

;;; Commentary:

;; flash-emacs-ts provides flash.nvim-like treesitter navigation for Emacs.
;; - Granular treesitter support (6+ options like flash.nvim)
;; - Evil mode compatibility (handles inclusive selection)
;; - Simple overlap resolution (outer nodes move to end)

;;; Code:

(require 'cl-lib)
(require 'treesit)

;;; Customization

(defgroup flash-emacs-ts nil
  "Flash.nvim-like treesitter navigation for Emacs."
  :group 'navigation
  :prefix "flash-emacs-ts-")

(defcustom flash-emacs-ts-labels "abcdefghijklmnopqrstuvwxyz"
  "Characters to use as jump labels for treesitter mode."
  :type 'string
  :group 'flash-emacs-ts)

(defcustom flash-emacs-ts-autojump t
  "Whether to automatically jump when there's only one treesitter match."
  :type 'boolean
  :group 'flash-emacs-ts)

;;; Faces

(defface flash-emacs-ts-label
  '((t (:background "red" :foreground "white" :weight bold)))
  "Face for treesitter jump labels."
  :group 'flash-emacs-ts)

(defface flash-emacs-ts-match
  '((t ()))
  "Face for treesitter matches."
  :group 'flash-emacs-ts)

;;; Internal variables

(defvar flash-emacs-ts--overlays nil
  "List of active overlays.")

;;; Utility functions

(defun flash-emacs-ts--evil-mode-active-p ()
  "Check if Evil mode is active."
  (and (boundp 'evil-mode) evil-mode))

(defun flash-emacs-ts--treesitter-available-p ()
  "Check if treesitter support is available."
  (treesit-available-p))

;;; Core treesitter functionality

(defun flash-emacs-ts--get-nodes-at-point (&optional window pos)
  "Get all treesitter nodes at POINT in WINDOW (flash.nvim style)."
  (when (not (flash-emacs-ts--treesitter-available-p))
    (error "Treesitter support not available"))
  (let ((target-window (or window (selected-window)))
        (target-pos (or pos (point)))
        (matches '()))
    (with-current-buffer (window-buffer target-window)
      (when (flash-emacs-ts--treesitter-available-p)
        (condition-case err
            (let* ((parser (treesit-parser-create 'python))
                   (node (treesit-node-at target-pos parser))
                   (done-ranges (make-hash-table :test 'equal)))
              (while node
                (let* ((start-pos (treesit-node-start node))
                       (end-pos (treesit-node-end node))
                       (node-type (treesit-node-type node))
                       (range-key (format "%d-%d" start-pos end-pos)))
                  (when (and (<= start-pos target-pos)
                            (>= end-pos target-pos)
                            (> end-pos start-pos)
                            (not (gethash range-key done-ranges)))
                    (puthash range-key t done-ranges)
                    (let ((text (buffer-substring-no-properties start-pos end-pos)))
                      (push (list :pos start-pos
                                 :end-pos end-pos
                                 :text text
                                 :group (intern node-type)
                                 :window target-window
                                 :buffer (current-buffer)
                                 :depth (- end-pos start-pos))
                            matches)))
                  (setq node (treesit-node-parent node))))
              (sort matches (lambda (a b)
                             (< (plist-get a :depth) (plist-get b :depth)))))
          (error
           (message "Error getting treesitter nodes: %s" (error-message-string err))))))
    matches))

;;; Overlay management

(defun flash-emacs-ts--create-match-overlay (match)
  "Create an overlay for highlighting MATCH."
  (let* ((pos (plist-get match :pos))
         (end-pos (plist-get match :end-pos))
         (buffer (plist-get match :buffer)))
    (with-current-buffer buffer
      (let ((overlay (make-overlay pos end-pos)))
        (overlay-put overlay 'face 'flash-emacs-ts-match)
        (overlay-put overlay 'flash-emacs-ts 'match)
        overlay))))

(defun flash-emacs-ts--show-overlays (all-matches labeled-matches)
  "Display overlays for matches precisely at element endpoints."
  (flash-emacs-ts--clear-overlays)

  ;; Create match highlighting overlays
  (dolist (match all-matches)
    (when-let* ((match-overlay (flash-emacs-ts--create-match-overlay match)))
      (push match-overlay flash-emacs-ts--overlays)))

  (let ((overlay-data '()))
    ;; Gather overlay positions for labels
    (dolist (match labeled-matches)
      (let ((pos (plist-get match :pos))
            (end-pos (plist-get match :end-pos))
            (buffer (plist-get match :buffer))
            (label (plist-get match :label)))
        (when label
          ;; Before-label overlay
          (push (list pos buffer label 'before) overlay-data)
          ;; Directly at endpoint (after-string)
          (push (list end-pos buffer label 'after) overlay-data))))

    ;; Sort overlays by descending position
    (setq overlay-data (sort overlay-data (lambda (a b) (> (car a) (car b)))))

    ;; Create overlays precisely at positions
    (dolist (data overlay-data)
      (let ((pos (nth 0 data))
            (buffer (nth 1 data))
            (label (nth 2 data))
            (type (nth 3 data)))
        (with-current-buffer buffer
          (let ((overlay (make-overlay pos pos)))
            (overlay-put overlay (if (eq type 'before) 'before-string 'after-string)
                         (propertize label 'face 'flash-emacs-ts-label))
            (overlay-put overlay 'flash-emacs-ts 'label)
            (push overlay flash-emacs-ts--overlays)))))))


(defun flash-emacs-ts--clear-overlays ()
  "Remove all flash treesitter overlays."
  (dolist (overlay flash-emacs-ts--overlays)
    (delete-overlay overlay))
  (setq flash-emacs-ts--overlays nil))

;;; Helper functions

(defun flash-emacs-ts--assign-labels (matches labels)
  "Assign labels to treesitter MATCHES."
  (let* ((max-labels (length labels))
         (labeled-matches '())
         (label-index 0))
    (dolist (match matches)
      (when (< label-index max-labels)
        (plist-put match :label (substring labels label-index (1+ label-index)))
        (setq label-index (1+ label-index))
        (push match labeled-matches)))
    (nreverse labeled-matches)))

(defun flash-emacs-ts--find-match-by-label (label matches)
  "Find the match with the given LABEL in MATCHES."
  (cl-find-if (lambda (match)
                (string= (plist-get match :label) label))
              matches))

(defun flash-emacs-ts--jump-to-match (match &optional range-mode)
  "Jump to the position of MATCH with Evil mode compatibility."
  (let ((target-window (plist-get match :window))
        (target-buffer (plist-get match :buffer))
        (pos (plist-get match :pos))
        (end-pos (plist-get match :end-pos))
        (current-window (selected-window)))
    (unless (eq (window-buffer current-window) target-buffer)
      (select-window target-window))
    (if range-mode
        (progn
          (goto-char pos)
          (push-mark)
          (if (flash-emacs-ts--evil-mode-active-p)
              (goto-char (1- end-pos))
            (goto-char end-pos))
          (activate-mark))
      (goto-char pos))))

;;; Main command

;;;###autoload
(defun flash-emacs-ts ()
  "Flash.nvim-like treesitter navigation with Evil mode support."
  (interactive)
  (unless (flash-emacs-ts--treesitter-available-p)
    (error "Treesitter support not available"))
  (let* ((current-window (selected-window))
         (current-point (point))
         (matches (flash-emacs-ts--get-nodes-at-point current-window current-point)))
    (cond
     ((= (length matches) 0)
      (message "No treesitter nodes found"))
     ((and flash-emacs-ts-autojump (= (length matches) 1))
      (flash-emacs-ts--jump-to-match (car matches) t)
      (message "Jumped to %s%s" 
               (plist-get (car matches) :group)
               (if (flash-emacs-ts--evil-mode-active-p) " (Evil)" "")))
     (t
      (let ((labeled-matches (flash-emacs-ts--assign-labels 
                           matches 
                           flash-emacs-ts-labels)))
        (unwind-protect
            (catch 'flash-exit
              (flash-emacs-ts--show-overlays matches labeled-matches)
              (while t
                (let* ((prompt (format "Flash treesitter%s: " 
                                     (if (flash-emacs-ts--evil-mode-active-p) " (Evil)" "")))
                       (char (read-char-exclusive prompt)))
                  (cond
                   ((or (= char 27) (= char 7))
                    (message "Flash treesitter cancelled")
                    (throw 'flash-exit nil))
                   ((= char 13)
                    (when labeled-matches
                      (flash-emacs-ts--jump-to-match (car labeled-matches) t)
                      (message "Jumped to %s" (plist-get (car labeled-matches) :group)))
                    (throw 'flash-exit nil))
                   ((and (>= char 32) (<= char 126))
                    (let* ((new-char (char-to-string char))
                           (target-match (flash-emacs-ts--find-match-by-label new-char labeled-matches)))
                      (if target-match
                          (progn
                            (flash-emacs-ts--jump-to-match target-match t)
                            (message "Jumped to %s" (plist-get target-match :group))
                            (throw 'flash-exit nil))
                        (message "Unknown key: %c" char))))
                   (t nil)))))
          (flash-emacs-ts--clear-overlays)))))))

(provide 'flash-emacs-ts)

;;; flash-emacs-ts.el ends here
