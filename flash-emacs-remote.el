;;; flash-emacs-remote.el --- Remote operations for flash-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Flash-Emacs Contributors
;; Maintainer: Flash-Emacs Contributors
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (evil "1.0.0") (flash-emacs "1.0.0"))
;; Keywords: navigation, jump, search, convenience, evil
;; URL: https://github.com/flash-emacs/flash-emacs

;;; Commentary:

;; This package provides remote operation functionality for flash-emacs,
;; similar to flash.nvim's remote feature.
;;
;; Remote operations allow you to:
;; 1. Start an operator (like delete, yank, change)
;; 2. Press 'r' to enter remote mode
;; 3. Jump to any position using flash
;; 4. Execute a motion or text object at that position
;; 5. Return to your original position
;;
;; Example: "yr<flash-label>iw" will yank a word at a remote position

;;; Code:

(require 'evil)
(require 'flash-emacs)
(require 'flash-emacs-ts)

;;; Customization

(defgroup flash-emacs-remote nil
  "Remote operations for flash-emacs."
  :group 'flash-emacs
  :prefix "flash-emacs-remote-")

(defvar flash-emacs-remote--current-operator nil
  "Current operator being executed.")

(defvar flash-emacs-remote--current-position nil
  "Current position being executed.")

(defun flash-emacs-remote--save-state ()
  "Save the current state for later restoration."
  (setq flash-emacs-remote--current-operator evil-this-operator)
  (setq flash-emacs-remote--current-position (point)))

(evil-define-operator flash-emacs-remote--restore-operator (beg end type register)
"Handle the operator at the target position."
  (funcall flash-emacs-remote--current-operator beg end type register))

;;;###autoload
(defun flash-emacs-remote ()
  "Start flash remote operation.
This allows you to perform operations in remote windows with automatic restoration.
Usage: Start an operator (like y, d, c), press 'r', then use flash to jump,
then input a motion or text object."
  (interactive)
  ;; Save current state
  (flash-emacs-remote--save-state)
  (setq evil-inhibit-operator t)
  (flash-emacs-jump)
  (call-interactively #'flash-emacs-remote--restore-operator)
  ;; jump to the original position
  (goto-char flash-emacs-remote--current-position))

(defun flash-emacs-remote-ts ()
  "Start a remote operation with the ts feature."
  (interactive)
  ;; Save current state
  (flash-emacs-remote--save-state))

;;;###autoload
(defun flash-emacs-remote-setup ()
  "Set up keybindings for flash-emacs-remote."
  (interactive)
  
  ;; Bind 'r' in operator-pending mode to flash-emacs-remote
  (define-key evil-operator-state-map (kbd "r") #'flash-emacs-remote)
  ;; Bind 'R' in operator-pending mode to flash-emacs-remote-ts
  (define-key evil-operator-state-map (kbd "R") #'flash-emacs-remote-ts))

(flash-emacs-remote-setup)
(provide 'flash-emacs-remote)

;;; flash-emacs-remote.el ends here
