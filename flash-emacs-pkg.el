;;; flash-emacs-pkg.el --- Package definition for flash-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;;; Commentary:

;; Package definition file for flash-emacs.

;;; Code:

(define-package "flash-emacs" "1.0.0"
  "Flash.nvim-like jump navigation for Emacs"
  '((emacs "26.1")
    (cl-lib "0.5"))
  :keywords '("navigation" "jump" "search" "convenience")
  :url "https://github.com/flash-emacs/flash-emacs"
  :maintainer '("Flash-Emacs Contributors")
  :authors '(("Flash-Emacs Contributors")))

;;; flash-emacs-pkg.el ends here 