;;; flash-emacs-autoloads.el --- automatically extracted autoloads -*- lexical-binding: t; -*-

;;; Code:

;;;### (autoloads nil "flash-emacs" "flash-emacs.el" (0 0 0 0))
;;; Generated autoloads from flash-emacs.el

(autoload 'flash-emacs-jump "flash-emacs" "\
Start flash jump mode.
Type characters to search, then use the displayed labels to jump." t nil)

(autoload 'flash-emacs-mode "flash-emacs" "\
Minor mode for flash-emacs jump navigation.

If called interactively, enable Flash-Emacs mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-flash-emacs-mode 'globalized-minor-mode t)

(defvar global-flash-emacs-mode nil "\
Non-nil if Global Flash-Emacs mode is enabled.
See the `global-flash-emacs-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flash-emacs-mode'.")

(custom-autoload 'global-flash-emacs-mode "flash-emacs" nil)

(autoload 'global-flash-emacs-mode "flash-emacs" "\
Toggle Flash-Emacs mode in all buffers.
With prefix ARG, enable Global Flash-Emacs mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Flash-Emacs mode is enabled in all buffers where
`(lambda nil (flash-emacs-mode 1))' would do it.
See `flash-emacs-mode' for more information on Flash-Emacs mode.

\(fn &optional ARG)" t nil)

;;;***

(provide 'flash-emacs-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flash-emacs-autoloads.el ends here 