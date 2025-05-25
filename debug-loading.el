;;; debug-loading.el --- Debug flash-emacs loading issues -*- lexical-binding: t; -*-

;;; Commentary:
;; Debug script to help identify loading issues

;;; Code:

(defun debug-flash-emacs-loading ()
  "Debug flash-emacs loading to find source of unwanted message."
  (interactive)
  
  (message "=== Flash-Emacs Loading Debug ===")
  
  ;; Check if flash-emacs is already loaded
  (if (featurep 'flash-emacs)
      (progn
        (message "❌ flash-emacs is already loaded!")
        (message "Current load path for flash-emacs:")
        (message "  %s" (locate-library "flash-emacs"))
        (message "")
        (message "To fix this:")
        (message "1. Restart Emacs")
        (message "2. Or run: (unload-feature 'flash-emacs t)")
        (message "3. Then reload from the correct location"))
    (message "✅ flash-emacs not yet loaded"))
  
  ;; Check current directory
  (message "")
  (message "Current directory: %s" default-directory)
  
  ;; Check if flash-emacs.el exists here
  (if (file-exists-p "flash-emacs.el")
      (message "✅ flash-emacs.el found in current directory")
    (message "❌ flash-emacs.el NOT found in current directory"))
  
  ;; Check if compiled version exists
  (if (file-exists-p "flash-emacs.elc")
      (message "✅ flash-emacs.elc found (compiled version)")
    (message "❌ No compiled version found"))
  
  ;; Check load-path
  (message "")
  (message "Current load-path entries containing 'flash':")
  (dolist (path load-path)
    (when (string-match-p "flash" path)
      (message "  %s" path)))
  
  ;; Show how to properly load
  (message "")
  (message "To load the correct version:")
  (message "1. cd to: %s" (expand-file-name default-directory))
  (message "2. Run: (load-file \"flash-emacs.el\")")
  (message "3. Or: (add-to-list 'load-path \"%s\")" (expand-file-name default-directory))
  (message "4. Then: (require 'flash-emacs)")
  
  (message "=== Debug Complete ==="))

;; Run the debug
(debug-flash-emacs-loading)

;;; debug-loading.el ends here 