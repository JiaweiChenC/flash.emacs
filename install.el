;;; install.el --- Installation script for flash-emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; This script provides an easy way to install flash-emacs.
;; Run with: emacs --batch -l install.el

;;; Code:

(require 'package)

(defun flash-emacs-install ()
  "Install flash-emacs package."
  (interactive)
  
  ;; Add MELPA if not already present
  (unless (assoc "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
  
  ;; Initialize package system
  (package-initialize)
  
  ;; Refresh package contents
  (unless package-archive-contents
    (package-refresh-contents))
  
  ;; Install flash-emacs
  (condition-case err
      (progn
        (package-install 'flash-emacs)
        (message "✓ flash-emacs installed successfully!")
        (message "")
        (message "To use flash-emacs, add this to your Emacs configuration:")
        (message "")
        (message "  (require 'flash-emacs)")
        (message "  (flash-emacs-mode 1)")
        (message "")
        (message "Then use C-c j to start flash jump!"))
    (error
     (message "✗ Failed to install flash-emacs: %s" (error-message-string err))
     (message "")
     (message "Manual installation:")
     (message "1. Download flash-emacs.el")
     (message "2. Add to your load-path")
     (message "3. (require 'flash-emacs)")
     (message "4. (flash-emacs-mode 1)"))))

(defun flash-emacs-install-local ()
  "Install flash-emacs from local directory."
  (interactive)
  (let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
    (condition-case err
        (progn
          ;; Add current directory to load path
          (add-to-list 'load-path current-dir)
          
          ;; Load flash-emacs
          (require 'flash-emacs)
          
          ;; Test that it loads correctly
          (if (fboundp 'flash-emacs-jump)
              (progn
                (message "✓ flash-emacs loaded successfully from local directory!")
                (message "")
                (message "To make this permanent, add to your Emacs configuration:")
                (message "")
                (message "  (add-to-list 'load-path \"%s\")" current-dir)
                (message "  (require 'flash-emacs)")
                (message "  (flash-emacs-mode 1)")
                (message "")
                (message "Then use C-c j to start flash jump!"))
            (error "flash-emacs-jump function not found after loading")))
      (error
       (message "✗ Failed to load flash-emacs locally: %s" (error-message-string err))))))

;; When run as a script
(when noninteractive
  (if (file-exists-p "flash-emacs.el")
      (flash-emacs-install-local)
    (flash-emacs-install)))

(provide 'install)

;;; install.el ends here 