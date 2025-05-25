;;; test-simple-issues.el --- Simple test for visual mode issues -*- lexical-binding: t; -*-

(require 'cl-lib)
(load-file "flash-emacs.el")

;; Mock evil-mode
(defvar evil-mode t)
(defvar evil-state 'normal)
(defvar evil-visual-selection 'char)
(defvar evil-visual-beginning 1)
(defvar evil-visual-end 10)

(defun evil-exit-visual-state ()
  (setq evil-state 'normal))

(message "=== Testing Visual Mode Issues ===")

;; Test 1: Check if visual mode detection works
(let ((evil-state 'visual))
  (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
    (message "Visual state detected: %s" (flash-emacs--evil-visual-state-p))))

;; Test 2: Check operator state detection  
(let ((evil-state 'operator))
  (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
    (message "Operator state detected: %s" (flash-emacs--evil-operator-state-p))))

;; Test 3: Test jump behavior in visual mode
(with-temp-buffer
  (insert "test content here test more test")
  (goto-char 1)
  
  (let ((evil-state 'visual)
        (evil-visual-selection 'char)
        (flash-emacs-evil-visual-extend t))
    (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
      
      ;; Find matches
      (let ((matches (flash-emacs--search-pattern "test")))
        (message "Found %d matches" (length matches))
        
        (when (> (length matches) 0)
          (let ((match (car matches))
                (state (list :evil-visual-state t)))
            (message "Match position: %d" (plist-get match :pos))
            (message "Before jump: cursor at %d" (point))
            
            ;; Test the jump
            (flash-emacs--jump-to-match match state)
            (message "After jump: cursor at %d" (point))))))))

;; Test 4: Test line visual mode
(with-temp-buffer
  (insert "Line 1\nLine 2 test\nLine 3\n")
  (goto-char 1)
  
  (let ((evil-state 'visual)
        (evil-visual-selection 'line)
        (flash-emacs-evil-visual-extend t))
    (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
      
      (let ((matches (flash-emacs--search-pattern "test")))
        (when (> (length matches) 0)
          (let ((match (car matches))
                (state (list :evil-visual-state t :evil-visual-type 'line)))
            (message "Line visual: match at %d" (plist-get match :pos))
            (flash-emacs--jump-to-match match state)
            (message "Line visual: after jump at %d, line beginning: %d" 
                    (point) (line-beginning-position))))))))

(message "=== Tests completed ===")

(provide 'test-simple-issues) 