;;; test-detailed-jump.el --- Detailed jump behavior test -*- lexical-binding: t; -*-

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

(message "=== Detailed Jump Behavior Test ===")

;; Test with different match positions
(with-temp-buffer
  (insert "start test middle test end test final")
  (goto-char 1)
  
  (let ((evil-state 'visual)
        (evil-visual-selection 'char)
        (flash-emacs-evil-visual-extend t))
    (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
      
      ;; Find all matches
      (let ((matches (flash-emacs--search-pattern "test")))
        (message "Found %d matches:" (length matches))
        (dolist (match matches)
          (message "  Match at position %d: '%s'" 
                  (plist-get match :pos) (plist-get match :text)))
        
        ;; Test jumping to each match
        (dotimes (i (min 3 (length matches)))
          (let ((match (nth i matches))
                (state (list :evil-visual-state t)))
            (goto-char 1) ; Reset cursor
            (message "Test %d: Jumping from position %d to match at %d" 
                    (1+ i) (point) (plist-get match :pos))
            
            (flash-emacs--jump-to-match match state)
            (message "  Result: cursor now at position %d" (point))
            
            ;; Verify the jump worked
            (let ((expected-pos (plist-get match :pos)))
              (if (= (point) expected-pos)
                  (message "  ✓ Jump successful")
                (message "  ✗ Jump failed: expected %d, got %d" 
                        expected-pos (point))))))))))

;; Test operator mode
(with-temp-buffer
  (insert "operator test content test here")
  (goto-char 1)
  
  (let ((evil-state 'operator))
    (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
      
      (let ((matches (flash-emacs--search-pattern "test")))
        (when (> (length matches) 0)
          (let ((match (car matches))
                (state (list :evil-visual-state nil)))
            (message "Operator mode: jumping from %d to %d" 
                    (point) (plist-get match :pos))
            (flash-emacs--jump-to-match match state)
            (message "Operator mode result: cursor at %d" (point))))))))

;; Test line visual mode with multiple lines
(with-temp-buffer
  (insert "Line 1: content\n")
  (insert "Line 2: test here\n") 
  (insert "Line 3: more content\n")
  (insert "Line 4: another test\n")
  (goto-char 1)
  
  (let ((evil-state 'visual)
        (evil-visual-selection 'line)
        (flash-emacs-evil-visual-extend t))
    (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
      
      (let ((matches (flash-emacs--search-pattern "test")))
        (message "Line visual mode: found %d matches" (length matches))
        (dolist (match matches)
          (let ((pos (plist-get match :pos)))
            (goto-char 1) ; Reset
            (message "Line visual: jumping to match at %d (line %d)" 
                    pos (line-number-at-pos pos))
            
            (let ((state (list :evil-visual-state t :evil-visual-type 'line)))
              (flash-emacs--jump-to-match match state)
              (message "Line visual result: cursor at %d (line %d)" 
                      (point) (line-number-at-pos)))))))))

(message "=== Detailed test completed ===")

(provide 'test-detailed-jump) 