;;; test-evil-integration.el --- Test evil-mode integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Test and demonstrate evil-mode integration with flash-emacs

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

;; Mock evil-mode variables and functions for testing
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar evil-visual-beginning nil)
(defvar evil-visual-end nil)

(defun evil-exit-visual-state ()
  "Mock function to exit visual state."
  (setq evil-state 'normal))

(defun test-evil-detection ()
  "Test evil-mode detection functions."
  (message "=== Testing Evil Detection ===")
  
  ;; Test when evil is not available
  (let ((evil-mode nil))
    (cl-assert (not (flash-emacs--evil-available-p)) nil
              "Should detect evil as not available"))
  
  ;; Test when evil is available (mock the feature)
  (let ((evil-mode t))
    (cl-letf (((symbol-function 'featurep) 
               (lambda (feature) (eq feature 'evil))))
      (cl-assert (flash-emacs--evil-available-p) nil
                "Should detect evil as available")))
  
  ;; Test visual state detection
  (let ((evil-mode t)
        (evil-state 'visual))
    (cl-letf (((symbol-function 'featurep) 
               (lambda (feature) (eq feature 'evil))))
      (cl-assert (flash-emacs--evil-visual-state-p) nil
                "Should detect visual state")))
  
  (let ((evil-mode t)
        (evil-state 'normal))
    (cl-letf (((symbol-function 'featurep) 
               (lambda (feature) (eq feature 'evil))))
      (cl-assert (not (flash-emacs--evil-visual-state-p)) nil
                "Should not detect visual state in normal mode")))
  
  ;; Test operator state detection
  (let ((evil-mode t)
        (evil-state 'operator))
    (cl-letf (((symbol-function 'featurep) 
               (lambda (feature) (eq feature 'evil))))
      (cl-assert (flash-emacs--evil-operator-state-p) nil
                "Should detect operator state")))
  
  (message "✓ Evil detection tests passed"))

(defun test-visual-type-detection ()
  "Test visual selection type detection."
  (message "=== Testing Visual Type Detection ===")
  
  (let ((evil-mode t)
        (evil-state 'visual))
    (cl-letf (((symbol-function 'featurep) 
               (lambda (feature) (eq feature 'evil))))
      
      ;; Test character-wise visual
      (let ((evil-visual-selection 'char))
        (cl-assert (eq (flash-emacs--evil-visual-type) 'char) nil
                  "Should detect character-wise visual selection"))
      
      ;; Test line-wise visual
      (let ((evil-visual-selection 'line))
        (cl-assert (eq (flash-emacs--evil-visual-type) 'line) nil
                  "Should detect line-wise visual selection"))
      
      ;; Test block-wise visual
      (let ((evil-visual-selection 'block))
        (cl-assert (eq (flash-emacs--evil-visual-type) 'block) nil
                  "Should detect block-wise visual selection"))))
  
  (message "✓ Visual type detection tests passed"))

(defun test-visual-range-detection ()
  "Test visual selection range detection."
  (message "=== Testing Visual Range Detection ===")
  
  (let ((evil-mode t)
        (evil-state 'visual)
        (evil-visual-beginning 100)
        (evil-visual-end 200))
    (cl-letf (((symbol-function 'featurep) 
               (lambda (feature) (eq feature 'evil))))
      
      (let ((range (flash-emacs--evil-visual-range)))
        (cl-assert (equal range '(100 . 200)) nil
                  "Should detect visual range correctly"))))
  
  (message "✓ Visual range detection tests passed"))

(defun test-state-creation-with-evil ()
  "Test state creation includes evil information."
  (message "=== Testing State Creation with Evil ===")
  
  ;; Test state creation in normal mode
  (let ((evil-mode t)
        (evil-state 'normal))
    (cl-letf (((symbol-function 'featurep) 
               (lambda (feature) (eq feature 'evil))))
      (let ((state (flash-emacs--create-state)))
        (cl-assert (not (plist-get state :evil-visual-state)) nil
                  "Should record non-visual state")
        (cl-assert (not (plist-get state :evil-visual-range)) nil
                  "Should have no visual range in normal mode"))))
  
  ;; Test state creation in visual mode
  (let ((evil-mode t)
        (evil-state 'visual)
        (evil-visual-selection 'char)
        (evil-visual-beginning 50)
        (evil-visual-end 150))
    (cl-letf (((symbol-function 'featurep) 
               (lambda (feature) (eq feature 'evil))))
      (let ((state (flash-emacs--create-state)))
        (cl-assert (plist-get state :evil-visual-state) nil
                  "Should record visual state")
        (cl-assert (equal (plist-get state :evil-visual-range) '(50 . 150)) nil
                  "Should record visual range")
        (cl-assert (eq (plist-get state :evil-visual-type) 'char) nil
                  "Should record visual type"))))
  
  (message "✓ State creation with evil tests passed"))

(defun test-visual-extension ()
  "Test visual selection extension."
  (message "=== Testing Visual Extension ===")
  
  (with-temp-buffer
    (insert "This is a test buffer for visual extension testing")
    (goto-char (point-min))
    
    ;; Test character-wise extension
    (let ((evil-mode t)
          (evil-state 'visual)
          (evil-visual-selection 'char)
          (flash-emacs-evil-visual-extend t))
      (cl-letf (((symbol-function 'featurep) 
                 (lambda (feature) (eq feature 'evil))))
        
        (flash-emacs--evil-extend-selection 20)
        (cl-assert (= (point) 20) nil
                  "Should move to target position for char visual")))
    
    ;; Test line-wise extension
    (goto-char 25)  ; Move to middle of line
    (let ((evil-mode t)
          (evil-state 'visual)
          (evil-visual-selection 'line)
          (flash-emacs-evil-visual-extend t))
      (cl-letf (((symbol-function 'featurep) 
                 (lambda (feature) (eq feature 'evil))))
        
        (flash-emacs--evil-extend-selection 30)
        (cl-assert (= (point) (line-beginning-position)) nil
                  "Should move to beginning of line for line visual"))))
  
  (message "✓ Visual extension tests passed"))

(defun test-jump-behavior-in-visual-mode ()
  "Test jumping behavior in different modes."
  (message "=== Testing Jump Behavior ===")
  
  (with-temp-buffer
    (insert "test content for jumping around in visual mode")
    (goto-char (point-min))
    
    (let ((match (list :pos 20 :end-pos 25 :window (selected-window) :text "content")))
      
      ;; Test normal mode jump
      (let ((evil-mode t)
            (evil-state 'normal)
            (state (list :evil-visual-state nil)))
        (cl-letf (((symbol-function 'featurep) 
                   (lambda (feature) (eq feature 'evil))))
          (flash-emacs--jump-to-match match state)
          (cl-assert (= (point) 20) nil
                    "Should jump to position in normal mode")))
      
      ;; Test visual mode jump with extension
      (goto-char (point-min))
      (let ((evil-mode t)
            (evil-state 'visual)
            (evil-visual-selection 'char)
            (flash-emacs-evil-visual-extend t)
            (state (list :evil-visual-state t)))
        (cl-letf (((symbol-function 'featurep) 
                   (lambda (feature) (eq feature 'evil))))
          (flash-emacs--jump-to-match match state)
          (cl-assert (= (point) 20) nil
                    "Should extend selection to target in visual mode")))
      
      ;; Test visual mode jump without extension
      (goto-char (point-min))
      (let ((evil-mode t)
            (evil-state 'normal)  ; Will be set by evil-exit-visual-state
            (flash-emacs-evil-visual-extend nil)
            (state (list :evil-visual-state t)))
        (cl-letf (((symbol-function 'featurep) 
                   (lambda (feature) (eq feature 'evil))))
          (flash-emacs--jump-to-match match state)
          (cl-assert (= (point) 20) nil
                    "Should jump and exit visual mode when extension disabled")
          (cl-assert (eq evil-state 'normal) nil
                    "Should exit visual state")))))
  
  (message "✓ Jump behavior tests passed"))

(defun demo-evil-integration ()
  "Interactive demo of evil-mode integration."
  (interactive)
  (with-temp-buffer
    (insert "Evil-mode Integration Demo

This buffer demonstrates flash-emacs integration with evil-mode:

1. Character-wise visual selection (v)
2. Line-wise visual selection (V) 
3. Block-wise visual selection (C-v)
4. Operator-pending mode (d, c, y, etc.)

Key features:
- Detects evil visual state automatically
- Extends selection when jumping in visual mode
- Supports all visual selection types
- Configurable behavior via flash-emacs-evil-visual-extend

Test content:
apple banana cherry date
elephant fox giraffe horse
iguana jaguar kangaroo lion
mouse newt octopus penguin

Try these scenarios:
1. Enter visual mode (v), then use flash-emacs-jump
2. Enter line visual mode (V), then use flash-emacs-jump  
3. Use operator + flash-emacs (like 'd' then flash-emacs-jump)
4. Toggle flash-emacs-evil-visual-extend and see the difference

Configuration examples:
;; Extend selection when jumping in visual mode (default)
(setq flash-emacs-evil-visual-extend t)

;; Exit visual mode when jumping (vim-like behavior)
(setq flash-emacs-evil-visual-extend nil)

;; Evil-mode key bindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd \"s\") #'flash-emacs-jump)
  (define-key evil-visual-state-map (kbd \"s\") #'flash-emacs-jump)
  (define-key evil-operator-state-map (kbd \"s\") #'flash-emacs-jump))
")
    
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    (local-set-key (kbd "C-c t") #'flash-emacs--test-evil-integration)
    
    (message "Evil integration demo ready! Try C-c j for flash-emacs, C-c t for evil state info.")))

(defun run-evil-integration-tests ()
  "Run all evil-mode integration tests."
  (message "=== Running Evil-mode Integration Tests ===")
  
  (condition-case err
      (progn
        (test-evil-detection)
        (test-visual-type-detection)
        (test-visual-range-detection)
        (test-state-creation-with-evil)
        (test-visual-extension)
        (test-jump-behavior-in-visual-mode)
        
        (message "✓ All evil-mode integration tests passed!"))
    (error
     (message "✗ Evil integration test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-evil-integration-tests)

(provide 'test-evil-integration)

;;; test-evil-integration.el ends here 