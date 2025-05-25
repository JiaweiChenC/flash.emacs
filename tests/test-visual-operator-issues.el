;;; test-visual-operator-issues.el --- Test visual mode and operator state issues -*- lexical-binding: t; -*-

;;; Commentary:
;; Test to identify issues with visual mode positioning and operator state

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

;; Mock evil-mode for testing
(defvar evil-mode t)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar evil-visual-beginning nil)
(defvar evil-visual-end nil)

(defun evil-exit-visual-state ()
  "Mock function to exit visual state."
  (setq evil-state 'normal))

(defun test-visual-mode-positioning ()
  "Test visual mode positioning behavior."
  (message "=== Testing Visual Mode Positioning ===")
  
  (with-temp-buffer
    (insert "This is a test line with multiple test words for testing")
    (goto-char (point-min))
    
    ;; Test character visual mode
    (let ((evil-mode t)
          (evil-state 'visual)
          (evil-visual-selection 'char)
          (evil-visual-beginning 10)
          (evil-visual-end 15)
          (flash-emacs-evil-visual-extend t))
      
      (cl-letf (((symbol-function 'featurep) 
                 (lambda (feature) (eq feature 'evil))))
        
        ;; Create a match at position 30
        (let ((match (list :pos 30 :end-pos 34 :window (selected-window) :text "test"))
              (state (list :evil-visual-state t 
                          :evil-visual-range '(10 . 15)
                          :evil-visual-type 'char)))
          
          (message "Before jump: cursor at %d, visual range %s" 
                  (point) (plist-get state :evil-visual-range))
          
          ;; Test the jump
          (flash-emacs--jump-to-match match state)
          
          (message "After jump: cursor at %d" (point))
          
          ;; In visual mode, cursor should be at the match position
          (cl-assert (= (point) 30) nil
                    "Visual mode jump should position cursor at match start")))))
  
  (message "✓ Visual mode positioning tests passed"))

(defun test-operator-state-detection ()
  "Test operator state detection."
  (message "=== Testing Operator State Detection ===")
  
  (with-temp-buffer
    (insert "test content for operator testing")
    (goto-char (point-min))
    
    ;; Test operator state detection
    (let ((evil-mode t)
          (evil-state 'operator))
      
      (cl-letf (((symbol-function 'featurep) 
                 (lambda (feature) (eq feature 'evil))))
        
        (cl-assert (flash-emacs--evil-operator-state-p) nil
                  "Should detect operator state")
        
        ;; Test state creation includes operator info
        (let ((state (flash-emacs--create-state)))
          (message "State created in operator mode: %s" state)
          ;; State should capture the current mode
          (cl-assert (not (plist-get state :evil-visual-state)) nil
                    "Should not be in visual state during operator mode")))))
  
  (message "✓ Operator state detection tests passed"))

(defun test-visual-line-mode ()
  "Test visual line mode behavior."
  (message "=== Testing Visual Line Mode ===")
  
  (with-temp-buffer
    (insert "Line 1: test content here\n")
    (insert "Line 2: more test content\n") 
    (insert "Line 3: final test line\n")
    (goto-char (point-min))
    
    ;; Test line visual mode
    (let ((evil-mode t)
          (evil-state 'visual)
          (evil-visual-selection 'line)
          (evil-visual-beginning (line-beginning-position))
          (evil-visual-end (progn (forward-line 1) (line-end-position)))
          (flash-emacs-evil-visual-extend t))
      
      (cl-letf (((symbol-function 'featurep) 
                 (lambda (feature) (eq feature 'evil))))
        
        ;; Create a match on line 3
        (goto-char (point-max))
        (forward-line -1)
        (let ((match-pos (+ (line-beginning-position) 10)))
          (let ((match (list :pos match-pos :end-pos (+ match-pos 4) 
                            :window (selected-window) :text "test"))
                (state (list :evil-visual-state t 
                            :evil-visual-range (cons evil-visual-beginning evil-visual-end)
                            :evil-visual-type 'line)))
            
            (goto-char (point-min))
            (message "Before jump: cursor at %d (line %d)" 
                    (point) (line-number-at-pos))
            
            ;; Test the jump
            (flash-emacs--jump-to-match match state)
            
            (message "After jump: cursor at %d (line %d)" 
                    (point) (line-number-at-pos))
            
            ;; In line visual mode, should be at beginning of target line
            (cl-assert (= (point) (line-beginning-position)) nil
                      "Line visual mode should position cursor at line beginning")))))
  
  (message "✓ Visual line mode tests passed"))

(defun test-search-pattern-with-modes ()
  "Test search pattern behavior in different modes."
  (message "=== Testing Search Pattern with Different Modes ===")
  
  (with-temp-buffer
    (insert "test content test here test everywhere")
    (goto-char (point-min))
    
    ;; Test normal mode
    (let ((evil-mode t)
          (evil-state 'normal))
      (cl-letf (((symbol-function 'featurep) 
                 (lambda (feature) (eq feature 'evil))))
        (let ((matches (flash-emacs--search-pattern "test")))
          (message "Normal mode: found %d matches" (length matches))
          (cl-assert (> (length matches) 0) nil
                    "Should find matches in normal mode"))))
    
    ;; Test visual mode
    (let ((evil-mode t)
          (evil-state 'visual)
          (evil-visual-selection 'char))
      (cl-letf (((symbol-function 'featurep) 
                 (lambda (feature) (eq feature 'evil))))
        (let ((matches (flash-emacs--search-pattern "test")))
          (message "Visual mode: found %d matches" (length matches))
          (cl-assert (> (length matches) 0) nil
                    "Should find matches in visual mode"))))
    
    ;; Test operator mode
    (let ((evil-mode t)
          (evil-state 'operator))
      (cl-letf (((symbol-function 'featurep) 
                 (lambda (feature) (eq feature 'evil))))
        (let ((matches (flash-emacs--search-pattern "test")))
          (message "Operator mode: found %d matches" (length matches))
          (cl-assert (> (length matches) 0) nil
                    "Should find matches in operator mode")))))
  
  (message "✓ Search pattern mode tests passed"))

(defun test-jump-behavior-issues ()
  "Test specific jump behavior issues."
  (message "=== Testing Jump Behavior Issues ===")
  
  (with-temp-buffer
    (insert "start test middle test end")
    (goto-char 1) ; Position at start
    
    ;; Test 1: Visual mode should jump to match position, not extend differently
    (let ((evil-mode t)
          (evil-state 'visual)
          (evil-visual-selection 'char)
          (evil-visual-beginning 1)
          (evil-visual-end 5)
          (flash-emacs-evil-visual-extend t))
      
      (cl-letf (((symbol-function 'featurep) 
                 (lambda (feature) (eq feature 'evil))))
        
        ;; Find "test" matches
        (let ((matches (flash-emacs--search-pattern "test")))
          (when (> (length matches) 0)
            (let ((first-match (car matches))
                  (state (list :evil-visual-state t)))
              
              (message "Jumping to first test match at position %d" 
                      (plist-get first-match :pos))
              
              ;; This should position cursor at the match
              (flash-emacs--jump-to-match first-match state)
              
              (message "After jump: cursor at %d" (point))
              
              ;; The issue might be here - cursor should be at match position
              (let ((expected-pos (plist-get first-match :pos)))
                (cl-assert (= (point) expected-pos) nil
                          "Visual mode jump should position cursor at match position %d, got %d"
                          expected-pos (point))))))))
    
    ;; Test 2: Operator mode behavior
    (goto-char 1)
    (let ((evil-mode t)
          (evil-state 'operator))
      
      (cl-letf (((symbol-function 'featurep) 
                 (lambda (feature) (eq feature 'evil))))
        
        (let ((matches (flash-emacs--search-pattern "test")))
          (when (> (length matches) 0)
            (let ((first-match (car matches))
                  (state (list :evil-visual-state nil)))
              
              (message "Operator mode jump to position %d" 
                      (plist-get first-match :pos))
              
              (flash-emacs--jump-to-match first-match state)
              
              (message "After operator jump: cursor at %d" (point)))))))
  
  (message "✓ Jump behavior issue tests completed"))

(defun run-visual-operator-tests ()
  "Run all visual mode and operator state tests."
  (message "=== Running Visual Mode and Operator State Tests ===")
  
  (condition-case err
      (progn
        (test-visual-mode-positioning)
        (test-operator-state-detection)
        (test-visual-line-mode)
        (test-search-pattern-with-modes)
        (test-jump-behavior-issues)
        
        (message "✓ All visual/operator tests completed!"))
    (error
     (message "✗ Visual/operator test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-visual-operator-tests)

(provide 'test-visual-operator-issues)

;;; test-visual-operator-issues.el ends here 