;;; test-integration-complete.el --- Complete integration test -*- lexical-binding: t; -*-

(require 'cl-lib)
(load-file "flash-emacs.el")

;; Mock evil-mode more completely
(defvar evil-mode t)
(defvar evil-state 'normal)
(defvar evil-visual-selection 'char)
(defvar evil-visual-beginning 1)
(defvar evil-visual-end 10)

(defun evil-exit-visual-state ()
  (setq evil-state 'normal))

(message "=== Complete Integration Test ===")

;; Test 1: Full visual mode workflow
(defun test-visual-mode-workflow ()
  "Test the complete visual mode workflow."
  (message "--- Testing Visual Mode Workflow ---")
  
  (with-temp-buffer
    (insert "This is a test line with multiple test words for testing")
    (goto-char 1)
    
    ;; Simulate character visual mode
    (let ((evil-state 'visual)
          (evil-visual-selection 'char)
          (evil-visual-beginning 1)
          (evil-visual-end 5)
          (flash-emacs-evil-visual-extend t))
      
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
        
        ;; Create state as flash-emacs would
        (let ((state (flash-emacs--create-state)))
          (message "Created state: visual=%s, type=%s" 
                  (plist-get state :evil-visual-state)
                  (plist-get state :evil-visual-type))
          
          ;; Test pattern search
          (flash-emacs--set-pattern state "test")
          (let ((matches (flash-emacs--update-search state)))
            (message "Found %d matches for 'test'" (length matches))
            
            ;; Test jumping to first match
            (when (> (length matches) 0)
              (let ((first-match (car matches)))
                (message "Jumping to first match at %d" (plist-get first-match :pos))
                (flash-emacs--jump-to-match first-match state)
                (message "Visual mode jump result: cursor at %d" (point))))))))))

;; Test 2: Full operator mode workflow  
(defun test-operator-mode-workflow ()
  "Test the complete operator mode workflow."
  (message "--- Testing Operator Mode Workflow ---")
  
  (with-temp-buffer
    (insert "operator test content test here test")
    (goto-char 1)
    
    ;; Simulate operator-pending mode
    (let ((evil-state 'operator))
      
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
        
        ;; Create state as flash-emacs would
        (let ((state (flash-emacs--create-state)))
          (message "Created operator state: visual=%s" 
                  (plist-get state :evil-visual-state))
          
          ;; Test pattern search
          (flash-emacs--set-pattern state "test")
          (let ((matches (flash-emacs--update-search state)))
            (message "Operator mode: found %d matches" (length matches))
            
            ;; Test jumping
            (when (> (length matches) 0)
              (let ((first-match (car matches)))
                (message "Operator jump to %d" (plist-get first-match :pos))
                (flash-emacs--jump-to-match first-match state)
                (message "Operator mode result: cursor at %d" (point))))))))))

;; Test 3: Line visual mode workflow
(defun test-line-visual-workflow ()
  "Test the complete line visual mode workflow."
  (message "--- Testing Line Visual Mode Workflow ---")
  
  (with-temp-buffer
    (insert "Line 1: some content\n")
    (insert "Line 2: test content here\n")
    (insert "Line 3: more content\n") 
    (insert "Line 4: another test here\n")
    (goto-char 1)
    
    ;; Simulate line visual mode
    (let ((evil-state 'visual)
          (evil-visual-selection 'line)
          (evil-visual-beginning (line-beginning-position))
          (evil-visual-end (progn (forward-line 1) (line-end-position)))
          (flash-emacs-evil-visual-extend t))
      
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
        
        (goto-char 1) ; Reset position
        
        ;; Create state as flash-emacs would
        (let ((state (flash-emacs--create-state)))
          (message "Line visual state: visual=%s, type=%s" 
                  (plist-get state :evil-visual-state)
                  (plist-get state :evil-visual-type))
          
          ;; Test pattern search
          (flash-emacs--set-pattern state "test")
          (let ((matches (flash-emacs--update-search state)))
            (message "Line visual: found %d matches" (length matches))
            
            ;; Test jumping to each match
            (dolist (match matches)
              (goto-char 1) ; Reset
              (let ((pos (plist-get match :pos)))
                (message "Line visual jump to %d (line %d)" 
                        pos (line-number-at-pos))
                (flash-emacs--jump-to-match match state)
                (message "Result: cursor at %d (line %d)" 
                        (point) (line-number-at-pos))))))))))

;; Test 4: Input handling simulation
(defun test-input-handling ()
  "Test input handling with different modes."
  (message "--- Testing Input Handling ---")
  
  (with-temp-buffer
    (insert "input test content test here")
    (goto-char 1)
    
    ;; Test visual mode input handling
    (let ((evil-state 'visual)
          (evil-visual-selection 'char)
          (flash-emacs-evil-visual-extend t))
      
      (cl-letf (((symbol-function 'featurep) (lambda (f) (eq f 'evil))))
        
        (let ((state (flash-emacs--create-state)))
          ;; Simulate typing "test"
          (flash-emacs--set-pattern state "t")
          (flash-emacs--update-search state)
          (message "After 't': %d matches" (length (flash-emacs--get-matches state)))
          
          (flash-emacs--set-pattern state "te")
          (flash-emacs--update-search state)
          (message "After 'te': %d matches" (length (flash-emacs--get-matches state)))
          
          (flash-emacs--set-pattern state "test")
          (flash-emacs--update-search state)
          (let ((matches (flash-emacs--get-matches state)))
            (message "After 'test': %d matches" (length matches))
            
            ;; Test label assignment
            (dolist (match matches)
              (message "Match at %d has label '%s'" 
                      (plist-get match :pos) 
                      (or (plist-get match :label) "none")))))))))

;; Test 5: Multi-window behavior
(defun test-multi-window ()
  "Test multi-window behavior."
  (message "--- Testing Multi-Window Behavior ---")
  
  (with-temp-buffer
    (insert "window test content test here")
    (let ((flash-emacs-multi-window t))
      
      ;; Test with single window
      (let ((matches (flash-emacs--search-pattern "test")))
        (message "Single window: %d matches" (length matches)))
      
      ;; Test with multi-window disabled
      (let ((flash-emacs-multi-window nil))
        (let ((matches (flash-emacs--search-pattern "test")))
          (message "Multi-window disabled: %d matches" (length matches)))))))

;; Run all tests
(test-visual-mode-workflow)
(test-operator-mode-workflow)
(test-line-visual-workflow)
(test-input-handling)
(test-multi-window)

(message "=== Complete Integration Test Finished ===")

(provide 'test-integration-complete) 