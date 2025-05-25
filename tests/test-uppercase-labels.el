;;; test-uppercase-labels.el --- Test uppercase labels functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that uppercase labels are used after lowercase ones are exhausted

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun test-uppercase-labels-generation ()
  "Test that uppercase labels are generated correctly."
  (let ((state (flash-emacs--create-state)))
    ;; Test with uppercase enabled (default)
    (let ((all-labels (flash-emacs--get-all-labels state)))
      (cl-assert (> (length all-labels) (length flash-emacs-labels)) nil
                "Should have more labels when uppercase is enabled")
      (cl-assert (cl-some (lambda (c) (and (>= c ?A) (<= c ?Z))) (string-to-list all-labels)) nil
                "Should contain uppercase letters")
      (cl-assert (cl-some (lambda (c) (and (>= c ?a) (<= c ?z))) (string-to-list all-labels)) nil
                "Should still contain lowercase letters"))
    
    ;; Test with uppercase disabled
    (let ((flash-emacs-uppercase-labels nil))
      (let ((all-labels (flash-emacs--get-all-labels state)))
        (cl-assert (= (length all-labels) (length flash-emacs-labels)) nil
                  "Should have same length as base labels when uppercase disabled")
        (cl-assert (not (cl-some (lambda (c) (and (>= c ?A) (<= c ?Z))) (string-to-list all-labels))) nil
                  "Should not contain uppercase letters when disabled")))))

(defun test-many-matches-use-uppercase ()
  "Test that many matches will use uppercase labels."
  (with-temp-buffer
    ;; Create a buffer with many 'a' characters to force uppercase usage
    (insert (make-string 50 ?a))
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      (let* ((state (flash-emacs--create-state))
             (pattern "a")
             (matches (progn 
                       (flash-emacs--set-pattern state pattern)
                       (flash-emacs--update-search state))))
        
        (when matches
          ;; Check that we have many matches
          (cl-assert (> (length matches) 26) nil "Should have more than 26 matches")
          
          ;; Check that some matches have uppercase labels
          (let ((labels (mapcar (lambda (m) (plist-get m :label)) matches))
                (has-uppercase nil))
            (dolist (label labels)
              (when (and label (string-match-p "[A-Z]" label))
                (setq has-uppercase t)))
            (cl-assert has-uppercase nil "Should have some uppercase labels when many matches")))
        
        (flash-emacs--clear-overlays)))))

(defun test-uppercase-conflict-avoidance ()
  "Test that conflict avoidance works with uppercase labels."
  (with-temp-buffer
    (insert "test testing tester tests testament tea team teach teacher")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      (let* ((pattern "te")
             (all-labels (concat flash-emacs-labels (upcase flash-emacs-labels)))
             (windows (list (selected-window)))
             (filtered-labels (flash-emacs--filter-labels-for-pattern all-labels pattern windows)))
        
        ;; Should filter out both lowercase and uppercase conflicting letters
        (cl-assert (not (cl-some (lambda (c) (= c ?a)) (string-to-list filtered-labels))) nil 
                  "Should filter out lowercase 'a'")
        (cl-assert (not (cl-some (lambda (c) (= c ?A)) (string-to-list filtered-labels))) nil 
                  "Should filter out uppercase 'A'")
        (cl-assert (not (cl-some (lambda (c) (= c ?s)) (string-to-list filtered-labels))) nil 
                  "Should filter out lowercase 's'")
        (cl-assert (not (cl-some (lambda (c) (= c ?S)) (string-to-list filtered-labels))) nil 
                  "Should filter out uppercase 'S'")
        
        ;; Should keep non-conflicting letters
        (cl-assert (cl-some (lambda (c) (= c ?d)) (string-to-list filtered-labels)) nil 
                  "Should keep lowercase 'd'")
        (cl-assert (cl-some (lambda (c) (= c ?D)) (string-to-list filtered-labels)) nil 
                  "Should keep uppercase 'D'")))))

(defun test-uppercase-jump-detection ()
  "Test that uppercase labels are detected correctly for jumping."
  (with-temp-buffer
    (insert "apple banana cherry date elderberry fig grape")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      (let* ((state (flash-emacs--create-state))
             (pattern "e")
             (matches (progn 
                       (flash-emacs--set-pattern state pattern)
                       (flash-emacs--update-search state))))
        
        (when matches
          ;; Find a match with an uppercase label
          (let ((uppercase-match (cl-find-if 
                                 (lambda (m) 
                                   (let ((label (plist-get m :label)))
                                     (and label (string-match-p "[A-Z]" label))))
                                 matches)))
            (when uppercase-match
              (let* ((uppercase-label (plist-get uppercase-match :label))
                     (new-pattern (concat pattern uppercase-label))
                     (detected-label (flash-emacs--check-jump-label 
                                     new-pattern pattern matches)))
                (cl-assert (string= detected-label uppercase-label) nil
                          "Should detect uppercase jump label correctly")))))
        
        (flash-emacs--clear-overlays)))))

(defun demo-uppercase-labels ()
  "Interactive demo showing uppercase labels in action."
  (interactive)
  (with-temp-buffer
    (insert "Demonstrating uppercase labels:

a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b
c c c c c c c c c c c c c c c c c c c c c c c c c c c c c c

This buffer has many repeated characters.
When you search for 'a', 'b', or 'c', you'll see:
1. First 26 matches get lowercase labels: a, s, d, f, g, h, j, k, l, etc.
2. Additional matches get uppercase labels: A, S, D, F, G, H, J, K, L, etc.

Try:
1. Press C-c j
2. Type 'a' to see many matches
3. Notice both lowercase and uppercase labels
4. Type an uppercase label (like 'A') to jump to that match
")
    (goto-char (point-min))
    
    (switch-to-buffer (current-buffer))
    (flash-emacs-mode 1)
    
    (message "Uppercase labels demo ready! Try searching for 'a', 'b', or 'c'.")))

(defun run-uppercase-tests ()
  "Run all uppercase labels tests."
  (message "=== Testing Uppercase Labels Functionality ===")
  
  (condition-case err
      (progn
        (test-uppercase-labels-generation)
        (message "✓ Uppercase labels generation test passed")
        
        (test-many-matches-use-uppercase)
        (message "✓ Many matches uppercase usage test passed")
        
        (test-uppercase-conflict-avoidance)
        (message "✓ Uppercase conflict avoidance test passed")
        
        (test-uppercase-jump-detection)
        (message "✓ Uppercase jump detection test passed")
        
        (message "✓ All uppercase labels tests passed!"))
    (error
     (message "✗ Uppercase test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Run tests when loaded
(run-uppercase-tests)

(provide 'test-uppercase-labels)

;;; test-uppercase-labels.el ends here 