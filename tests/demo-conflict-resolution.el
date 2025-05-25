;;; demo-conflict-resolution.el --- Demo of conflict resolution -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive demo showing how flash-emacs avoids label conflicts

;;; Code:

(load-file "flash-emacs.el")

(defun demo-conflict-resolution ()
  "Interactive demo showing conflict resolution in action."
  (interactive)
  
  (let ((demo-buffer (get-buffer-create "*Flash Conflict Resolution Demo*")))
    (with-current-buffer demo-buffer
      (erase-buffer)
      (insert "=== Flash-Emacs Conflict Resolution Demo ===

This demo shows how flash-emacs intelligently avoids label conflicts.

EXAMPLE TEXT:
test testing tester tests testament
tea team teach teacher
apple application apply approach
function definition defun defvar
the then there these them they

=== HOW IT WORKS ===

1. PROBLEM: If we search for 'te' and assign labels 'a', 's', 'd', 'f'...
   - Typing 'a' could mean: JUMP to label 'a' OR search for 'tea'
   - Typing 's' could mean: JUMP to label 's' OR search for 'tes'
   
2. SOLUTION: Flash-emacs scans the text and removes conflicting labels:
   - Finds 'tea' in text → removes 'a' from available labels
   - Finds 'tes' in text → removes 's' from available labels
   - Only assigns safe labels like 'd', 'f', 'g', etc.

3. RESULT: No ambiguity!
   - 'te' + 'd' = JUMP to label 'd' (safe)
   - 'te' + 'a' = SEARCH for 'tea' (no label 'a' assigned)

=== TRY IT YOURSELF ===

1. Press C-c j to start flash jump
2. Type 'te' and observe which labels appear
3. Notice 'a' and 's' are NOT assigned as labels
4. Try typing 'a' - it will search for 'tea', not jump
5. Try typing 'd' - it will jump to the first match with label 'd'

=== STEP-BY-STEP EXAMPLE ===

Pattern: 'te'
Text contains: 'tea', 'team', 'teach', 'teacher', 'test', 'testing', 'tester', 'tests', 'testament'

Conflict analysis:
- 'te' + 'a' → 'tea' (found in text) → remove 'a' from labels
- 'te' + 's' → 'tes' (found in text) → remove 's' from labels  
- 'te' + 'd' → 'ted' (not found) → keep 'd' as label
- 'te' + 'f' → 'tef' (not found) → keep 'f' as label

Final labels: d, f, g, h, j, k, l, q, w, r, y, u, i, o, p, z, x, c, v, b, n, m

This ensures typing continues search when appropriate, jumps when intended!
")
      (goto-char (point-min)))
    
    (switch-to-buffer demo-buffer)
    (flash-emacs-mode 1)
    
    (message "Conflict resolution demo ready! Try the examples above.")))

(defun analyze-pattern-conflicts (pattern)
  "Analyze and display conflicts for a given pattern."
  (interactive "sPattern to analyze: ")
  
  (with-temp-buffer
    (insert "test testing tester tests testament tea team teach teacher apple application apply")
    (goto-char (point-min))
    
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      
      (let* ((all-labels "asdfghjklqwertyuiopzxcvbnm")
             (windows (list (selected-window)))
             (conflicting (flash-emacs--find-conflicting-labels 
                          pattern 
                          (mapcar #'char-to-string (string-to-list all-labels))
                          (selected-window)))
             (filtered-labels (flash-emacs--filter-labels-for-pattern all-labels pattern windows)))
        
        (message "=== Conflict Analysis for Pattern '%s' ===" pattern)
        (message "Original labels: %s" all-labels)
        (message "Conflicting labels: %s" (mapconcat #'identity conflicting " "))
        (message "Safe labels: %s" filtered-labels)
        (message "Removed: %s" (cl-set-difference (string-to-list all-labels) 
                                                  (string-to-list filtered-labels)))))))

(defun demo-step-by-step-conflicts ()
  "Show step-by-step how conflicts are resolved."
  (interactive)
  
  (message "=== Step-by-Step Conflict Resolution ===")
  
  (dolist (pattern '("t" "te" "tes" "test"))
    (analyze-pattern-conflicts pattern)
    (message ""))
  
  (message "Notice how more labels get filtered out as the pattern gets more specific!"))

;; Auto-run demo when loaded interactively
(when (called-interactively-p 'any)
  (demo-conflict-resolution))

(provide 'demo-conflict-resolution)

;;; demo-conflict-resolution.el ends here 