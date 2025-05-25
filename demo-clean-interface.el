;;; demo-clean-interface.el --- Demo of clean interface improvements -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive demo showcasing the simplified prompt and silent jumping

;;; Code:

(load-file "flash-emacs.el")

(defun demo-clean-interface ()
  "Interactive demo showing the new clean interface improvements."
  (interactive)
  (with-temp-buffer
    (insert "Flash-Emacs Clean Interface Demo
=====================================

NEW IMPROVEMENTS:
✓ Simplified prompts: Shows only your search pattern
✓ Silent jumping: No more 'Jumped to: xxx' messages
✓ Clean, distraction-free experience

BEFORE vs AFTER:
Before: Flash [test]: 
After:  test

Before: Jumped to: content
After:  (silent jump)

SAMPLE TEXT FOR TESTING:
========================

test testing tester tests testament
apple banana cherry date elderberry
hello world example text content
function variable method class object
search pattern match label jump
quick brown fox jumps over lazy dog

INSTRUCTIONS:
=============

1. Press C-c j to start flash jump
2. Notice the clean prompt format:
   - Empty: '' (empty)
   - Pattern 'te': 'te'
   - Pattern 'test': 'test'

3. Type characters to build your search pattern
4. Use labels to jump (notice silent jumping)
5. Try different patterns to see the clean interface

FEATURES TO TEST:
================

• Multi-character patterns: 'te' → 'tes' → 'test'
• Conflict avoidance: 'te' + 'a' = search, 'te' + 'd' = jump
• Uppercase labels: Search for common letters to see A-Z labels
• Multi-window: Open multiple windows and search across them
• Silent jumping: No messages when you jump to matches

Ready to try the clean interface!
")
    (goto-char (point-min))
    
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    
    (message "Clean interface demo ready! Press C-c j to experience the improved UI.")))

(defun demo-prompt-comparison ()
  "Show a comparison of old vs new prompt formats."
  (interactive)
  (message "=== Flash-Emacs Prompt Comparison ===")
  (sit-for 1)
  
  (message "OLD FORMAT:")
  (sit-for 1)
  (message "Flash: ")
  (sit-for 1)
  (message "Flash [t]: ")
  (sit-for 1)
  (message "Flash [te]: ")
  (sit-for 1)
  (message "Flash [test]: ")
  (sit-for 2)
  
  (message "NEW FORMAT:")
  (sit-for 1)
  (message "'' (empty)")
  (sit-for 1)
  (message "t")
  (sit-for 1)
  (message "te")
  (sit-for 1)
  (message "test")
  (sit-for 2)
  
  (message "Much cleaner! Try the real thing with C-c j"))

(provide 'demo-clean-interface)

;;; demo-clean-interface.el ends here 