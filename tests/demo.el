;;; demo.el --- Interactive demo for flash-emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple interactive demo to showcase flash-emacs functionality

;;; Code:

(load-file "flash-emacs.el")

(defun flash-emacs-demo ()
  "Run an interactive demo of flash-emacs."
  (interactive)
  
  ;; Create demo buffer
  (let ((demo-buffer (get-buffer-create "*Flash Emacs Demo*")))
    (with-current-buffer demo-buffer
      (erase-buffer)
      (insert "Welcome to Flash Emacs Demo!

This buffer contains various text patterns for testing flash-emacs functionality.

Common words to search for:
- the, and, or, but, if, when, while, for, in, on, at, to, from
- function, variable, buffer, window, point, mark, region

Programming keywords:
- defun, defvar, defcustom, let, lambda, progn, cond, when, unless
- class, method, function, return, import, export, const, var

Sample code:
(defun example-function (arg1 arg2)
  \"Example function for testing.\"
  (let ((result (+ arg1 arg2)))
    (message \"Result: %d\" result)
    result))

(defvar example-variable 42
  \"Example variable for testing.\")

More test content:
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco.

Repeated patterns:
test test test test test
hello hello hello hello
world world world world

Special characters and symbols:
@#$%^&*()[]{}|\\:;\"'<>,.?/
function() { return true; }
array[index] = value;

URLs and emails:
https://example.com
mailto:test@example.com
ftp://files.example.org

Numbers and dates:
123 456 789 2024-01-01 12:34:56

End of demo content.

INSTRUCTIONS:
1. Press C-c j (or M-x flash-emacs-jump) to start flash jump mode
2. Type a search pattern (try 'test', 'def', 'the', etc.)
3. Use the red labels (a, s, d, f, g, h, etc.) to jump to any match
4. Press ESC to cancel or Enter to jump to first match
5. Use Backspace to edit your search pattern

Try searching for:
- 'test' - will show multiple matches with labels
- 'def' - will find function definitions
- 'the' - will find common word occurrences
- 'example' - will find example-related text
- '(' - will find opening parentheses
")
      (goto-char (point-min))
      (emacs-lisp-mode))
    
    ;; Switch to demo buffer and enable flash-emacs-mode
    (switch-to-buffer demo-buffer)
    (flash-emacs-mode 1)
    
    (message "Flash Emacs Demo ready! Press C-c j to start jumping, or try the suggestions above.")))

;; Auto-run demo when loaded interactively
(when (called-interactively-p 'any)
  (flash-emacs-demo))

(provide 'demo)

;;; demo.el ends here 