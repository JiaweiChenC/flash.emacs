;;; test-flash-emacs.el --- Test file for flash-emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains test content and functions to test flash-emacs functionality.

;;; Code:

;; Load the flash-emacs package
(load-file "flash-emacs.el")

;; Test content with various patterns to search for
(defvar test-content
  "This is a test file for flash-emacs functionality.

The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.

Some test patterns:
- function definitions: defun, defvar, defcustom
- common words: the, and, or, but, if, when, while
- symbols: (, ), [, ], {, }
- numbers: 123, 456, 789

More test content:
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco.

Code examples:
(defun example-function (arg1 arg2)
  \"Example function for testing.\"
  (let ((result (+ arg1 arg2)))
    (message \"Result: %d\" result)
    result))

(defvar example-variable 42
  \"Example variable for testing.\")

(defcustom example-custom-var \"hello\"
  \"Example custom variable.\"
  :type 'string
  :group 'example)

More patterns to test:
- Repeated words: test test test
- Mixed case: Test TEST tEsT
- Special chars: @#$%^&*()
- URLs: https://example.com
- Emails: test@example.com

End of test content.")

(defun create-test-buffer ()
  "Create a test buffer with sample content."
  (interactive)
  (let ((buffer (get-buffer-create "*flash-emacs-test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert test-content)
      (goto-char (point-min))
      (emacs-lisp-mode))
    (switch-to-buffer buffer)
    (message "Test buffer created. Try M-x flash-emacs-jump or C-c j")))

(defun test-flash-emacs-basic ()
  "Basic test of flash-emacs functionality."
  (interactive)
  (create-test-buffer)
  (message "Test buffer created. Now you can:
1. Run M-x flash-emacs-jump (or C-c j if flash-emacs-mode is enabled)
2. Type 'the' to search for 'the'
3. Use the displayed labels (a, s, d, f, etc.) to jump
4. Press ESC to cancel, Enter to jump to first match
5. Use backspace to delete characters from search"))

(defun test-flash-emacs-search-function ()
  "Test the search function directly."
  (interactive)
  (create-test-buffer)
  (let ((pattern "test"))
    (let ((matches (flash-emacs--search-pattern pattern)))
      (message "Found %d matches for '%s'" (length matches) pattern)
      (when matches
        (dolist (match (cl-subseq matches 0 (min 5 (length matches))))
          (message "Match at pos %d: '%s'" 
                   (plist-get match :pos)
                   (plist-get match :text)))))))

(defun test-flash-emacs-labels ()
  "Test label assignment."
  (interactive)
  (create-test-buffer)
  (let* ((pattern "the")
         (matches (flash-emacs--search-pattern pattern))
         (current-window (selected-window))
         (current-point (point)))
    (when matches
      (setq matches (flash-emacs--assign-labels matches flash-emacs-labels
                                               current-window current-point))
      (message "Label assignments:")
      (dolist (match (cl-subseq matches 0 (min 10 (length matches))))
        (message "Position %d gets label '%s'" 
                 (plist-get match :pos)
                 (plist-get match :label))))))

(defun test-flash-emacs-overlays ()
  "Test overlay creation and display."
  (interactive)
  (create-test-buffer)
  (let* ((pattern "test")
         (matches (flash-emacs--search-pattern pattern))
         (current-window (selected-window))
         (current-point (point)))
    (when matches
      (setq matches (flash-emacs--assign-labels matches flash-emacs-labels
                                               current-window current-point))
      (flash-emacs--show-overlays matches)
      (message "Overlays displayed for %d matches. They will be cleared in 5 seconds." 
               (length matches))
      (run-with-timer 5 nil #'flash-emacs--clear-overlays))))

(defun demo-flash-emacs ()
  "Interactive demo of flash-emacs."
  (interactive)
  (create-test-buffer)
  (flash-emacs-mode 1)
  (message "Flash-emacs demo ready!

Try these:
1. C-c j (or M-x flash-emacs-jump) - Start flash jump
2. Type 'def' to find function definitions
3. Type 'the' to find common words
4. Type 'test' to find test occurrences
5. Use the red labels (a,s,d,f,etc.) to jump
6. ESC to cancel, Enter for first match
7. Backspace to edit search

The current buffer has lots of test content to try!"))

;; Enable flash-emacs-mode for easy testing
(defun enable-flash-emacs-test-mode ()
  "Enable flash-emacs mode and set up keybindings for testing."
  (interactive)
  (flash-emacs-mode 1)
  (local-set-key (kbd "C-c t") #'test-flash-emacs-basic)
  (local-set-key (kbd "C-c s") #'test-flash-emacs-search-function)
  (local-set-key (kbd "C-c l") #'test-flash-emacs-labels)
  (local-set-key (kbd "C-c o") #'test-flash-emacs-overlays)
  (local-set-key (kbd "C-c d") #'demo-flash-emacs)
  (message "Flash-emacs test mode enabled. Keybindings:
C-c j - flash-emacs-jump
C-c t - basic test
C-c s - search function test  
C-c l - label test
C-c o - overlay test
C-c d - demo"))

(provide 'test-flash-emacs)

;;; test-flash-emacs.el ends here 