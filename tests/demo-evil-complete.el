;;; demo-evil-complete.el --- Complete evil-mode integration demo -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive demonstration of flash-emacs evil-mode integration

;;; Code:

(require 'cl-lib)
(load-file "flash-emacs.el")

(defun demo-evil-complete ()
  "Complete interactive demo of evil-mode integration."
  (interactive)
  (with-temp-buffer
    (insert "# Flash-Emacs Evil-mode Integration Demo

Welcome to the complete flash-emacs evil-mode integration demo!

This buffer demonstrates all the features that make flash-emacs work
seamlessly with evil-mode, providing the same experience as flash.nvim.

## Test Content

Here's some sample text to practice with:

### Programming Examples
function calculateSum(a, b) {
    return a + b;
}

const numbers = [1, 2, 3, 4, 5];
const doubled = numbers.map(x => x * 2);

### Natural Language
The quick brown fox jumps over the lazy dog.
Pack my box with five dozen liquor jugs.
How vexingly quick daft zebras jump!

### Mixed Content
TODO: Implement feature X
FIXME: Bug in function Y  
NOTE: Remember to update docs
WARNING: This code needs review

## Evil-mode Integration Features

### 1. Visual Mode Support

Flash-emacs automatically detects when you're in evil visual mode:

- Character visual (v): Extends selection character by character
- Line visual (V): Extends selection line by line  
- Block visual (C-v): Extends rectangular selection

### 2. Operator Integration

Works with all evil operators:
- Delete (d): d + flash-emacs = delete to target
- Change (c): c + flash-emacs = change to target
- Yank (y): y + flash-emacs = yank to target
- And more!

### 3. Smart Behavior

- Preserves visual selection type
- Handles multi-window scenarios
- Configurable extension behavior
- Seamless state transitions

## Try These Scenarios

### Scenario 1: Character Visual Selection
1. Position cursor at start of 'function' (line 9)
2. Press 'v' to enter character visual mode
3. Press 's' (or your flash-emacs binding)
4. Type 'ret' to search for 'return'
5. Press the label to extend selection to 'return'
6. Result: 'function calculateSum(a, b) {\\n    return' selected

### Scenario 2: Line Visual Selection  
1. Position cursor anywhere on line 9
2. Press 'V' to enter line visual mode
3. Press 's' (flash-emacs)
4. Type 'const' to search for 'const'
5. Press the label to extend to that line
6. Result: Multiple lines selected from function to const

### Scenario 3: Operator + Flash
1. Position cursor at start of 'function'
2. Press 'd' (delete operator)
3. Press 's' (flash-emacs)
4. Type 'num' to search for 'numbers'
5. Press the label to delete from cursor to target
6. Result: Text deleted from 'function' to 'numbers'

### Scenario 4: Block Visual Selection
1. Position cursor at start of 'TODO:' (line 20)
2. Press 'C-v' to enter block visual mode
3. Move down a few lines to create a block
4. Press 's' (flash-emacs)
5. Type 'WAR' to search for 'WARNING'
6. Press the label to extend block to target

### Scenario 5: Multi-window Navigation
1. Split window: C-x 2 or C-x 3
2. Switch between windows: C-x o
3. Use flash-emacs from any window
4. See matches in all visible windows
5. Jump to any window seamlessly

## Configuration Examples

### Basic Evil Integration
```elisp
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd \"s\") #'flash-emacs-jump)
  (define-key evil-visual-state-map (kbd \"s\") #'flash-emacs-jump)
  (define-key evil-operator-state-map (kbd \"s\") #'flash-emacs-jump))
```

### Visual Selection Behavior
```elisp
;; Extend selection when jumping (default, flash.nvim-like)
(setq flash-emacs-evil-visual-extend t)

;; Exit visual mode when jumping (traditional vim-like)
(setq flash-emacs-evil-visual-extend nil)
```

### Advanced Configuration
```elisp
;; Customize for your workflow
(setq flash-emacs-labels \"asdfghjklqwertyuiopzxcvbnm\"
      flash-emacs-uppercase-labels t
      flash-emacs-multi-window t
      flash-emacs-case-sensitive nil
      flash-emacs-evil-visual-extend t)
```

## Key Bindings for This Demo

- C-c j: flash-emacs-jump (works in all modes)
- C-c t: Test evil integration functions
- C-c i: Show current evil state info
- C-c r: Reset buffer to original state

## Testing Commands

Use these to verify the integration:

- M-x flash-emacs--test-evil-integration
- M-x flash-emacs--evil-available-p
- M-x flash-emacs--evil-visual-state-p

## Tips for Best Experience

1. **Practice the patterns**: Start with simple 2-3 character searches
2. **Use visual mode**: Try all three visual modes (v, V, C-v)
3. **Combine with operators**: Practice d/c/y + flash-emacs
4. **Multi-window**: Split windows and see cross-window jumping
5. **Customize labels**: Adjust labels to match your typing preferences

## Troubleshooting

If something doesn't work as expected:

1. Check if evil-mode is loaded: M-x evil-mode
2. Verify key bindings: C-h k s (should show flash-emacs-jump)
3. Test evil state detection: C-c t
4. Check configuration: C-h v flash-emacs-evil-visual-extend

## What Makes This Special

Flash-emacs provides the same experience as flash.nvim:

✓ Multi-character search with progressive narrowing
✓ Smart label assignment avoiding conflicts  
✓ Visual mode integration with selection extension
✓ Operator-pending mode support
✓ Multi-window navigation
✓ Configurable behavior matching user preferences

This is a complete implementation that brings flash.nvim's power to Emacs!

---

Happy jumping! 🚀
")
    
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    
    ;; Set up key bindings for the demo
    (local-set-key (kbd "C-c j") #'flash-emacs-jump)
    (local-set-key (kbd "C-c t") #'flash-emacs--test-evil-integration)
    (local-set-key (kbd "C-c i") (lambda () 
                                   (interactive)
                                   (message "Evil state: %s | Visual: %s | Type: %s" 
                                           (if (boundp 'evil-state) evil-state "N/A")
                                           (flash-emacs--evil-visual-state-p)
                                           (flash-emacs--evil-visual-type))))
    (local-set-key (kbd "C-c r") (lambda () 
                                   (interactive)
                                   (demo-evil-complete)))
    
    ;; Enable markdown-like highlighting if available
    (when (fboundp 'markdown-mode)
      (markdown-mode))
    
    (message "Evil-mode integration demo ready! Use C-c j for flash-emacs, C-c t for tests, C-c i for state info.")))

(defun demo-evil-scenarios ()
  "Run through specific evil-mode scenarios."
  (interactive)
  (let ((scenarios '(
    "Scenario 1: Character Visual (v + flash-emacs)"
    "Scenario 2: Line Visual (V + flash-emacs)"  
    "Scenario 3: Operator Pending (d + flash-emacs)"
    "Scenario 4: Block Visual (C-v + flash-emacs)"
    "Scenario 5: Multi-window Navigation")))
    
    (dolist (scenario scenarios)
      (message "%s" scenario)
      (sit-for 1))
    
    (message "Try each scenario in the demo buffer!")))

(defun demo-evil-configuration ()
  "Show configuration examples for evil integration."
  (interactive)
  (with-temp-buffer
    (insert ";; Flash-Emacs Evil-mode Configuration Examples

;; Basic setup - add to your init.el
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd \"s\") #'flash-emacs-jump)
  (define-key evil-visual-state-map (kbd \"s\") #'flash-emacs-jump)
  (define-key evil-operator-state-map (kbd \"s\") #'flash-emacs-jump))

;; Alternative key bindings
(with-eval-after-load 'evil
  ;; Use 'f' for flash (replaces evil-find-char)
  (define-key evil-normal-state-map (kbd \"f\") #'flash-emacs-jump)
  (define-key evil-visual-state-map (kbd \"f\") #'flash-emacs-jump)
  (define-key evil-operator-state-map (kbd \"f\") #'flash-emacs-jump)
  
  ;; Keep original 'f' on a different key
  (define-key evil-normal-state-map (kbd \"F\") #'evil-find-char))

;; Visual selection behavior
(setq flash-emacs-evil-visual-extend t)    ; Extend selection (default)
;; (setq flash-emacs-evil-visual-extend nil)  ; Exit visual mode

;; Complete configuration
(setq flash-emacs-labels \"asdfghjklqwertyuiopzxcvbnm\"
      flash-emacs-uppercase-labels t
      flash-emacs-search-mode 'exact
      flash-emacs-multi-window t
      flash-emacs-case-sensitive nil
      flash-emacs-min-pattern-length 1
      flash-emacs-max-matches 100
      flash-emacs-evil-visual-extend t)

;; Face customization
(set-face-attribute 'flash-emacs-label nil
                    :background \"#ff007c\" 
                    :foreground \"#c8d3f5\"
                    :weight 'bold)

(set-face-attribute 'flash-emacs-match nil
                    :background \"#3e68d7\" 
                    :foreground \"#c8d3f5\")

;; Use-package configuration
(use-package flash-emacs
  :load-path \"path/to/flash-emacs\"
  :config
  (setq flash-emacs-evil-visual-extend t)
  
  :bind
  (:map evil-normal-state-map
   (\"s\" . flash-emacs-jump))
  (:map evil-visual-state-map
   (\"s\" . flash-emacs-jump))
  (:map evil-operator-state-map
   (\"s\" . flash-emacs-jump)))
")
    
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (message "Evil-mode configuration examples ready!")))

;; Auto-run the main demo
(demo-evil-complete)

(provide 'demo-evil-complete)

;;; demo-evil-complete.el ends here 