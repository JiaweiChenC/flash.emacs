# Flash-Emacs

Flash.nvim-like jump navigation for Emacs with full evil-mode support.

## Overview

Flash-emacs provides fast, accurate jump navigation similar to flash.nvim. Type a search pattern, see labeled matches, then jump instantly using single-character labels.

### Key Features

- **Multi-character search patterns** with progressive narrowing
- **Smart label assignment** prioritizing current window and distance  
- **Intelligent conflict avoidance** between search and jump labels
- **Multi-window support** for searching across visible windows
- **Evil-mode integration** with visual selection support
- **Visual feedback** with highlighted matches and overlay labels
- **Incremental search** with real-time updates
- **Customizable appearance** and behavior

## Installation

### Manual Installation

1. Download `flash-emacs.el` to your Emacs configuration directory
2. Add to your init file:

```elisp
(load-file "path/to/flash-emacs.el")
(require 'flash-emacs)
```

### Basic Key Binding

```elisp
(global-set-key (kbd "C-c j") #'flash-emacs-jump)
```

## Evil-mode Integration

Flash-emacs provides comprehensive evil-mode integration that matches flash.nvim's behavior:

### Setup

```elisp
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "s") #'flash-emacs-jump)
  (define-key evil-visual-state-map (kbd "s") #'flash-emacs-jump)
  (define-key evil-operator-state-map (kbd "s") #'flash-emacs-jump))
```

### Visual Mode Behavior

When in evil visual mode, flash-emacs automatically:

1. **Detects visual state** - Recognizes character, line, and block visual modes
2. **Extends selection** - Jumping extends the selection to the target (configurable)
3. **Preserves visual type** - Maintains character/line/block selection behavior
4. **Handles multi-window** - Works across windows while preserving selection

#### Visual Selection Types

- **Character-wise (`v`)** - Extends selection character by character
- **Line-wise (`V`)** - Extends selection line by line, snapping to line boundaries  
- **Block-wise (`C-v`)** - Extends rectangular selection

### Operator-Pending Mode

Flash-emacs works seamlessly with evil operators:

```
d s [pattern] [label]  ; Delete to flash target
c s [pattern] [label]  ; Change to flash target  
y s [pattern] [label]  ; Yank to flash target
```

### Configuration

```elisp
;; Extend selection when jumping in visual mode (default)
(setq flash-emacs-evil-visual-extend t)

;; Exit visual mode when jumping (vim-like behavior)
(setq flash-emacs-evil-visual-extend nil)
```

## Usage

### Basic Usage

1. Press your flash-emacs key binding (e.g., `C-c j` or `s` in evil-mode)
2. Type characters to search for a pattern
3. See labeled matches appear in real-time
4. Press a label character to jump instantly

### Multi-character Search

Flash-emacs supports progressive search refinement:

```
t      → Shows matches for "t"
te     → Narrows to matches for "te"  
tes    → Further narrows to "tes"
test   → Final pattern "test"
a      → If 'a' is a label, jumps to that match
       → If 'a' is not a label, searches for "testa"
```

### Smart Label Conflict Avoidance

The system intelligently avoids label conflicts:

- For pattern "te", if text contains "tea" and "tes", labels 'a' and 's' are filtered out
- Only "safe" labels that won't conflict with valid search continuations are assigned
- This ensures unambiguous interaction between searching and jumping

## Configuration

### Customization Variables

```elisp
;; Characters used as jump labels
(setq flash-emacs-labels "asdfghjklqwertyuiopzxcvbnm")

;; Use uppercase labels after lowercase ones
(setq flash-emacs-uppercase-labels t)

;; Search mode: 'exact or 'regex
(setq flash-emacs-search-mode 'exact)

;; Search in all visible windows
(setq flash-emacs-multi-window t)

;; Case sensitive search
(setq flash-emacs-case-sensitive nil)

;; Minimum pattern length before showing labels
(setq flash-emacs-min-pattern-length 1)

;; Maximum number of matches to process
(setq flash-emacs-max-matches 100)

;; Evil visual selection behavior
(setq flash-emacs-evil-visual-extend t)
```

### Face Customization

```elisp
;; Customize label appearance
(set-face-attribute 'flash-emacs-label nil
                    :background "red" 
                    :foreground "white" 
                    :weight 'bold)

;; Customize match highlighting
(set-face-attribute 'flash-emacs-match nil
                    :background "yellow" 
                    :foreground "black")

;; Customize current match
(set-face-attribute 'flash-emacs-current nil
                    :background "green" 
                    :foreground "white" 
                    :weight 'bold)
```

## Examples

### Evil-mode Scenarios

#### Character Visual Selection
```
1. Position cursor at start of word
2. Press 'v' to enter character visual mode
3. Press 's' (flash-emacs-jump)
4. Type search pattern
5. Press label to extend selection to target
```

#### Line Visual Selection  
```
1. Position cursor anywhere on line
2. Press 'V' to enter line visual mode
3. Press 's' (flash-emacs-jump)
4. Type search pattern
5. Press label to extend selection to target line
```

#### Operator + Flash
```
1. Press 'd' (delete operator)
2. Press 's' (flash-emacs-jump)  
3. Type search pattern
4. Press label to delete from cursor to target
```

### Multi-window Navigation

Flash-emacs works across all visible windows:

1. Split windows with different buffers
2. Use flash-emacs from any window
3. See matches labeled in all windows
4. Jump to any visible location instantly

## Technical Details

### Architecture

- **State Management** - Tracks search pattern, matches, labels, and evil state
- **Search Engine** - Supports exact and regex matching with conflict avoidance
- **Label Assignment** - Smart prioritization by window and distance
- **Visual Feedback** - Real-time overlays for matches and labels
- **Evil Integration** - Deep integration with evil-mode states and selections

### Performance

- Efficient search algorithms with configurable match limits
- Lazy evaluation and caching for responsive interaction
- Optimized overlay management for smooth visual feedback

### Compatibility

- **Emacs Version** - Requires Emacs 26.1 or later
- **Evil-mode** - Optional but recommended for vim-like experience
- **Dependencies** - Only requires `cl-lib` (built-in)

## Testing

Run the test suite to verify functionality:

```elisp
;; Load and run basic tests
(load-file "test-functionality.el")

;; Test evil-mode integration
(load-file "test-evil-integration.el")

;; Interactive demos
(load-file "demo.el")
M-x demo-evil-integration
```

## Comparison with flash.nvim

Flash-emacs provides feature parity with flash.nvim:

| Feature | flash.nvim | flash-emacs |
|---------|------------|-------------|
| Multi-character search | ✓ | ✓ |
| Smart label assignment | ✓ | ✓ |
| Conflict avoidance | ✓ | ✓ |
| Visual mode support | ✓ | ✓ |
| Operator-pending mode | ✓ | ✓ |
| Multi-window search | ✓ | ✓ |
| Incremental search | ✓ | ✓ |
| Customizable labels | ✓ | ✓ |

## Contributing

Contributions are welcome! Please:

1. Test your changes thoroughly
2. Add tests for new functionality  
3. Update documentation as needed
4. Follow existing code style

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.

## Acknowledgments

- Inspired by [flash.nvim](https://github.com/folke/flash.nvim) by folke
- Built for the Emacs and evil-mode communities

