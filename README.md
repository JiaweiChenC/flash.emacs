# Flash-Emacs

Flash.nvim-like jump navigation for Emacs with full evil-mode support.


<!-- ### Key Features -->

<!-- - **Multi-character search patterns** with progressive narrowing -->
<!-- - **Smart label assignment** prioritizing current window and distance   -->
<!-- - **Intelligent conflict avoidance** between search and jump labels -->
<!-- - **Window-aware search** - only shows matches in visible content -->
<!-- - **Multi-window support** for searching across visible windows -->
<!-- - **Evil-mode integration** with visual selection support -->
<!-- - **Visual feedback** with highlighted matches and overlay labels -->
<!-- - **Incremental search** with real-time updates -->
<!-- - **Customizable appearance** and behavior -->

## Installation

## demo 

![emacs flash demo](flash-emacs-demo.gif)

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

### Multi-window Navigation

Flash-emacs works across all visible windows:

1. Split windows with different buffers
2. Use flash-emacs from any window
3. See matches labeled in all windows
4. Jump to any visible location instantly

## Comparison with flash.nvim

Flash-emacs provides feature parity with flash.nvim:

| Feature | flash.nvim | flash-emacs |
|---------|------------|-------------|
| Multi-character search | ✓ | ✓ |
| Smart label assignment | ✓ | ✓ |
| Conflict avoidance | ✓ | ✓ |
| Window-aware search | ✓ | ✓ |
| Visual mode support | ✓ | ✓ |
| Operator-pending mode | ✓ | ✓ |
| Multi-window search | ✓ | ✓ |
| Incremental search | ✓ | ✓ |
| Customizable labels | ✓ | ✓ |

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.

## Acknowledgments

- Inspired by [flash.nvim](https://github.com/folke/flash.nvim) by folke

