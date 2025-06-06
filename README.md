# Flash-Emacs

Flash.nvim-like jump navigation for Emacs with full evil-mode support.

## Key Features

- **Multi-character search patterns** with progressive narrowing
- **Smart label assignment** prioritizing current window and distance   
- **Intelligent conflict avoidance** between search and jump labels
- **Window-aware search** - only shows matches in visible content
- **Multi-window support** for searching across visible windows
- **Buffer exclusion** - automatically skips binary, image, and other unsuitable content
- **Smart case sensitivity** - lowercase patterns are case-insensitive, mixed case are case-sensitive
- **Visual feedback** with highlighted matches and overlay labels
- **Incremental search** with real-time updates
- **Clean operation** - doesn't set mark or interfere with Emacs state
- **Customizable appearance** and behavior

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

## Changelog

### v1.1.0 (Latest)

**New Features:**
- **Buffer Exclusion**: Automatically excludes binary files, images, large files, and system buffers from search
- **Smart Case Sensitivity**: Supports Vim-style smart case (lowercase = case-insensitive, mixed case = case-sensitive)
- **Window Bounds Restriction**: Only searches within visible window content
- **Zero Matches Auto-Exit**: Automatically exits when no matches found

**Improvements:**
- **Performance**: Faster search by excluding unsuitable buffers
- **Window Priority Fix**: Better handling when same buffer appears in multiple windows
- **Clean Operation**: Fixed mark-setting issue that caused unwanted "Mark set" messages
- **Better User Experience**: More intuitive behavior and relevant results only

**Bug Fixes:**
- Fixed issue where `goto-line` calls were setting the mark
- Improved window switching logic for better navigation
- Enhanced conflict detection for more accurate label assignment

### v1.0.0 (Initial)

- Basic flash.nvim-like jump navigation
- Multi-character search with progressive narrowing
- Label assignment and conflict avoidance
- Multi-window support
- Customizable labels and appearance

## Acknowledgments

- Inspired by [flash.nvim](https://github.com/folke/flash.nvim) by folke

### Smart Case Sensitivity

Flash-emacs supports smart case sensitivity (similar to Vim's smartcase):

```elisp
;; Configuration options
(setq flash-emacs-case-sensitive 'smart)  ; Default: smart case
;; (setq flash-emacs-case-sensitive nil)     ; Always case-insensitive  
;; (setq flash-emacs-case-sensitive t)       ; Always case-sensitive
```

- **'smart** (default): lowercase patterns are case-insensitive, patterns with uppercase are case-sensitive
- **nil**: always case-insensitive
- **t**: always case-sensitive

### Buffer Exclusion

Flash-emacs automatically excludes unsuitable buffers from search to avoid performance issues and unwanted matches in binary content:

#### Automatically Excluded Content

- **Binary files**: Buffers containing null bytes or high ratios of non-printable characters
- **Image buffers**: Buffers in `image-mode`, `pdf-view-mode`, `doc-view-mode`
- **Archive content**: Buffers in `archive-mode`, `tar-mode`
- **Large files**: Buffers exceeding `flash-emacs-max-buffer-size` (default: 1MB)
- **System buffers**: Hidden buffers (starting with space) and most special buffers (starting with `*`)

#### Customization

```elisp
;; Add custom modes to exclude
(add-to-list 'flash-emacs-exclude-modes 'my-binary-mode)

;; Add custom exclusion function
(defun my-buffer-exclude-p (buffer)
  "Return non-nil if BUFFER should be excluded."
  (with-current-buffer buffer
    (string-match "large-data" (buffer-name))))

(add-to-list 'flash-emacs-exclude-functions 'my-buffer-exclude-p)

;; Adjust size limit (in bytes)
(setq flash-emacs-max-buffer-size 2097152)  ; 2MB
```

#### Benefits

- **Performance**: Avoids searching through large binary files
- **Relevance**: Excludes non-textual content where search results aren't meaningful
- **Stability**: Prevents issues with binary data in search patterns

