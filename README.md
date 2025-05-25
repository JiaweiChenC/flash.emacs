# Flash-Emacs

An Emacs implementation of flash.nvim's standalone jump functionality, providing fast and intuitive navigation within buffers using incremental search and smart label assignment.

## Features

- **Multi-character search patterns**: Type multiple characters to narrow down matches progressively
- **Smart label assignment**: Labels are assigned based on distance from cursor and window priority
- **Uppercase labels support**: After lowercase labels are exhausted, uppercase letters are used
- **Multi-window support**: Search and jump across all visible windows (configurable)
- **Visual feedback**: Highlighted matches with overlay labels
- **Incremental search**: Real-time updates as you type
- **Intelligent conflict avoidance**: Prevents label conflicts with search pattern continuation
- **Clean interface**: Simplified prompts showing only the search pattern and silent jumping
- **Customizable appearance**: Configurable faces and label characters

## Installation

### Package Repositories

Flash-emacs is available from the following package repositories:

#### Manual Installation

1. Clone or download this repository:
   ```bash
   git clone https://github.com/flash-emacs/flash-emacs.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/flash-emacs")
   (require 'flash-emacs)
   (global-set-key (kbd "C-c j") #'flash-emacs-jump)  ; Bind to preferred key
   ```

#### use-package

```elisp
(use-package flash-emacs
  :ensure t
  :bind ("C-c j" . flash-emacs-jump))
```

#### straight.el

```elisp
(straight-use-package
 '(flash-emacs :type git :host github :repo "flash-emacs/flash-emacs"))
(global-set-key (kbd "C-c j") #'flash-emacs-jump)
```

## Quick Start

1. **Install the package** using one of the methods above
2. **Bind to a key**: `(global-set-key (kbd "C-c j") #'flash-emacs-jump)`
3. **Start jumping**: Press `C-c j` or run `M-x flash-emacs-jump`
4. **Type to search**: Enter characters to build your search pattern
5. **Jump**: Type a label character to jump to that match

## How It Works

### Multi-Character Search
Flash-emacs supports building search patterns with multiple characters, just like flash.nvim:

1. **Pattern Building**: Type characters to build your search pattern
   - `t` → finds all words starting with 't'
   - `te` → narrows to words starting with 'te'
   - `tes` → further narrows to words starting with 'tes'
   - `test` → very specific matches for 'test'

2. **Label Assignment**: Once matches are found, labels are assigned to each match
3. **Jump Execution**: Type a label character to jump to that match

### Smart Label Assignment

Flash-emacs uses an intelligent label assignment system:

1. **Lowercase first**: Initial matches get lowercase labels: `a`, `s`, `d`, `f`, `g`, etc.
2. **Uppercase when needed**: When lowercase labels are exhausted, uppercase letters are used: `A`, `S`, `D`, `F`, `G`, etc.
3. **Priority order**: Labels are assigned based on distance from cursor and window priority
4. **Conflict avoidance**: Labels that would conflict with search continuation are automatically excluded

### Example Workflow

```
Buffer content: "test testing tester tests testament tea team"

1. Press C-c j (start flash jump)
2. Type 'te'
   - Prompt shows: "te: "
   - Matches: test, testing, tester, tests, testament, tea, team
   - Labels assigned: d, f, g, h, j, k, l (note: 'a' and 's' excluded)
   - Why excluded? Because 'tea' and 'tes' exist in text
3. Type 'd' → jumps silently to first match with label 'd'
   OR
   Type 'a' → searches for 'tea' (extends pattern, prompt shows "tea: ")

With many matches:
1. First 26 matches get lowercase: a, s, d, f, g, h, j, k, l, q, w, e, r, t, y, u, i, o, p, z, x, c, v, b, n, m
2. Additional matches get uppercase: A, S, D, F, G, H, J, K, L, Q, W, E, R, T, Y, U, I, O, P, Z, X, C, V, B, N, M
```

## Usage

### Basic Usage

1. **Start flash jump**: `C-c j` (or `M-x flash-emacs-jump`)
2. **Type search pattern**: Enter characters to build your search
3. **Jump to match**: Type a label character to jump to that position
4. **Cancel**: Press `C-g` or `ESC` to cancel

### Customization

```elisp
;; Customize label characters (default: "asdfghjklqwertyuiopzxcvbnm")
(setq flash-emacs-labels "asdfghjkl")

;; Enable/disable uppercase labels (default: t)
(setq flash-emacs-uppercase-labels nil)

;; Enable/disable multi-window search (default: t)
(setq flash-emacs-multi-window nil)

;; Customize faces
(set-face-attribute 'flash-emacs-match nil 
                    :background "yellow" :foreground "black")
(set-face-attribute 'flash-emacs-label nil 
                    :background "red" :foreground "white" :weight 'bold)

;; Case sensitivity (default: nil)
(setq flash-emacs-case-sensitive t)

;; Minimum pattern length before showing labels (default: 1)
(setq flash-emacs-min-pattern-length 2)
```

