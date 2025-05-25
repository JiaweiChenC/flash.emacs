# Flash-Emacs Tests and Demos

This directory contains all test files, demo scripts, and debugging utilities for flash-emacs.

## Test Files

### Core Functionality Tests
- **`test-basic-functionality.el`** - Basic functionality tests (search, labels, jumping)
- **`test-integration-complete.el`** - Complete integration test covering all features
- **`test-complete-functionality.el`** - Comprehensive functionality verification

### Evil-mode Integration Tests
- **`test-evil-integration.el`** - Evil-mode integration tests with mocked functions
- **`test-visual-operator-issues.el`** - Tests for visual mode and operator state issues
- **`test-detailed-jump.el`** - Detailed jump behavior verification
- **`test-simple-issues.el`** - Simple tests for visual mode issues

### Feature-Specific Tests
- **`test-multichar.el`** - Multi-character search pattern tests
- **`test-label-conflicts.el`** - Label conflict avoidance tests
- **`test-window-bounds.el`** - Window bounds functionality tests
- **`test-uppercase-labels.el`** - Uppercase label functionality tests

### Legacy Tests
- **`test-flash-emacs.el`** - Original test file
- **`test-functionality.el`** - Early functionality tests
- **`test-clean-startup.el`** - Clean startup tests
- **`test-prompt-changes.el`** - Prompt behavior tests
- **`test-message-fix.el`** - Message handling tests

## Demo Files

### Interactive Demos
- **`demo-complete.el`** - Complete feature demonstration
- **`demo-evil-complete.el`** - Complete evil-mode integration demo
- **`demo-window-bounds.el`** - Window bounds feature demo
- **`demo-conflict-resolution.el`** - Label conflict resolution demo
- **`demo-clean-interface.el`** - Clean interface demonstration
- **`demo.el`** - Basic demo script

## Debug and Verification Files

### Debugging Utilities
- **`debug-loading.el`** - Debug loading issues
- **`debug-search.el`** - Debug search functionality

### Verification Scripts
- **`verify-fix.el`** - Verify specific fixes
- **`verify-multichar.el`** - Verify multi-character functionality

## Running Tests

### Individual Tests
```bash
# Run a specific test
emacs --batch -l tests/test-basic-functionality.el

# Run integration test
emacs --batch -l tests/test-integration-complete.el
```

### Interactive Demos
```bash
# Run complete demo
emacs -l tests/demo-complete.el

# Run evil-mode demo
emacs -l tests/demo-evil-complete.el
```

### All Tests
```bash
# Run all basic tests
for test in tests/test-*.el; do
    echo "Running $test..."
    emacs --batch -l "$test"
done
```

## Test Categories

### ✅ Passing Tests
- All basic functionality tests
- Evil-mode integration tests
- Multi-character search tests
- Window bounds tests
- Label conflict avoidance tests

### 🔧 Development Tests
- Debug utilities for troubleshooting
- Verification scripts for specific features

### 🎯 Demo Scripts
- Interactive demonstrations of features
- Complete workflow examples
- Evil-mode integration examples

## Notes

- All tests use mocked evil-mode functions for batch testing
- Demo files are designed for interactive use
- Debug files help troubleshoot specific issues
- Tests verify flash-emacs matches flash.nvim behavior exactly 