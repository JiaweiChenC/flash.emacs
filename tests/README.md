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

# Flash-Emacs Tests

This directory contains comprehensive tests and demos for flash-emacs functionality.

## Test Categories

### Core Functionality Tests
- `test-basic-functionality.el` - Basic search and jump functionality
- `test-complete-functionality.el` - Complete feature integration test
- `test-integration-complete.el` - Full integration testing

### Multi-Character Search Tests
- `test-multichar.el` - Multi-character search pattern tests
- `verify-multichar.el` - Verification of multi-character behavior
- `demo-multichar.el` - Interactive multi-character demo

### Label Conflict Resolution Tests
- `test-label-conflicts.el` - Label conflict avoidance tests
- `demo-conflict-resolution.el` - Interactive conflict resolution demo

### Distance-Based Label Assignment Tests
- `test-distance-priority.el` - Basic distance-based sorting tests
- `test-distance-priority-corrected.el` - Corrected distance sorting tests
- `debug-distance-calculation.el` - Debug distance calculation
- `demo-cursor-centered.el` - Demo cursor-centered label assignment
- `demo-cursor-positions.el` - Demo label assignment at different cursor positions

### Evil-Mode Integration Tests
- `test-evil-integration.el` - Evil-mode integration tests
- `demo-evil-complete.el` - Complete evil-mode demo

### Visual Mode and Operator Tests
- `test-visual-operator-issues.el` - Visual mode and operator state tests
- `test-simple-issues.el` - Simple visual mode issue tests
- `test-detailed-jump.el` - Detailed jump behavior tests

### Window Bounds Tests
- `test-window-bounds.el` - Window bounds functionality tests
- `demo-window-bounds.el` - Interactive window bounds demo

### Debug and Development Files
- `debug-loading.el` - Debug loading issues
- `debug-search.el` - Debug search functionality
- `test-message-fix.el` - Message handling fixes
- `verify-fix.el` - Verification of fixes
- `demo-clean-interface.el` - Clean interface demo
- `demo-complete.el` - Complete functionality demo

## Key Features Tested

### 1. Multi-Character Search
- Progressive pattern building: "t" → "te" → "tes" → "test"
- Smart jump detection: distinguishes between search continuation and jump labels
- Real-time match narrowing with visual feedback

### 2. Label Conflict Avoidance
- Intelligent filtering of conflicting labels
- Pattern analysis: for "te", filters out 'a' and 's' if "tea" and "tes" exist
- Ensures unambiguous jump behavior

### 3. Cursor-Centered Label Assignment
- **Distance-based prioritization**: Labels assigned based on 2D distance from cursor
- **Position fallback**: When distances are equal, earlier position wins
- **Adaptive behavior**: Label assignment changes based on cursor position
- **Flash.nvim compatibility**: Uses same distance calculation algorithm

### 4. Evil-Mode Integration
- Character visual mode (v) support
- Line visual mode (V) support  
- Block visual mode (C-v) support
- Operator-pending mode support
- Configurable selection extension behavior

### 5. Window Bounds Awareness
- Only visible content gets labels
- Improved performance by limiting search scope
- Clearer visual feedback

### 6. Visual Mode Positioning
- Correct cursor positioning at match locations
- Automatic selection extension in evil visual modes
- Proper mark ring integration

## Running Tests

### Run All Tests
```bash
# From the flash.emacs directory
for test in tests/test-*.el; do
    echo "Running $test..."
    emacs --batch -l "$test"
done
```

### Run Specific Test Categories
```bash
# Multi-character tests
emacs --batch -l tests/test-multichar.el

# Distance-based label assignment
emacs --batch -l tests/demo-cursor-centered.el
emacs --batch -l tests/demo-cursor-positions.el

# Evil-mode integration
emacs --batch -l tests/test-evil-integration.el

# Window bounds
emacs --batch -l tests/test-window-bounds.el
```

### Interactive Demos
```bash
# Complete functionality demo
emacs --batch -l tests/demo-complete.el

# Conflict resolution demo
emacs --batch -l tests/demo-conflict-resolution.el

# Cursor-centered label assignment
emacs --batch -l tests/demo-cursor-centered.el
```

## Test Results Summary

All tests demonstrate that flash-emacs successfully replicates flash.nvim's behavior:

✓ **Multi-character search**: Progressive narrowing works correctly  
✓ **Smart jump detection**: Unambiguous label vs search distinction  
✓ **Conflict avoidance**: No label conflicts with search patterns  
✓ **Cursor-centered labels**: Distance-based prioritization implemented  
✓ **Evil-mode integration**: All visual modes and operators supported  
✓ **Window bounds**: Only visible content labeled  
✓ **Visual positioning**: Correct cursor placement and selection extension  

The implementation achieves feature parity with flash.nvim while maintaining Emacs-native behavior and integration. 