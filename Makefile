# Makefile for flash-emacs

EMACS ?= emacs
PACKAGE_NAME = flash-emacs
VERSION = 1.0.0

# Source files
SOURCES = flash-emacs.el
OBJECTS = $(SOURCES:.el=.elc)

# Test files
TEST_FILES = tests/test-functionality.el \
             tests/test-multichar.el \
             tests/test-label-conflicts.el \
             tests/test-complete-functionality.el

# Package files
PACKAGE_FILES = $(SOURCES) \
                flash-emacs-pkg.el \
                flash-emacs-autoloads.el \
                README.md \
                LICENSE \
                CHANGELOG.md

.PHONY: all compile test clean package install autoloads help

# Default target
all: compile autoloads

# Help target
help:
	@echo "Available targets:"
	@echo "  compile    - Byte-compile Emacs Lisp files"
	@echo "  test       - Run all tests"
	@echo "  clean      - Remove compiled files"
	@echo "  package    - Create package archive"
	@echo "  install    - Install package locally"
	@echo "  autoloads  - Generate autoloads file"
	@echo "  help       - Show this help message"

# Compile Emacs Lisp files
compile: $(OBJECTS)

%.elc: %.el
	$(EMACS) -batch -f batch-byte-compile $<

# Run tests
test:
	@echo "Running flash-emacs tests..."
	@for test_file in $(TEST_FILES); do \
		echo "Running $$test_file..."; \
		$(EMACS) --batch -l $$test_file || exit 1; \
	done
	@echo "All tests passed!"

# Generate autoloads
autoloads:
	$(EMACS) --batch \
		--eval "(require 'package)" \
		--eval "(package-generate-autoloads \"$(PACKAGE_NAME)\" \".\")"

# Clean compiled files
clean:
	rm -f *.elc tests/*.elc

# Create package archive
package: clean
	@echo "Creating package archive..."
	mkdir -p dist
	tar -czf dist/$(PACKAGE_NAME)-$(VERSION).tar.gz \
		--transform 's,^,$(PACKAGE_NAME)-$(VERSION)/,' \
		$(PACKAGE_FILES)
	@echo "Package created: dist/$(PACKAGE_NAME)-$(VERSION).tar.gz"

# Install package locally
install: compile
	$(EMACS) --batch \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"local\" . \"$(PWD)\"))" \
		--eval "(package-initialize)" \
		--eval "(package-install-file \"$(PWD)\")"

# Development targets
dev-install:
	$(EMACS) --batch \
		--eval "(add-to-list 'load-path \"$(PWD)\")" \
		--eval "(require 'flash-emacs)" \
		--eval "(message \"flash-emacs loaded successfully\")"

# Check package
check:
	$(EMACS) --batch \
		--eval "(require 'package)" \
		--eval "(package-lint-batch-and-exit)" \
		$(SOURCES)

# Format code (if you have elisp-format)
format:
	@if command -v elisp-format >/dev/null 2>&1; then \
		elisp-format $(SOURCES); \
	else \
		echo "elisp-format not found. Install it for code formatting."; \
	fi

# Lint code (if you have package-lint)
lint:
	@if $(EMACS) --batch --eval "(require 'package-lint)" 2>/dev/null; then \
		$(EMACS) --batch \
			--eval "(require 'package-lint)" \
			--eval "(package-lint-batch-and-exit)" \
			$(SOURCES); \
	else \
		echo "package-lint not available. Install it for linting."; \
	fi 