# justfile

pkg     := "region-cursors"
emacs   := env_var_or_default("EMACS", "emacs")
files	:= `find . -maxdepth 1 -name '*.el' ! -name '*-autoloads.el' | sort | tr '\n' ' '`
batch   := emacs + " -Q --batch -L ."

# Cache for lint dependencies
lint_dir := justfile_directory() + "/.elpa-lint"

@_default:
    just --list

# Build everything
build: compile autoloads

# Byte-compile all .el files
compile:
    @{{batch}} --eval "(setq byte-compile-error-on-warn t)" \
        -f batch-byte-compile {{files}}

# Generate autoloads file
autoloads:
    @{{batch}} --eval "(require 'loaddefs-gen)" \
        --eval "(loaddefs-generate default-directory \
                  (expand-file-name \"{{pkg}}-autoloads.el\"))"

# Verify declare-function correctness
check:
    @{{batch}} --eval "(check-declare-directory default-directory)"

# Check documentation strings
checkdoc:
    #!/usr/bin/env bash
    set -euo pipefail
    status=0
    for f in {{files}}; do
        out=$({{batch}} --eval "(checkdoc-file \"$f\")" 2>&1 || true)
        if [ -n "$out" ]; then
            printf '%s\n' "$out"
            status=1
        fi
    done
    if [ "$status" -eq 0 ]; then echo "checkdoc: clean"; fi
    exit "$status"

# Lint all .el files
lint:
    @{{batch}} \
        --eval "(setq package-user-dir \"{{lint_dir}}\")" \
        --eval "(require 'package)" \
        --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
        --eval "(package-initialize)" \
        --eval "(unless (package-installed-p 'package-lint) \
                  (package-refresh-contents) \
                  (package-install 'package-lint))" \
        --eval "(require 'package-lint)" \
        --eval "(setq package-lint-main-file \"{{pkg}}.el\")" \
        -f package-lint-batch-and-exit {{files}}

# Wipe build artifacts & lint dependency cache
clean:
    @rm -f *.elc {{pkg}}-autoloads.el
    @rm -rf {{lint_dir}}
