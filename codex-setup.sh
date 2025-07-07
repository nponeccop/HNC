#!/bin/bash
set -e

# Ensure local bin directory exists and is in PATH
STACK_LOCAL_BIN="$HOME/.local/bin"
mkdir -p "$STACK_LOCAL_BIN"
export PATH="$STACK_LOCAL_BIN:$PATH"

# Download stack if it's not installed
if ! command -v stack >/dev/null 2>&1; then
  echo "Downloading stack..."
  curl -L https://www.stackage.org/stack/linux-x86_64 \
    | tar xz --wildcards --strip-components=1 -C "$STACK_LOCAL_BIN" '*/stack'
fi

# Update package index and install GHC if necessary
stack update
stack --no-terminal setup

cat <<'END'
Stack is installed and GHC is set up. You can now install the project dependencies with:
  stack install
END
