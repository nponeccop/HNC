#!/bin/bash
set -e

# Install stack via apt-get if it's not already available
if ! command -v stack >/dev/null 2>&1; then
  echo "Installing stack..."
  apt-get update
  apt-get install -y haskell-stack
fi

# Build the project using stack
stack update
stack --no-terminal setup
stack build

cat <<'END'
Stack is installed and the project has been built.
END
