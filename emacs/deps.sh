#!/bin/sh

# Update the package list
pkg update

# Check and install OpenJDK for Java development (change to your desired version)
if ! pkg info | grep -q openjdk; then
    echo "Installing OpenJDK..."
    pkg install -y openjdk17
fi

# Check and install npm, necessary for installing bash-language-server
if ! pkg info | grep -q npm; then
    echo "Installing npm..."
    pkg install -y npm
fi

# Install bash-language-server via npm if not already installed
if ! npm list -g | grep -q bash-language-server; then
    echo "Installing bash-language-server..."
    npm install -g bash-language-server
fi

# Check and install git if necessary
if ! pkg info | grep -q git; then
    echo "Installing git..."
    pkg install -y git
fi

# Check and install LLVM (includes clang)
if ! pkg info | grep -q llvm; then
    echo "Installing LLVM..."
    pkg install -y llvm
fi

# Check and install Go
if ! pkg info | grep -q go; then
    echo "Installing Go..."
    pkg install -y go
fi

# Install gopls, the Go language server
# This assumes Go is already installed and $GOPATH is set
if ! command -v gopls >/dev/null 2>&1; then
    echo "Installing gopls..."
    go install golang.org/x/tools/gopls@latest
fi
