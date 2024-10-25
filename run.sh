#!/bin/sh

# Install the Coenobita binary
./install.sh

# Find the sample project
cd tests/a

# Clean and check
cargo clean
cargo check