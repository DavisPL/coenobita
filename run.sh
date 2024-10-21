#!/bin/sh

# Install the Coenobita binary
./install.sh

# Find the sample project
cd ../a

# Clean and check
cargo clean
cargo check