#!/bin/sh

# Build the standard library wrapper
cd library/std && cargo build

# Install the Coenobita binary
cd ../../ && cargo install --path compiler/coenobita-bin --force