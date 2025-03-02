#!/bin/sh

# Build the standard library wrapper (both release and debug)
cd library/std && cargo build --release && cargo build

# Install the Coenobita binary
cd ../../ && cargo install --path compiler/coenobita-bin --force