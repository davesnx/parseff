#!/bin/bash
set -euo pipefail

# Build release profile (fast — incremental)
opam exec -- dune build bench/bench_quick.exe --profile=release 2>&1 | tail -5

# Run benchmark
opam exec -- dune exec bench/bench_quick.exe --profile=release --no-print-directory 2>&1
