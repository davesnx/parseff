#!/bin/bash
set -euo pipefail

# Run tests — must pass for any kept result
opam exec -- dune build @runtest 2>&1 | tail -5
