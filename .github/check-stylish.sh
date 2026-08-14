#!/usr/bin/env bash

set -euo pipefail

# NUL-delimited so paths with spaces or newlines survive; an unquoted command
# substitution here would word-split them into separate arguments.
mapfile -d '' files < <(git ls-files -z -- '*.hs')

if [[ ${#files[@]} -gt 0 ]]; then
  stylish-haskell -i "${files[@]}"
fi

git diff --exit-code
