#!/usr/bin/env bash
# Generates website/src/content/docs/changelog.md from the root CHANGES.md.
# Each "## <version>" heading becomes a link to the GitHub release tag.

set -euo pipefail

REPO_URL="https://github.com/davesnx/parseff"
INPUT="$(dirname "$0")/../CHANGES.md"
OUTPUT="$(dirname "$0")/src/content/docs/changelog.md"

{
  cat <<'FRONTMATTER'
---
title: Changelog
description: Release history and changes for Parseff
---
FRONTMATTER

  echo ""
  echo "All notable changes to Parseff. See [all releases on GitHub](${REPO_URL}/releases)."
  echo ""

  # Transform "## <version>" lines into linked headings, skip "Unreleased"
  skip=0
  while IFS= read -r line; do
    if [[ "$line" =~ ^##[[:space:]]+(.*) ]]; then
      version="${BASH_REMATCH[1]}"
      if [[ "${version,,}" == *unreleased* ]]; then
        skip=1
        continue
      fi
      skip=0
      echo "## [${version}](${REPO_URL}/releases/tag/${version})"
    else
      if [[ "$skip" -eq 0 ]]; then
        echo "$line"
      fi
    fi
  done < "$INPUT"
} > "$OUTPUT"
