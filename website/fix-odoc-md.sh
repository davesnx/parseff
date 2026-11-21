#!/bin/sh
# Post-process odoc-generated markdown for the website.
# Called from dune promote rules in website/src/content/docs/guides/dune.
#
# Usage: fix-odoc-md.sh <file>
#
# Transformations applied:
#  1. Replace odoc cross-reference links with plain code spans
#  2. Unescape \+ to +
#  3. Fix code spans that contain backticks (fix-code-spans.sh)
#  4. Add blank line after closing code fences before text
#  5. Add blank line after bullet lists before paragraphs
#  6. Collapse runs of 3+ newlines to exactly 2

set -e

FILE="$1"

# Portable sed -i: BSD sed (macOS) requires -i '' while GNU sed does not.
# Use a temp file to avoid the incompatibility.
sedi() {
  sed "$1" "$2" > "$2.sedtmp" && mv "$2.sedtmp" "$2"
}

# 1-2. sed text replacements
sedi 's|\[`\([^`]*\)`\](\./Parseff[^)]*)|`\1`|g' "$FILE"
sedi 's|\\+|+|g' "$FILE"
"$(dirname "$0")/fix-code-spans.sh" "$FILE"

# 4-6. awk whitespace normalisation
awk '
BEGIN { in_code = 0; blank_count = 0; after_fence = 0; prev_list = 0 }
/^```/ {
    if (in_code) {
        in_code = 0
        after_fence = 1
    } else {
        # Opening a new code fence
        if (after_fence && blank_count == 0) { print "" }
        else if (blank_count > 0) { print "" }
        in_code = 1
        after_fence = 0
    }
    blank_count = 0
    prev_list = 0
    print
    next
}
in_code {
    if (blank_count > 0) { print ""; blank_count = 0 }
    prev_list = 0
    print
    next
}
/^$/ {
    blank_count++
    after_fence = 0
    prev_list = 0
    next
}
{
    # Add blank line after closing code fence
    if (after_fence && blank_count == 0) {
        print ""
    } else if (blank_count > 0) {
        print ""
    }
    # Add blank line after list items when followed by non-list text
    if (prev_list && !/^- / && !/^[0-9]+\. / && !/^  /) {
        print ""
    }
    blank_count = 0
    after_fence = 0
    prev_list = (/^- / || /^[0-9]+\. /)
    print
}
' "$FILE" > "$FILE.tmp" && mv "$FILE.tmp" "$FILE"
