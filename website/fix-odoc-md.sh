#!/bin/sh
set -e

FILE="$1"

sedi() {
  sed "$1" "$2" > "$2.sedtmp" && mv "$2.sedtmp" "$2"
}

sedi 's|\[`\([^`]*\)`\](\./Parseff[^)]*)|`\1`|g' "$FILE"
sedi 's|\\+|+|g' "$FILE"
sedi 's|\\=|=|g' "$FILE"
"$(dirname "$0")/fix-code-spans.sh" "$FILE"

awk '
BEGIN { in_code = 0; blank_count = 0; after_fence = 0; prev_list = 0 }
/^```/ {
    if (in_code) {
        in_code = 0
        after_fence = 1
    } else {
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
    if (after_fence && blank_count == 0) {
        print ""
    } else if (blank_count > 0) {
        print ""
    }
    if (prev_list && !/^- / && !/^[0-9]+\. / && !/^  /) {
        print ""
    }
    blank_count = 0
    after_fence = 0
    prev_list = (/^- / || /^[0-9]+\. /)
    print
}
' "$FILE" > "$FILE.tmp" && mv "$FILE.tmp" "$FILE"
