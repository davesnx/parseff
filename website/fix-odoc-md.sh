#!/bin/sh
# Post-process odoc-generated markdown for the website.
# Called from dune promote rules in website/src/content/docs/guides/dune.
#
# Usage: fix-odoc-md.sh <file>
#
# Transformations applied:
#  1. Replace odoc cross-reference links with plain code spans
#  2. Unescape \= to = and \_ to _
#  3. Fix backtick-prefixed code spans (``X` -> `` `X ``)
#  4. Add blank line after closing code fences before text
#  5. Add blank line after bullet lists before paragraphs
#  6. Collapse runs of 3+ newlines to exactly 2

FILE="$1"

# 1-2. sed text replacements
sed -i 's|\[`\([^`]*\)`\](\./Parseff[^)]*)|`\1`|g' "$FILE"
sed -i 's|\\=|=|g' "$FILE"
sed -i 's|\\_|_|g' "$FILE"
sed -i 's|\\--|--|g' "$FILE"
sed -i 's|\\+|+|g' "$FILE"
sed -i 's|\\-\\>|→|g' "$FILE"
# 3. Replace placeholder text with proper markdown
sed -i 's|`backslash-quote`|`\\"`|g' "$FILE"
sed -i 's|`backslash-backslash`|`\\\\`|g' "$FILE"
sed -i 's|`LBRACE`|`{`|g' "$FILE"
sed -i 's|`LBRACKET`|`[`|g' "$FILE"
# Tag plain ``` blocks that contain OCaml code with the ocaml language
# (for {v ... v} blocks used to work around odoc code block escaping)
sed -i '/^```$/{N;/^```\nlet /s/^```/```ocaml/;}' "$FILE"
sed -i '/^```$/{N;/^```\n(\*/s/^```/```ocaml/;}' "$FILE"
sed -i '/^```$/{N;/^```\nParseff\./s/^```/```ocaml/;}' "$FILE"
sed -i '/^```$/{N;/^```\nand /s/^```/```ocaml/;}' "$FILE"
# 4. Fix odoc inline code spans that contain backticks:
#    ``X` at end/mid-line -> `` `X `` (double-backtick code span missing spaces)
#    Only match when NOT at start of line (excludes code fences ```)
perl -i -pe 's/(?<=\s)``(?!`)(\S.*?)(?<!`)``/`` `$1 ``/g' "$FILE"
perl -i -pe 's/(?<=\s)``(?!`)(\S+?)`(?!`)/`` `$1 ``/g' "$FILE"

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
