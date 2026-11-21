#!/bin/sh
set -e

FILE="$1"
SECTION="$2"
DOC_DIR="$3"

case "$SECTION" in
  api)
    OTHER_DIR="$DOC_DIR/guides"
    TARGET_PREFIX="../guides"
    ;;
  guides)
    OTHER_DIR="$DOC_DIR"
    TARGET_PREFIX="../api"
    ;;
  *)
    echo "fix-relative-links.sh: expected section 'api' or 'guides', got '$SECTION'" >&2
    exit 1
    ;;
esac

perl -i -pe '
  BEGIN {
    our %other = map {
      s{.*/}{}; s{\.mld$}{.md}; $_ => 1
    } glob("$ARGV[0]/*.mld");
    our $prefix = $ARGV[1];
    splice @ARGV, 0, 2;
  }
  if (/^```/) { $fence = !$fence; next }
  next if $fence;
  s{\(\./([^/#?)]+\.md)(#[^)]*)?\)}{
    $other{$1} ? "($prefix/$1" . ($2 // "") . ")" : $&
  }ge;
' "$OTHER_DIR" "$TARGET_PREFIX" "$FILE"
