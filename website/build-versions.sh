#!/usr/bin/env bash

set -euo pipefail

MIN_VERSION=0.4.0

here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/.." && pwd)"
root="$(cd "$here" && node --input-type=module -e "import { root } from './site.mjs'; console.log(root)")"
out="$(realpath -m "${1:-$here/dist}")"
work="$(mktemp -d "$here/.build.XXXXXX")"
trap 'rm -rf "$work"' EXIT

die() { echo "build-versions: $*" >&2; exit 1; }

[ -n "$out" ] && [ "$out" != / ] || die "refusing to clear output path '$out'"

build() {
  local content="$1" base="$2" version="$3" channel="$4" dest="$5"
  rm -rf "$here/.astro"
  (
    cd "$here"
    DOCS_CONTENT_DIR="$content" DOCS_BASE="$base" DOCS_VERSION="$version" DOCS_CHANNEL="$channel" \
      npx astro build --outDir "$dest"
  )
  [ -f "$dest/pagefind/pagefind.js" ] || die "no search index in $dest"
}

tags=()
while IFS= read -r tag; do
  [ "$(printf '%s\n%s\n' "$MIN_VERSION" "$tag" | sort -V | head -1)" = "$MIN_VERSION" ] && tags+=("$tag")
done < <(git -C "$repo" tag --list '[0-9]*.[0-9]*.[0-9]*' --sort=-v:refname | grep -v -- '-' || true)
[ "${#tags[@]}" -gt 0 ] || die "no tag >= $MIN_VERSION"
latest="${tags[0]}"

rm -rf "$out"
mkdir -p "$out"

for tag in "${tags[@]}"; do
  mkdir -p "$work/src-$tag"
  git -C "$repo" archive "$tag" website/src/content/docs | tar -C "$work/src-$tag" -xf -
  content="$work/src-$tag/website/src/content/docs"
  if [ "$tag" = "$latest" ]; then
    build "$content" "$root" "$tag" latest "$out"
    channel=alias
  else
    channel=old
  fi
  build "$content" "$root/$tag" "$tag" "$channel" "$out/$tag"
done

build "$here/src/content/docs" "$root/main" main unreleased "$out/main"

node -e '
const [latest, ...tags] = process.argv.slice(1);
const entries = [
  { version: "main", path: "main/", label: "main (unreleased)" },
  ...tags.map((version) =>
    version === latest
      ? { version, path: "", label: `${version} (latest)` }
      : { version, path: `${version}/`, label: version },
  ),
];
process.stdout.write(JSON.stringify(entries) + "\n");
' "$latest" "${tags[@]}" > "$out/versions.json"
