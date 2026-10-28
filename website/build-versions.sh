#!/usr/bin/env bash

set -euo pipefail

MIN_VERSION=0.4.0
ROOT_BASE=/parseff

here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/.." && pwd)"
out="$(realpath -m "${1:-$here/dist}")"
work="$(mktemp -d "$here/.build.XXXXXX")"
trap 'rm -rf "$work"' EXIT

die() { echo "build-versions: $*" >&2; exit 1; }

build() {
  local content="$1" base="$2" version="$3" channel="$4" dest="$5"
  local shell="$work/shell"
  rm -rf "$shell"
  mkdir -p "$shell/src/content"
  tar -C "$here" --exclude=./node_modules --exclude=./dist --exclude=./.astro \
    --exclude='./.build.*' --exclude=./src/content -cf - . | tar -C "$shell" -xf -
  cp -a "$content" "$shell/src/content/docs"
  (
    cd "$shell"
    DOCS_BASE="$base" DOCS_VERSION="$version" DOCS_CHANNEL="$channel" \
      npx astro build
  )
  mkdir -p "$dest"
  cp -a "$shell/dist/." "$dest/"
  [ -f "$dest/pagefind/pagefind.js" ] || die "no search index in $dest"
}

tags=()
while IFS= read -r tag; do
  [ "$(printf '%s\n%s\n' "$MIN_VERSION" "$tag" | sort -V | head -1)" = "$MIN_VERSION" ] && tags+=("$tag")
done < <(git -C "$repo" tag --list '[0-9]*.[0-9]*.[0-9]*' --sort=-v:refname)
[ "${#tags[@]}" -gt 0 ] || die "no tag >= $MIN_VERSION"
latest="${tags[0]}"

rm -rf "$out"
mkdir -p "$out"

for tag in "${tags[@]}"; do
  mkdir -p "$work/src-$tag"
  git -C "$repo" archive "$tag" website/src/content/docs | tar -C "$work/src-$tag" -xf -
  content="$work/src-$tag/website/src/content/docs"
  if [ "$tag" = "$latest" ]; then
    build "$content" "$ROOT_BASE" "$tag" latest "$out"
    channel=latest
  else
    channel=old
  fi
  build "$content" "$ROOT_BASE/$tag" "$tag" "$channel" "$out/$tag"
done

build "$here/src/content/docs" "$ROOT_BASE/main" main unreleased "$out/main"

touch "$out/.nojekyll"
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
