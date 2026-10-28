#!/usr/bin/env bash

set -euo pipefail

MIN_VERSION="${MIN_VERSION:-0.4.0}"
ROOT_BASE="/parseff"

here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/.." && pwd)"
out="$(realpath -m "${1:-$here/dist}")"
work="$(mktemp -d "${TMPDIR:-/tmp}/parseff-docs.XXXXXX")"
trap 'rm -rf "$work"' EXIT

die() { echo "build-versions: $*" >&2; exit 1; }

patch() {
  local file="$1" from="$2" to="$3"
  FROM="$from" TO="$to" perl -0pi -e '
    BEGIN { $n = 0 }
    $n += s/\Q$ENV{FROM}\E/$ENV{TO}/g;
    END { $? = $n ? 0 : 1 }
  ' "$file" || die "pattern not found in $file: $from"
}

build() {
  local dir="$1" base="$2" version="$3" channel="$4" dest="$5"
  (
    cd "$dir"
    DOCS_BASE="$base" DOCS_VERSION="$version" DOCS_CHANNEL="$channel" \
      npx astro build --outDir "$dest"
  )
  [ -f "$dest/pagefind/pagefind.js" ] || die "no search index in $dest"
}

patch_legacy() {
  local dir="$1" base="$2" channel="$3"
  cp "$here"/src/components/VersionSelect.astro "$here"/src/components/VersionBanner.astro \
    "$dir/src/components/"
  patch "$dir/astro.config.mjs" "base: '/parseff'," "base: '$base',"
  patch "$dir/astro.config.mjs" "fixOgImageBase('/parseff')" "fixOgImageBase('$base')"
  patch "$dir/astro.config.mjs" "Head: './src/components/Head.astro'," \
    "Banner: './src/components/VersionBanner.astro', Head: './src/components/Head.astro', ThemeSelect: './src/components/VersionSelect.astro',"
  if [ "$channel" != latest ]; then
    patch "$dir/astro.config.mjs" "title: 'Parseff'," \
      "title: 'Parseff', head: [{ tag: 'meta', attrs: { name: 'robots', content: 'noindex' } }],"
  fi
  if grep -rlF '](/parseff/' "$dir/src/content/docs" > "$work/links"; then
    while IFS= read -r file; do patch "$file" '](/parseff/' "]($base/"; done < "$work/links"
  fi
}

build_tag() {
  local tag="$1" base="$2" channel="$3" dest="$4" dir="$5"
  if grep -qF 'DOCS_BASE' "$dir/astro.config.mjs"; then
    build "$dir" "$base" "$tag" "$channel" "$dest"
  else
    patch_legacy "$dir" "$base" "$channel"
    build "$dir" "$base" "$tag" "$channel" "$dest"
  fi
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
  src="$work/$tag"
  mkdir -p "$src"
  git -C "$repo" archive "$tag" website | tar -x -C "$src"
  site="$src/website"
  (cd "$site" && npm ci)
  if [ "$tag" = "$latest" ]; then
    cp -a "$site" "$work/root-site"
    build_tag "$tag" "$ROOT_BASE" latest "$work/dist-root" "$work/root-site"
    channel=latest
  else
    channel=old
  fi
  build_tag "$tag" "$ROOT_BASE/$tag" "$channel" "$work/dist-$tag" "$site"
done

build "$here" "$ROOT_BASE/main" main unreleased "$work/dist-main"

cp -a "$work/dist-root/." "$out/"
for tag in "${tags[@]}"; do
  mkdir -p "$out/$tag"
  cp -a "$work/dist-$tag/." "$out/$tag/"
  rm -f "$out/$tag/versions.json"
done
mkdir -p "$out/main"
cp -a "$work/dist-main/." "$out/main/"
rm -f "$out/main/versions.json"
touch "$out/.nojekyll"

{
  printf '[{"version":"main","path":"main/","label":"main (unreleased)"}'
  for tag in "${tags[@]}"; do
    if [ "$tag" = "$latest" ]; then
      printf ',{"version":"%s","path":"","label":"%s (latest)"}' "$tag" "$tag"
    else
      printf ',{"version":"%s","path":"%s/","label":"%s"}' "$tag" "$tag" "$tag"
    fi
  done
  printf ']\n'
} > "$out/versions.json"
