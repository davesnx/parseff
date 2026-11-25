import { existsSync, readdirSync } from 'node:fs';
import { basename, dirname, join, relative, resolve, sep } from 'node:path';
import { SKIP, visit } from 'unist-util-visit';
import { contentDir, root } from '../../site.mjs';

const VERSION_SEGMENT = /^(?:main|\d+\.\d+\.\d+)(?:[/?#]|$)/;
const PAGE_LINK = /^(\.{1,2}\/[^?#]*?)(\.mdx?)(#.*)?$/;
const MODULE_PAGE = /^\.\/Parseff(?:-[^/?#]+)?\.md(?:#|$)/;

// The content file called name, when exactly one file has that name.
function pageNamed(name) {
  const files = readdirSync(contentDir, { recursive: true }).filter((f) => basename(f) === name);
  return files.length === 1 ? join(contentDir, files[0]) : undefined;
}

// odoc links pages by file path, such as ./errors.md#a. It writes every page of
// the package to one directory, while the website splits them into api/ and
// guides/, so a ./name.md that is not next to the linking page is found by
// name. Starlight serves each page at a route, so the path resolves to that
// route.
function pageRoute(url, file) {
  const match = PAGE_LINK.exec(url);
  if (!match || !file) return undefined;

  const [, path, extension, anchor = ''] = match;
  let target = resolve(dirname(file), path) + extension;
  if (!existsSync(target) && /^\.\/[^/]+$/.test(path)) {
    target = pageNamed(basename(target)) ?? target;
  }
  const slug = relative(contentDir, target.slice(0, -extension.length)).split(sep).join('/');
  if (slug.startsWith('../') || !existsSync(target)) {
    console.warn(`remark-rebase-links: ${file}: no page for ${url}`);
    return undefined;
  }

  const page = slug === 'index' ? '' : `${slug.replace(/\/index$/, '')}/`;
  return `${root}/${page}${anchor}`;
}

export default function remarkRebaseLinks({ base, site }) {
  const from = `${root}/`;
  const to = `${base.replace(/\/$/, '')}/`;
  const origin = site ? site.replace(/\/$/, '') : '';
  const siteRoot = origin ? `${origin}${root}` : '';

  return (tree, file) => {
    visit(tree, ['link', 'definition', 'image'], (node, index, parent) => {
      if (typeof node.url !== 'string') return;

      // The website has no module pages, so a link to one becomes its text.
      if (node.type === 'link' && parent && MODULE_PAGE.test(node.url)) {
        parent.children.splice(index, 1, ...node.children);
        return [SKIP, index];
      }

      let url = pageRoute(node.url, file.path) ?? node.url;
      if (siteRoot && (url === siteRoot || url.startsWith(`${siteRoot}/`))) {
        url = url.slice(origin.length);
        if (url === root) url = from;
      }

      if (to !== from && url.startsWith(from)) {
        const rest = url.slice(from.length);
        if (!VERSION_SEGMENT.test(rest)) {
          url = to + rest;
        }
      }

      node.url = url;
    });
  };
}
