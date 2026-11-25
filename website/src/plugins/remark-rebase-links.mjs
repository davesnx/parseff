import { existsSync } from 'node:fs';
import { dirname, relative, resolve, sep } from 'node:path';
import { visit } from 'unist-util-visit';
import { contentDir, root } from '../../site.mjs';

const VERSION_SEGMENT = /^(?:main|\d+\.\d+\.\d+)(?:[/?#]|$)/;
const PAGE_LINK = /^(\.{1,2}\/[^?#]*?)(\.mdx?)(#.*)?$/;

// odoc links pages by file path, such as ./errors.md or ../guides/x.md#a.
// Starlight serves each page at a route, so the path resolves to that route.
function pageRoute(url, file) {
  if (url.startsWith('./#')) return url.slice(2);

  const match = PAGE_LINK.exec(url);
  if (!match || !file) return undefined;

  const [, path, extension, anchor = ''] = match;
  const target = resolve(dirname(file), path);
  const slug = relative(contentDir, target).split(sep).join('/');
  if (slug.startsWith('../') || !existsSync(target + extension)) {
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
    visit(tree, ['link', 'definition', 'image'], (node) => {
      if (typeof node.url !== 'string') return;

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
