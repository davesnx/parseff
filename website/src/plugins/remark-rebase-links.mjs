import { readdirSync } from 'node:fs';
import { basename, posix, relative, sep } from 'node:path';
import { visit } from 'unist-util-visit';
import { root } from '../../site.mjs';

function indexPageRoutes(dir) {
  const routes = new Map();
  const ambiguous = new Set();
  const walk = (current) => {
    for (const entry of readdirSync(current, { withFileTypes: true })) {
      const full = `${current}/${entry.name}`;
      if (entry.isDirectory()) {
        walk(full);
      } else if (entry.isFile() && /\.mdx?$/.test(entry.name)) {
        const rel = relative(dir, full).split(sep).join(posix.sep);
        const route =
          rel === 'index.md' || rel === 'index.mdx'
            ? ''
            : rel.replace(/\/index\.mdx?$/, '/').replace(/\.mdx?$/, '/');
        const key = basename(entry.name);
        if (routes.has(key)) {
          ambiguous.add(key);
          routes.delete(key);
        } else if (!ambiguous.has(key)) {
          routes.set(key, route);
        }
      }
    }
  };
  walk(dir);
  return routes;
}

export default function remarkRebaseLinks({ base, contentDir, site }) {
  const from = `${root}/`;
  const to = `${base.replace(/\/$/, '')}/`;
  const siteOrigin = site ? site.replace(/\/$/, '') : '';
  const siteRootPrefix = siteOrigin ? `${siteOrigin}${from}` : '';
  const siteRootExact = siteOrigin ? `${siteOrigin}${root}` : '';
  const pageRoutes = contentDir ? indexPageRoutes(contentDir) : new Map();

  return (tree) => {
    visit(tree, ['link', 'definition', 'image'], (node) => {
      if (typeof node.url !== 'string') return;

      if (siteRootPrefix && node.url.startsWith(siteRootPrefix)) {
        node.url = to + node.url.slice(siteRootPrefix.length);
        return;
      }

      if (siteRootExact && node.url === siteRootExact) {
        node.url = to;
        return;
      }

      if (to !== from && node.url.startsWith(from)) {
        node.url = to + node.url.slice(from.length);
        return;
      }

      const mdMatch = node.url.match(/^\.\/([^/#?]+\.mdx?)(#.*)?$/);
      if (mdMatch) {
        const route = pageRoutes.get(mdMatch[1]);
        if (route !== undefined) {
          node.url = `${to}${route}${mdMatch[2] ?? ''}`;
        }
      }
    });
  };
}
