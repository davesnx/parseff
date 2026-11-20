import { visit } from 'unist-util-visit';
import { root } from '../../site.mjs';

const VERSION_SEGMENT = /^(?:main|\d+\.\d+\.\d+)(?:[/?#]|$)/;

export default function remarkRebaseLinks({ base, site }) {
  const from = `${root}/`;
  const to = `${base.replace(/\/$/, '')}/`;
  const origin = site ? site.replace(/\/$/, '') : '';
  const siteRoot = origin ? `${origin}${root}` : '';

  return (tree) => {
    visit(tree, ['link', 'definition', 'image'], (node) => {
      if (typeof node.url !== 'string') return;

      let url = node.url;
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
