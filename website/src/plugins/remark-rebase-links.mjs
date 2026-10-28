import { visit } from 'unist-util-visit';
import { root } from '../../site.mjs';

export default function remarkRebaseLinks({ base }) {
  const from = `${root}/`;
  const to = `${base.replace(/\/$/, '')}/`;
  return (tree) => {
    if (to === from) return;
    visit(tree, ['link', 'definition', 'image'], (node) => {
      if (node.url.startsWith(from)) node.url = to + node.url.slice(from.length);
    });
  };
}
