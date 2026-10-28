import { visit } from 'unist-util-visit';

const PREFIX = '/parseff/';

export default function remarkRebaseLinks({ base }) {
  const target = `${base.replace(/\/$/, '')}/`;
  return (tree) => {
    if (target === PREFIX) return;
    visit(tree, ['link', 'definition'], (node) => {
      if (node.url.startsWith(PREFIX)) node.url = target + node.url.slice(PREFIX.length);
    });
  };
}
