import { visit } from 'unist-util-visit';

const LABEL_TO_VARIANT = {
  'Tip': 'tip',
  'Note': 'note',
  'Caution': 'caution',
  'Warning': 'danger',
  'Danger': 'danger',
  'Performance': 'tip',
  'Performance Critical': 'danger',
};

export default function remarkBoldAsides() {
  return (tree) => {
    visit(tree, 'paragraph', (node, index, parent) => {
      if (!parent || index === undefined) return;

      const children = node.children;
      if (children.length === 0) return;

      const first = children[0];
      if (first.type !== 'strong' || first.children.length === 0) return;

      const boldText = first.children
        .filter((c) => c.type === 'text')
        .map((c) => c.value)
        .join('');

      const match = boldText.match(/^(.+?):?$/);
      if (!match) return;

      const label = match[1];
      const variant = LABEL_TO_VARIANT[label];
      if (!variant) return;

      const remainingChildren = children.slice(1);
      if (
        remainingChildren.length > 0 &&
        remainingChildren[0].type === 'text'
      ) {
        remainingChildren[0] = {
          ...remainingChildren[0],
          value: remainingChildren[0].value.replace(/^ /, ''),
        };
      }

      const isDirectVariant = ['Tip', 'Note', 'Caution', 'Warning', 'Danger'].includes(label);

      const directive = {
        type: 'containerDirective',
        name: variant,
        attributes: {},
        children: [],
        data: {
          hName: 'div',
        },
      };

      if (!isDirectVariant) {
        directive.children.push({
          type: 'paragraph',
          children: [{ type: 'text', value: label }],
          data: { directiveLabel: true },
        });
      }

      if (remainingChildren.length > 0) {
        directive.children.push({
          type: 'paragraph',
          children: remainingChildren,
        });
      }

      parent.children[index] = directive;
    });
  };
}
