/**
 * Remark plugin that converts paragraphs starting with **Label:** into
 * Starlight-compatible container directives (:::tip, :::caution, :::danger).
 *
 * This bridges odoc's {b Label:} markup with Starlight's admonition system.
 * odoc renders {b Label:} as **Label:** in markdown. This plugin detects
 * that pattern and wraps the paragraph in a container directive that
 * Starlight's built-in remarkAsides plugin renders as a styled aside.
 *
 * Mapping:
 *   **Performance:**  -> :::tip[Performance]
 *   **Tip:**          -> :::tip
 *   **Caution:**      -> :::caution
 *   **Warning:**      -> :::danger
 *   **Danger:**       -> :::danger
 *   **Note:**         -> :::note
 *
 * Any **Label:** not in the mapping is left unchanged.
 *
 * Usage in .mld files:
 *   {b Performance:} [consume] is optimized for literal string matching.
 *
 * Becomes in promoted markdown:
 *   **Performance:** `consume` is optimized for literal string matching.
 *
 * Which this plugin transforms into the directive AST equivalent of:
 *   :::tip[Performance]
 *   `consume` is optimized for literal string matching.
 *   :::
 */

import { visit } from 'unist-util-visit';

/** Map bold labels to Starlight aside variants. */
const LABEL_TO_VARIANT = {
  // Direct variant names
  'Tip': 'tip',
  'Note': 'note',
  'Caution': 'caution',
  'Warning': 'danger',
  'Danger': 'danger',
  // Semantic labels → tip (informational)
  'Performance': 'tip',
  'Performance Critical': 'danger',
};

export default function remarkBoldAsides() {
  return (tree) => {
    visit(tree, 'paragraph', (node, index, parent) => {
      if (!parent || index === undefined) return;

      const children = node.children;
      if (children.length === 0) return;

      // Check if the paragraph starts with a <strong> node
      const first = children[0];
      if (first.type !== 'strong' || first.children.length === 0) return;

      // Extract the bold text
      const boldText = first.children
        .filter((c) => c.type === 'text')
        .map((c) => c.value)
        .join('');

      // Match "Label:" or "Label" pattern
      const match = boldText.match(/^(.+?):?$/);
      if (!match) return;

      const label = match[1];
      const variant = LABEL_TO_VARIANT[label];
      if (!variant) return;

      // Build the remaining content (everything after the **Label:** and
      // any leading whitespace in the next text node)
      const remainingChildren = children.slice(1);
      if (
        remainingChildren.length > 0 &&
        remainingChildren[0].type === 'text'
      ) {
        // Strip leading space after the bold label
        remainingChildren[0] = {
          ...remainingChildren[0],
          value: remainingChildren[0].value.replace(/^ /, ''),
        };
      }

      // If the label is a direct variant name (Tip, Note, etc.), don't
      // repeat it as the title — let Starlight use its default.
      // For semantic labels (Performance, etc.), use the label as title.
      const isDirectVariant = ['Tip', 'Note', 'Caution', 'Warning', 'Danger'].includes(label);

      // Build a containerDirective node that Starlight's remarkAsides
      // plugin will pick up.
      const directive = {
        type: 'containerDirective',
        name: variant,
        attributes: {},
        children: [],
        data: {
          hName: 'div',
        },
      };

      // Add a directive label if we have a custom title
      if (!isDirectVariant) {
        directive.children.push({
          type: 'paragraph',
          children: [{ type: 'text', value: label }],
          data: { directiveLabel: true },
        });
      }

      // Add the content paragraph
      if (remainingChildren.length > 0) {
        directive.children.push({
          type: 'paragraph',
          children: remainingChildren,
        });
      }

      // Replace the original paragraph with the directive
      parent.children[index] = directive;
    });
  };
}
