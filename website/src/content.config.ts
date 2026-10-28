import { defineCollection } from 'astro:content';
import { glob } from 'astro/loaders';
import { docsSchema } from '@astrojs/starlight/schema';
import { contentDir } from '../site.mjs';

const extensions = ['markdown', 'mdown', 'mkdn', 'mkd', 'mdwn', 'md', 'mdx'];

export const collections = {
	docs: defineCollection({
		loader: glob({ base: contentDir, pattern: `**/[^_]*.{${extensions.join(',')}}` }),
		schema: docsSchema(),
	}),
};
