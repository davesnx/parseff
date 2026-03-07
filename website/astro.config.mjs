// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightThemeFlexoki from 'starlight-theme-flexoki';
import remarkBoldAsides from './src/plugins/remark-bold-asides.mjs';
import opengraphImage from 'astro-opengraph-image';
import { readFile, writeFile, readdir } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';

/** Wrap an integration so that any routes it injects are forced to prerender.
 *  This avoids the "no adapter installed" warning for purely-static sites. */
function staticOnly(integration) {
	const orig = integration.hooks['astro:config:setup'];
	if (orig) {
		integration.hooks['astro:config:setup'] = (options) => {
			const realInject = options.injectRoute;
			options.injectRoute = (route) => realInject({ ...route, prerender: true });
			return orig(options);
		};
	}
	return integration;
}

/** Fix og:image URLs when using a base path (astro-opengraph-image doesn't account for it). */
function fixOgImageBase(base) {
	return {
		name: 'fix-og-image-base',
		hooks: {
			async 'astro:build:done'({ dir }) {
				if (!base || base === '/') return;
				const prefix = base.replace(/\/$/, '');
				const dirPath = fileURLToPath(dir);
				const files = await readdir(dirPath, { recursive: true });
				for (const file of files) {
					if (!file.endsWith('.html')) continue;
					const filePath = join(dirPath, file);
					const html = await readFile(filePath, 'utf-8');
					const fixed = html.replaceAll('/_og/', `${prefix}/_og/`);
					if (fixed !== html) {
						await writeFile(filePath, fixed);
					}
				}
			},
		},
	};
}

// https://astro.build/config
export default defineConfig({
	site: 'https://davesnx.github.io',
	base: '/parseff',
	markdown: {
		remarkPlugins: [remarkBoldAsides],
	},
	integrations: [
		starlight({
			plugins: [starlightThemeFlexoki()],
			components: {
				Head: './src/components/Head.astro',
				PageSidebar: './src/components/PageSidebar.astro',
				PageTitle: './src/components/PageTitle.astro',
			},
			title: 'Parseff',
			description: 'Parser combinators with OCaml 5 algebraic effects',
			social: [
				{
					icon: 'github',
					label: 'GitHub',
					href: 'https://github.com/davesnx/parseff',
				},
			],
			customCss: [
				'./src/styles/custom.css',
			],
			sidebar: [
				{ label: 'Quick start', slug: 'index' },
				{ label: 'Primitives', slug: 'api/primitives' },
				{ label: 'Combinators', slug: 'api/combinators' },
				{ label: 'Repetition and separation', slug: 'api/repetition' },
				{ label: 'Convenience', slug: 'api/convenience' },
				{ label: 'Error handling', slug: 'api/errors' },
				{ label: 'Diagnostics', slug: 'api/diagnostics' },
				{ label: 'Zero-copy and fused operations', slug: 'api/zero-copy' },
				{ label: 'Streaming', slug: 'api/streaming' },
				{
					label: 'Guides',
					items: [
						{ label: 'Your first parser', slug: 'guides/first-parser' },
						{ label: 'Making parsers fast', slug: 'guides/optimization' },
						{ label: 'Parsing an IP address', slug: 'guides/ip-address' },
						{ label: 'A JSON parser', slug: 'guides/json-parser' },
						{ label: 'Expressions with precedence', slug: 'guides/expression-parser' },
					],
				},
				{ label: 'Comparison with Angstrom', slug: 'guides/comparison' },
				{ label: 'Credits', slug: 'credits' },
			],
		}),
		staticOnly(opengraphImage({
			background: '#101014',
			width: 1200,
			height: 630,
			scale: 3,
			fonts: [
				{
					name: 'Instrument Sans',
					data: await readFile(
						'node_modules/@fontsource/instrument-sans/files/instrument-sans-latin-400-normal.woff',
					),
					style: 'normal',
					weight: 400,
				},
			{
				name: 'Instrument Sans',
				data: await readFile(
					'node_modules/@fontsource/instrument-sans/files/instrument-sans-latin-700-normal.woff',
				),
				style: 'normal',
				weight: 700,
			},
			{
				name: 'IBM Plex Mono',
				data: await readFile(
					'node_modules/@fontsource/ibm-plex-mono/files/ibm-plex-mono-latin-400-normal.woff',
				),
				style: 'normal',
				weight: 400,
			},
		],
		})),
		fixOgImageBase('/parseff'),
	],
});
