// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightThemeFlexoki from 'starlight-theme-flexoki';
import remarkBoldAsides from './src/plugins/remark-bold-asides.mjs';
import remarkRebaseLinks from './src/plugins/remark-rebase-links.mjs';
import opengraphImage from 'astro-opengraph-image';
import { readFile, writeFile, readdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';
import { base, channel, contentDir, root, version } from './site.mjs';

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

const pageExists = (slug) =>
	['md', 'mdx'].some((ext) =>
		['', '/index'].some((suffix) => existsSync(join(contentDir, `${slug}${suffix}.${ext}`))),
	);

/** Drop sidebar entries whose page is missing from the content being built. */
function existingPages(items) {
	return items.flatMap((item) => {
		if (item.items) {
			const kept = existingPages(item.items);
			return kept.length ? [{ ...item, items: kept }] : [];
		}
		return pageExists(item.slug) ? [item] : [];
	});
}

// https://astro.build/config
export default defineConfig({
	site: 'https://davesnx.github.io',
	base,
	vite: {
		define: {
			__DOCS_ROOT__: JSON.stringify(root),
			__DOCS_VERSION__: JSON.stringify(version),
			__DOCS_CHANNEL__: JSON.stringify(channel),
		},
	},
	markdown: {
		remarkPlugins: [remarkBoldAsides, [remarkRebaseLinks, { base }]],
	},
	integrations: [
		starlight({
			plugins: [starlightThemeFlexoki()],
			components: {
				Banner: './src/components/VersionBanner.astro',
				Head: './src/components/Head.astro',
				PageSidebar: './src/components/PageSidebar.astro',
				PageTitle: './src/components/PageTitle.astro',
				ThemeSelect: './src/components/VersionSelect.astro',
			},
			title: 'Parseff',
			head:
				channel === 'latest'
					? []
					: [{ tag: 'meta', attrs: { name: 'robots', content: 'noindex' } }],
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
			sidebar: existingPages([
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
			{ label: 'Changelog', slug: 'changelog' },
			{ label: 'Credits', slug: 'credits' },
			]),
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
		fixOgImageBase(base),
	],
});
