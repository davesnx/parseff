// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightThemeFlexoki from 'starlight-theme-flexoki';
import remarkBoldAsides from './src/plugins/remark-bold-asides.mjs';

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
				'@fontsource/instrument-sans/400.css',
				'@fontsource/instrument-sans/500.css',
				'@fontsource/instrument-sans/600.css',
				'@fontsource/instrument-sans/700.css',
				'@fontsource/ibm-plex-mono/400.css',
				'@fontsource/ibm-plex-mono/500.css',
				'@fontsource/ibm-plex-mono/600.css',
				'./src/styles/custom.css',
			],
			sidebar: [
				{ label: 'Quick start', slug: 'index' },
				{
			label: 'API reference',
				items: [
					{ label: 'Core', slug: 'api/primitives' },
					{ label: 'Combinators', slug: 'api/combinators' },
					{ label: 'Repetition and separation', slug: 'api/repetition' },
					{ label: 'Convenience', slug: 'api/convenience' },
					{ label: 'Error handling', slug: 'api/errors' },
					{ label: 'Diagnostics', slug: 'api/diagnostics' },
					{ label: 'Zero-copy and fused operations', slug: 'api/zero-copy' },
					{ label: 'Streaming', slug: 'api/streaming' },
				],
				},
				{
					label: 'Guides',
					items: [
						{ label: 'Your first parser', slug: 'guides/first-parser' },
						{ label: 'Making parsers fast', slug: 'guides/optimization' },
						{ label: 'Comparison with Angstrom', slug: 'guides/comparison' },
						{ label: 'Parsing an IP address', slug: 'guides/ip-address' },
						{ label: 'A JSON parser', slug: 'guides/json-parser' },
						{ label: 'Expressions with precedence', slug: 'guides/expression-parser' },
					],
				},
				{ label: 'Credits', slug: 'credits' },
			],
		}),
	],
});
