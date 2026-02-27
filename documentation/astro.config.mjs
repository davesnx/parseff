// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightThemeFlexoki from 'starlight-theme-flexoki';

// https://astro.build/config
export default defineConfig({
	site: 'https://davesnx.github.io',
	base: '/parseff',
	integrations: [
		starlight({
			plugins: [starlightThemeFlexoki()],
			components: {
				Head: './src/components/Head.astro',
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
				{
					label: 'Documentation',
					items: [
						{ label: 'Introduction', slug: 'index' },
						{ label: 'Quick Start', slug: 'quick-start' },
						{ label: 'Your First Parser', slug: 'guides/first-parser' },
					],
				},
				{
					label: 'API Reference',
					items: [
						{ label: 'Overview', slug: 'api/overview' },
						{ label: 'Core', slug: 'api/primitives' },
						{ label: 'Combinators', slug: 'api/combinators' },
						{ label: 'Repetition & Separation', slug: 'api/repetition' },
						{ label: 'Convenience', slug: 'api/convenience' },
						{ label: 'Zero-Copy & Fused Ops', slug: 'api/zero-copy' },
						{ label: 'Streaming', slug: 'api/streaming' },
					],
				},
				{
					label: 'Guides',
					items: [
						{ label: 'Error Handling', slug: 'guides/errors' },
						{ label: 'Error Accumulation', slug: 'guides/error-accumulation' },
						{ label: 'Making Parsers Fast', slug: 'guides/optimization' },
					{ label: 'Comparison with Angstrom', slug: 'guides/comparison' },
					{ label: 'Credits', slug: 'guides/credits' },
				],
				},
				{
					label: 'Examples',
					items: [
						{ label: 'Parsing an IP Address', slug: 'examples/ip-address' },
						{ label: 'A JSON Parser', slug: 'examples/json-parser' },
						{ label: 'Expressions with Precedence', slug: 'examples/expression-parser' },
						{ label: 'Zero-Copy Performance', slug: 'examples/zero-copy-performance' },
					],
				},
			],
		}),
	],
});
