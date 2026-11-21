import type { APIRoute, GetStaticPaths } from 'astro';
import { getCollection } from 'astro:content';
import { readFile } from 'node:fs/promises';
import { Resvg } from '@resvg/resvg-js';
import satori from 'satori';

const width = 1200;
const height = 630;
const scale = 3;

const font = (path: string) => readFile(`node_modules/@fontsource/${path}`);

const fonts = [
	{ name: 'Instrument Sans', data: await font('instrument-sans/files/instrument-sans-latin-400-normal.woff'), style: 'normal', weight: 400 },
	{ name: 'Instrument Sans', data: await font('instrument-sans/files/instrument-sans-latin-700-normal.woff'), style: 'normal', weight: 700 },
	{ name: 'IBM Plex Mono', data: await font('ibm-plex-mono/files/ibm-plex-mono-latin-400-normal.woff'), style: 'normal', weight: 400 },
] as const;

const dotSvg = `data:image/svg+xml,${encodeURIComponent(`<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"><circle cx="12" cy="12" r="1" fill="rgba(124,172,248,0.2)"/></svg>`)}`;

type Node = { type: string; props: { style: Record<string, string>; children?: Node[] | string } };

const el = (type: string, style: Record<string, string>, children?: Node[] | string): Node => ({
	type,
	props: { style, children },
});

const truncate = (d: string) => (d.length > 120 ? d.slice(0, 120) + '...' : d);

interface Props {
	title: string;
	description: string;
}

export const getStaticPaths = (async () => {
	const entries = await getCollection('docs');
	const pages = entries.map((entry) => ({
		params: { slug: entry.id },
		props: { title: entry.data.title, description: truncate(entry.data.description ?? '') },
	}));
	if (!entries.some((entry) => entry.id === '404')) {
		pages.push({ params: { slug: '404' }, props: { title: '404', description: '' } });
	}
	return pages;
}) satisfies GetStaticPaths;

export const GET: APIRoute<Props> = async ({ props: { title, description } }) => {
	const tree = el('div', { display: 'flex', flexDirection: 'column', width: '100%', height: '100%' }, [
		el('div', { display: 'flex', position: 'relative', width: '100%', height: '100%' }, [
			el('div', {
				display: 'flex',
				position: 'absolute',
				top: '12px',
				left: '12px',
				bottom: '12px',
				right: '12px',
				backgroundImage: `url('${dotSvg}')`,
				backgroundSize: '12px 12px',
				backgroundRepeat: 'repeat',
			}),
			el(
				'div',
				{
					display: 'flex',
					flexDirection: 'column',
					justifyContent: 'space-between',
					width: '100%',
					height: '100%',
					padding: '30px',
					fontFamily: "'Instrument Sans', sans-serif",
					gap: '0px',
					position: 'relative',
				},
				[
					el(
						'div',
						{
							display: 'flex',
							flexDirection: 'row',
							justifyContent: 'space-between',
							alignItems: 'center',
							width: '100%',
							gap: '12px',
						},
						[
							el('span', { fontSize: '20px', fontWeight: '700', color: '#7CACF8', lineHeight: '1' }, 'Parseff'),
							el(
								'span',
								{
									fontSize: '12px',
									color: '#455E86',
									lineHeight: '1',
									border: '1px solid #455E86',
									padding: '4px 5px',
									borderRadius: '8px',
									backgroundColor: '#101014',
								},
								'documentation',
							),
						],
					),
					el(
						'div',
						{
							display: 'flex',
							flexDirection: 'column',
							justifyContent: 'flex-end',
							width: '100%',
							height: '100%',
							fontFamily: "'Instrument Sans', sans-serif",
							paddingBottom: '20px',
						},
						[
							el('span', { fontSize: '18px', fontWeight: '700', color: '#ffffff', lineHeight: '1' }, title),
							...(description
								? [
										el(
											'span',
											{ fontSize: '14px', fontWeight: '400', color: '#b4b4be', lineHeight: '1.5', marginTop: '12px' },
											description,
										),
									]
								: []),
						],
					),
				],
			),
		]),
	]);

	const svg = await satori(tree, {
		width: width / scale,
		height: height / scale,
		fonts: [...fonts],
	});
	const png = new Resvg(svg, {
		fitTo: { mode: 'zoom', value: scale },
		font: { loadSystemFonts: false },
		background: '#101014',
	})
		.render()
		.asPng();
	return new Response(new Uint8Array(png), { headers: { 'Content-Type': 'image/png' } });
};
