// usage: node check-links.mjs DIST BASE
//
// Exits 1 when a page in DIST, built with base path BASE, links from its
// <main> content to a page, file or anchor under BASE that does not exist,
// or when no link under BASE is found.

import { existsSync, readFileSync, readdirSync, statSync } from 'node:fs';
import { join, relative, sep } from 'node:path';

const [dist, base] = process.argv.slice(2);
if (!dist || !base) {
	console.error('usage: node check-links.mjs DIST BASE');
	process.exit(2);
}

const pages = (dir) =>
	readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
		const path = join(dir, entry.name);
		if (entry.isDirectory()) return pages(path);
		return entry.name === 'index.html' ? [path] : [];
	});

const ids = new Map();
const idsOf = (file) => {
	if (!ids.has(file)) {
		const html = readFileSync(file, 'utf8');
		ids.set(file, new Set([...html.matchAll(/\sid="([^"]+)"/g)].map((m) => m[1])));
	}
	return ids.get(file);
};

const resolveTarget = (pathname) => {
	const path = join(dist, decodeURIComponent(pathname.slice(base.length)));
	if (!pathname.endsWith('/') && existsSync(path) && statSync(path).isFile()) return path;
	const index = join(path, 'index.html');
	return existsSync(index) ? index : undefined;
};

let checked = 0;
const broken = [];
for (const page of pages(dist)) {
	const dir = relative(dist, join(page, '..')).split(sep).join('/');
	const route = dir ? `${base}/${dir}/` : `${base}/`;
	const main = readFileSync(page, 'utf8').match(/<main[\s\S]*<\/main>/)?.[0] ?? '';
	for (const [, href] of main.matchAll(/\shref="([^"]+)"/g)) {
		const url = new URL(href.replaceAll('&amp;', '&'), `http://site${route}`);
		if (url.host !== 'site' || !url.pathname.startsWith(`${base}/`)) continue;
		checked++;
		const target = resolveTarget(url.pathname);
		const anchor = decodeURIComponent(url.hash.slice(1));
		if (!target) broken.push(`${route}: ${href} (no such page)`);
		else if (anchor && target.endsWith('.html') && !idsOf(target).has(anchor))
			broken.push(`${route}: ${href} (no such anchor)`);
	}
}

broken.forEach((line) => console.error(`check-links: ${line}`));
console.log(`check-links: ${checked} links under ${base}/, ${broken.length} broken`);
process.exit(broken.length || !checked ? 1 : 0);
