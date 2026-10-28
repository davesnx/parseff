import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';

export const root = '/parseff';

const channels = ['latest', 'old', 'unreleased', 'alias'];

export const base = process.env.DOCS_BASE ?? root;
export const version = process.env.DOCS_VERSION ?? 'main';
export const channel = process.env.DOCS_CHANNEL ?? 'unreleased';
export const contentDir = process.env.DOCS_CONTENT_DIR
	? resolve(process.env.DOCS_CONTENT_DIR)
	: fileURLToPath(new URL('./src/content/docs', import.meta.url));

if (!channels.includes(channel)) {
	throw new Error(`DOCS_CHANNEL must be one of ${channels.join(', ')}, got "${channel}"`);
}
