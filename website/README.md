# Parseff Documentation

This directory contains the Parseff documentation website built with [Astro](https://astro.build) and [Starlight](https://starlight.astro.build/).

## Development

```bash
# Install dependencies
npm install

# Start dev server at localhost:4321
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview
```

## Structure

```
documentation/
├── src/
│   ├── content/
│   │   └── docs/           # Markdown/MDX documentation pages
│   │       ├── index.mdx   # Homepage
│   │       ├── api/        # API reference
│   │       └── guides/     # How-to guides
│   └── styles/
│       └── custom.css      # Custom dark theme
├── public/                 # Static assets
├── astro.config.mjs        # Astro configuration
└── package.json
```

## Customization

The site uses a custom dark theme inspired by errore.org. Colors and styles can be modified in `src/styles/custom.css`.

## Adding Content

1. Create a new `.md` or `.mdx` file in `src/content/docs/`
2. Add frontmatter:
   ```yaml
   ---
   title: Your Page Title
   description: Page description
   ---
   ```
3. Update `astro.config.mjs` sidebar configuration
4. Write your content using Markdown

## Versions

`make website-build-versions` builds `main` and every release tag from 0.4.0 on (pre-release tags are skipped) into `website/dist`, through `website/build-versions.sh`:

- `/parseff/` is the newest tag, `/parseff/X.Y.Z/` is each tag, `/parseff/main/` is `main`.
- Every version is built in place with the current site (config, components, styles). Only the docs content comes from the tag, through `git archive` into a temporary directory that `DOCS_CONTENT_DIR` points to. It defaults to `src/content/docs`.
- `site.mjs` holds the site root (`/parseff`) and reads the build settings once: `DOCS_BASE`, `DOCS_VERSION`, `DOCS_CHANNEL` and `DOCS_CONTENT_DIR`. Without them, `npm run dev` and `npm run build` use version `main`, channel `unreleased`, and the root as base.
- `DOCS_CHANNEL` is one of `latest`, `old`, `unreleased` or `alias`. Other values fail the build.
  - `old` and `unreleased` get a banner and `noindex`.
  - `alias` is the `/parseff/X.Y.Z/` copy of the newest tag. It gets `noindex` and a canonical link to the same page at the root, and no banner.
- Sidebar entries whose page is missing in an old tag are dropped.
- `versions.json` at the site root feeds the version selector. When it is missing, for example in `npm run dev`, the selector shows only the current version and logs the error to the console.
- Links and images to `/parseff/...` in content are rewritten to the base of the version being built. Write links in content as `/parseff/api/errors/`.

## Deployment

The `website` job in `.github/workflows/ci.yml` builds every version. It runs on pull requests too, so a pull request tests the versions build, but it uploads nothing there.

The `deploy` job publishes to GitHub Pages. It runs only on pushes to `main`, on tag pushes, and on manual dispatch on `main`. It is the only job with Pages permissions. A pull request never deploys.

On a tag push, the `website` job builds from the checkout of `main`, not of the tag. The site (config, components, styles) and `/parseff/main/` come from `main`. The tag only adds its own docs content, which the script reads with `git archive`.

## Resources

- [Astro Documentation](https://docs.astro.build)
- [Starlight Documentation](https://starlight.astro.build/)
