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

`make website-build-versions` builds `main` and every tag from 0.4.0 on into `website/dist`, through `website/build-versions.sh`:

- `/parseff/` is the newest tag, `/parseff/X.Y.Z/` is each tag, `/parseff/main/` is `main`.
- Versions other than the newest tag get an "old version" or "unreleased" banner and `noindex`.
- `versions.json` at the site root feeds the version selector. `public/versions.json` is a stub for `npm run dev`.
- Tags that predate the env-driven config are patched at build time. The script fails if a pattern is missing.
- Use links relative to the page root such as `api/errors/` in content, not `/parseff/...`.

## Deployment

The `deploy` job in `.github/workflows/ci.yml` runs on pushes to `main`, tag pushes and manual dispatch. It rebuilds every version and publishes them to GitHub Pages.

## Resources

- [Astro Documentation](https://docs.astro.build)
- [Starlight Documentation](https://starlight.astro.build/)
