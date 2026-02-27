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

## Deployment

The site automatically deploys to GitHub Pages when pushed to the `main` branch via GitHub Actions (`.github/workflows/deploy-docs.yml`).

## Resources

- [Astro Documentation](https://docs.astro.build)
- [Starlight Documentation](https://starlight.astro.build/)
