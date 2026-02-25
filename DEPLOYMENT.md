# Deploying Parseff Documentation

This guide explains how to deploy the Parseff documentation website to GitHub Pages.

## Overview

The documentation is built with Astro and Starlight and automatically deploys to GitHub Pages via GitHub Actions.

## Prerequisites

1. GitHub repository with Pages enabled
2. Repository settings:
   - Go to Settings → Pages
   - Source: GitHub Actions

## Automatic Deployment

The site automatically deploys when you push to the `main` branch.

The GitHub Actions workflow (`.github/workflows/deploy-docs.yml`) will:
1. Check out the code
2. Install Node.js dependencies
3. Build the Astro site
4. Deploy to GitHub Pages

## Manual Deployment

To deploy manually:

```bash
# 1. Build the site
cd documentation
npm install
npm run build

# 2. Deploy the dist/ folder to gh-pages branch
# (using your preferred deployment method)
```

## Configuration

### Update Repository URLs

Before deploying, update these files with your repository information:

1. **`documentation/astro.config.mjs`**:
   ```js
   site: 'https://davesnx.github.io',
   base: '/parseff',  // or your repo name
   social: [
     {
       icon: 'github',
       label: 'GitHub',
       href: 'https://github.com/davesnx/parseff',
     },
   ],
   ```

2. **`README.md`**:
   ```markdown
   **[View Full Documentation](https://davesnx.github.io/parseff/)**
   ```

3. **`documentation/src/content/docs/index.mdx`** and other pages:
   - Update GitHub links to `https://github.com/davesnx/parseff`

### Custom Domain (Optional)

To use a custom domain:

1. Add a `public/CNAME` file in the `documentation` directory:
   ```
   docs.yourdomain.com
   ```

2. Update `astro.config.mjs`:
   ```js
   site: 'https://docs.yourdomain.com',
   base: '/',  // Root path for custom domain
   ```

3. Configure DNS:
   - Add a CNAME record pointing to `davesnx.github.io`

## Verifying Deployment

After deployment:

1. Visit `https://davesnx.github.io/parseff/`
2. Check that all pages load correctly
3. Verify syntax highlighting works
4. Test the search functionality

## Troubleshooting

### Build Fails

```bash
# Check for errors locally
cd documentation
npm run build
```

Common issues:
- Missing dependencies: `npm install`
- TypeScript errors: Check frontmatter in `.md` files
- Invalid configuration: Validate `astro.config.mjs`

### 404 Errors

If pages show 404:
- Verify `base` in `astro.config.mjs` matches repository name
- Check that links in content use `/parseff/` prefix

### Styling Issues

If custom styles don't apply:
- Verify `src/styles/custom.css` exists
- Check it's listed in `astro.config.mjs` → `customCss`

## Local Development

Test the site locally before deploying:

```bash
cd documentation

# Development server with hot reload
npm run dev
# Opens at http://localhost:4321/parseff

# Preview production build
npm run build
npm run preview
```

## Maintenance

### Updating Dependencies

```bash
cd documentation
npm update
npm audit fix
```

### Adding Content

1. Create new `.md` file in `src/content/docs/`
2. Add to sidebar in `astro.config.mjs`
3. Push to `main` branch
4. GitHub Actions will automatically rebuild and deploy

## Resources

- [Astro Documentation](https://docs.astro.build)
- [Starlight Documentation](https://starlight.astro.build/)
- [GitHub Pages Documentation](https://docs.github.com/pages)
