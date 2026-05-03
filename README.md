# Visible Explorer

A static, browser-based tool for [Visible app](https://www.makevisible.com/) users to upload their CSV export and explore long COVID / ME-CFS tracking analyses. All R code runs locally in the browser via [WebR](https://docs.r-wasm.org/webr/) — no data leaves the user's machine.

## Local development

```bash
quarto preview
```

This starts a live-reload server. Note: WebR cells take ~5–10s on first load while the runtime and packages download.

## Building for deployment

```bash
quarto render
```

Output goes to `_site/`. Deploy as a static site (Netlify, GitHub Pages, etc.).

## Stack

- [Quarto](https://quarto.org) for the site
- [r-wasm/quarto-live](https://github.com/r-wasm/quarto-live) extension for in-browser R cells
- [WebR](https://docs.r-wasm.org/webr/) for the R runtime

## Sample data

`sample-visible.csv` is a real export used during development. Not committed if you keep `.gitignore` defaults.
