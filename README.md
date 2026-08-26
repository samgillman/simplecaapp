# SimpleCa²⁺

A modern, interactive Shiny application for calcium imaging data analysis. SimpleCa²⁺ provides a complete workflow for processing, analyzing, and visualizing calcium signals from individual cell recordings.

Developed by the **Wang Lab** at the University of Nebraska Medical Center (UNMC). Use it in your browser at [simplecalcium.samgillman.org](https://simplecalcium.samgillman.org) — no installation, and your data never leaves your device.

![R](https://img.shields.io/badge/R-4.5+-blue.svg)
![Shiny](https://img.shields.io/badge/Shiny-1.8+-green.svg)
![License](https://img.shields.io/badge/license-MIT-orange.svg)

## Features

### Data Processing
- **Multi-file support**: Load and analyze multiple experimental groups simultaneously
- **Frame-range baseline correction**: Define a stable baseline window with synchronized slider and typed frame controls; F₀ is the per-cell mean across those frames
- **ΔF/F₀ normalization**: Standard normalization for comparable calcium signals
- **Batch processing**: Process multiple datasets at once

### Visualization Tools
- **Time course plots**: Average traces with SEM ribbons and individual cell overlays
- **Heatmaps**: Global activity visualization with customizable sorting and color palettes
- **Metrics plots**: Per-cell quantification with statistical summaries
- **Publication-ready outputs**: High-resolution exports (PNG, PDF, TIFF, SVG)

### Quantitative Analysis
Automatic calculation of key calcium imaging metrics:
- **Peak ΔF/F₀**: Maximum fluorescence response intensity (excludes baseline period)
- **Time to Peak**: Time-axis coordinate of the post-baseline maximum (not stimulus latency unless Time = 0 marks stimulus onset)
- **Rise Time (10-90%)**: Time for signal to rise from 10% to 90% of peak
- **10–90% ΔF/F₀ Rise Rate**: Average normalized-fluorescence slope between the 10% and 90% crossings (not a direct calcium-influx measurement)
- **Time to % Peak**: Time to reach 25%, 50%, and 75% of peak
- **FWHM & Derived Half-Width**: Exact duration at half-maximum when both crossings are observed; sustained responses are marked right-censored and exported with an observed lower bound; derived Half-Width is exact FWHM divided by two
- **Area Under Curve (AUC)**: Signed net ΔF/F₀ integral over time
- **Signal-to-Noise Ratio (SNR)**: Response strength relative to baseline noise
- **Baseline Standard Deviation**: Quantifies noise level during rest period

### Advanced Features (New)
- **Precise Baseline Window Entry**: Set the start and end frames by typing values or using the synchronized range slider
- **Per-file Column Mapping**: Confirm automatic Time detection, explicitly select elapsed-time or frame-index columns, generate Time from sampling rate, and exclude metadata or unwanted traces before processing
- **Baseline Period Protection**: Peaks within baseline frames are excluded from analysis
- **Metric Explanations**: Visual breakdown showing how each metric is calculated using your actual data
- **Improved ΔF/F₀ Calculation**: Handles edge cases like very small baselines and already-processed data

### Export & Documentation
- **Multiple export formats**: CSV, Excel, PNG, PDF, TIFF, SVG
- **Summary statistics**: Mean, SEM, and sample size for all metrics
- **Interactive tables**: Sortable, searchable data tables with copy/download
- **Complete protocol**: Step-by-step guide included in the app

## Installation

### Prerequisites
- R (≥ 4.0)
- RStudio (recommended)

### Quick Start

1. **Clone the repository:**
```bash
git clone https://github.com/samgillman/simplecaapp.git
cd simplecaapp
```

2. **Install renv (if not already installed):**
```r
install.packages("renv")
```

3. **Restore package dependencies:**
```r
renv::restore()
```

4. **Run the app:**
```r
shiny::runApp()
```

The app will open in your default web browser.

## Usage

### Data Format

SimpleCa²⁺ requires data in **wide format** (CSV or Excel):

| Time | Cell1 | Cell2 | Cell3 |
|------|-------|-------|-------|
| 0.0  | 1.02  | 1.05  | 1.01  |
| 0.1  | 1.03  | 1.06  | 1.02  |
| 0.2  | 1.50  | 1.45  | 1.30  |
| ...  | ...   | ...   | ...   |

- **Time column (recommended)**: Elapsed seconds with a header beginning with "Time"; it may appear anywhere in the table.
- **Frame column (optional alternative)**: Frame numbers are converted to elapsed seconds using the sampling rate configured in Load Data.
- **Unnamed ImageJ frame index**: A sequential first column with a blank header is recognized as Frame rather than analyzed as a cell trace.
- **No Time or Frame column**: Time is generated from the sampling rate without discarding any cell column.
- **Cell columns**: Fluorescence values for each cell, with a unique header for each cell.
- **Column confirmation**: After upload, Advanced Options shows the detected Time source for each file and lets you exclude unwanted numeric columns before analysis.

### Workflow

1. **Load Data** → Upload your files and configure processing options
2. **Time Course** → Visualize average traces with customizable styling
3. **Heatmap** → View cell-by-cell activity patterns
4. **Metrics** → Quantify and compare signal properties between groups (with per-metric explanations)
5. **Data & Export** → Explore numerical results and download all figures and data

For detailed instructions, see [PROTOCOL.md](PROTOCOL.md) or use the built-in Help tab.

## Project Structure

```
simplecaapp/
├── app.R                 # Main application file
├── PROTOCOL.md           # Detailed user guide
├── README.md             # This file
├── renv.lock             # Package dependency lockfile
├── R/                    # Shiny modules & helpers
│   ├── mod_load_data.R       # Upload + ΔF/F₀ processing
│   ├── mod_time_course.R     # Time course plots
│   ├── mod_heatmap.R         # Heatmaps
│   ├── mod_metrics.R         # Metrics plots
│   ├── mod_metrics_explained.R  # Per-metric visual explanations
│   ├── mod_data_export.R     # Data tables + figure export
│   ├── mod_help.R            # Built-in help
│   ├── components.R          # Reusable UI components
│   ├── plot_controls.R       # Shared plot controls
│   ├── theme.R               # Design tokens & CSS
│   └── utils.R               # Metrics computation & IO helpers
├── scripts/
│   └── export_shinylive.R    # Build the WebAssembly (browser-only) version
├── tests/                # testthat unit tests (run: Rscript tests/testthat.R)
├── www/                  # Static assets (logos, images)
└── renv/                 # Package management (auto-managed)
```

## Testing

The metric calculations are verified against synthetic traces with
hand-computed ground truth (peak, rise time, FWHM, AUC, SNR, threshold
crossings), including missing samples and right-censored sustained responses.
Run the suite from the repo root:

```bash
Rscript tests/testthat.R
```

Tests also run automatically on every push via GitHub Actions
(`.github/workflows/ci.yml`).

## Key Dependencies

- **shiny** & **shinydashboard**: Web application framework
- **ggplot2** & **plotly**: Visualization
- **dplyr** & **data.table**: Data manipulation
- **DT**: Interactive tables
- **readxl**: Excel file support
- **gt** & **kableExtra**: Publication-quality tables

Full dependency list: [renv.lock](renv.lock)

## Development

### Adding Features
Modules are organized in the `R/` directory following Shiny module best practices. Each module has a UI function (`*_ui()`) and a server function (`*_server()`).

### Package Management
This project uses `renv` for reproducible package management:
- `renv::snapshot()` - Save current package versions
- `renv::restore()` - Install recorded package versions
- `renv::status()` - Check for changes

## Deployment

### Option 1: Cloudflare Pages via Shinylive (browser-only, no server)

The app can be compiled to WebAssembly with
[Shinylive](https://posit-dev.github.io/r-shinylive/) so it runs entirely in
the visitor's browser — free static hosting, no R server to maintain, and
uploaded data never leaves the user's machine.

Build locally:

```bash
Rscript scripts/export_shinylive.R
Rscript -e 'httpuv::runStaticServer("_shinylive")'   # preview
```

Automatic deployment is set up in
`.github/workflows/deploy-shinylive.yml`. One-time setup (do NOT use
Cloudflare's "Connect GitHub" flow — Cloudflare's builders have no R; the
GitHub Action builds the site and uploads the static files):

1. Create a Cloudflare API token (**My Profile → API Tokens → Create
   Token**) with the **Cloudflare Pages: Edit** permission.
2. In the GitHub repo: **Settings → Secrets and variables → Actions**, add
   `CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID` (the account ID is
   the hex string after `dash.cloudflare.com/` in the dashboard URL).

The workflow creates the Pages project (`simpleca`) automatically on its
first run. Every push to `main` then rebuilds and publishes the site at
`https://simpleca.pages.dev`.

#### Custom domain (`simplecalcium.samgillman.org`)

The deploy workflow automatically registers `simplecalcium.samgillman.org`
with the Pages project. For the domain to go live, DNS must point at the
Pages site:

- **If `samgillman.org` is managed in this Cloudflare account**: open the
  zone's DNS settings and confirm a `CNAME` record exists for
  `simplecalcium` → `simpleca.pages.dev` (proxied). Cloudflare usually
  creates it when the custom domain is confirmed under
  **Workers & Pages → simpleca → Custom domains**.
- **If DNS is hosted elsewhere**: add a `CNAME` record for
  `simplecalcium` pointing to `simpleca.pages.dev` at your DNS provider.

Certificates are issued automatically once DNS resolves (can take a few
minutes).

Notes on the WebAssembly build:
- The gt-table image export is hidden (it needs a local Chrome); all
  CSV/Excel/figure downloads work normally.
- First load downloads the R runtime and packages (tens of MB), so it is
  slower than the server version; everything after that is instant.
- Processing runs on the visitor's machine, so very large recordings are
  limited by their browser's memory.

### Option 2: Posit Connect Cloud / shinyapps.io (server-hosted)
```r
library(rsconnect)
rsconnect::deployApp()
```
The `manifest.json` used by Posit Connect Cloud git-backed deployment is
kept in the repo — regenerate it with `rsconnect::writeManifest()` after
changing dependencies.

## Citation

If you use SimpleCa²⁺ in your research, please cite:

```
Gillman, S. (2026). SimpleCa²⁺: in-browser analysis of calcium imaging data.
Wang Lab, University of Nebraska Medical Center.
https://simplecalcium.samgillman.org
```

The citation will be updated when the accompanying manuscript is published.
To mint a citable DOI for a release: enable this repository in the
[GitHub–Zenodo integration](https://zenodo.org/account/settings/github/)
(one-time, requires a Zenodo login), then publish a GitHub release — Zenodo
archives it and issues the DOI automatically.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Support

For questions or issues:
- Open an issue on GitHub
- See [PROTOCOL.md](PROTOCOL.md) for detailed usage instructions
- Check the built-in Help tab in the application

## Acknowledgments

Built with R Shiny and designed for calcium imaging researchers.

---

**SimpleCa²⁺** - Making calcium imaging analysis simple and accessible.
