# Changelog

All notable changes to SimpleCa will be documented in this file.

## Unreleased

## [1.15.0] - 2026-08-26

### Added
- **Precise baseline window entry:** Start and End frame number fields stay synchronized with the baseline range slider, allowing either quick dragging or exact typed values.
- **Per-file column mapping:** Advanced Options confirms automatic Time/Frame detection and allows an explicit elapsed-time column, frame-index column, generated Time, and exclusion of unwanted numeric columns before analysis.
- **Heatmap color-scale interval:** Plot Controls can retain automatic legend spacing or use a typed interval such as 0.5 ΔF/F₀ for both on-screen and downloaded heatmaps.
- **Informative browser loading screen:** Cloudflare/Shinylive visitors now see branded, accessible estimated progress, real app-readiness detection, rotating usage tips, the local-processing privacy promise, and a recovery prompt when first-time setup takes unusually long.

### Improved
- **Baseline processing is frame-range only:** The Load Data screen now asks only for a stable baseline frame window. F₀ is always the per-cell mean across those frames; the Rolling Minimum and Percentile controls and processing paths have been removed.
- **Censored-width summaries are explicit:** Instead of a bare `n=0`, exact FWHM and Half-Width rows state how many responses recovered and how many were right-censored; observed lower bounds remain a separate row.
- **Metric explanations match the implemented calculations:** AUC is identified as a signed net integral, Time to Peak is distinguished from stimulus latency, the fluorescence rise-rate metric no longer claims to directly measure calcium influx, and Half-Width is identified as the derived value FWHM/2. Explanation plots now reuse the stored post-baseline peak and selected baseline window.
- **Load workflow follows processing order:** Advanced Options now appears before the Process Data action, sampling-rate guidance is shorter, and responsive layouts use either three columns or one vertical sequence instead of an ambiguous two-plus-one wrap.
- **Changed preprocessing settings invalidate old results:** Selecting different files or changing baseline, sampling-rate, or column-mapping inputs now removes outputs calculated with the previous configuration and clearly asks the user to process again, preventing stale plots or exports from appearing current.

### Fixed
- **Collapsed sidebar no longer leaves an empty dark strip:** When AdminLTE moves the sidebar fully off-screen, the header, content, and footer now return to the viewport edge instead of reserving a nonexistent 50 px mini-sidebar rail.
- **Single-recording violin plots stay proportional:** A violin no longer expands across most of the Metrics plot when only one recording or displayed group is present; multi-group comparisons retain their existing width.

## [1.14.0] - 2026-08-16

### Fixed
- **All downloads work in the deployed (browser) app.** An instrumented WebAssembly build showed every HTTP download link — plot images, header CSVs, and ZIP archives alike — failing in the shinylive runtime: downloads started and then received an error response, which browsers saved as an HTML file. Every download now generates the file exactly as before but delivers the bytes over the app's live Shiny connection to a client-side saver, the same in-page mechanism the table corner buttons already used successfully. One code path serves desktop and browser builds identically; failures now surface as an in-app error notification instead of a junk file

## [1.13.4] - 2026-08-16

### Fixed
- **Table CSV/Excel buttons export every row, not just the visible page.** The corner buttons on all four Data & Export tables silently truncated exports to the current 25-row page because the tables ran in server-side processing mode; they are now client-side, so the buttons carry the complete data. The header "Download CSV" buttons and ZIP archives were never affected
- **Time Course table's CSV/Excel buttons produce a download again**: the export crashed on the raw numeric Time column; it is now formatted for display like every other table's numeric columns

## [1.13.3] - 2026-08-16

### Fixed
- **Export file names identify their actual contents.** Per-group processed data downloads now lead with that group's original file name (e.g. `NG2_Nicotine_..._processed_<date>.csv`); with several files loaded, whole-dataset exports (metrics, summaries, figures, ZIPs) lead with the group count (e.g. `3_groups_cell_metrics_...`) instead of misleadingly carrying the first upload's name. Single-file exports keep the original-file-name prefix

## [1.13.2] - 2026-08-16

### Fixed
- **ZIP downloads work in the browser (deployed) build.** Download All (ZIP) for processed data and Download All Figures (ZIP) failed on the deployed site and saved an HTML error page instead: they depended on the compiled zip package, which is unavailable in the WebAssembly runtime. Archives are now written with a base-R ZIP writer (DEFLATE streams and CRC32 obtained via gzfile), verified byte-for-byte against independent unzip implementations and covered by tests

## [1.13.1] - 2026-08-16

### Added
- **Download All (ZIP) for processed data**: the Processed Data table and the Figure Export data column both offer every group's processed wide CSV in one ZIP, so multi-file uploads don't require cycling the group dropdown
- **Multi-file mode accumulates across Browse clicks.** A native file dialog replaces its selection each time and cannot pick files from different folders in one pass, so browsing again now adds to the staged list instead of discarding it; re-selecting a file name replaces its older entry. Each staged file has a remove button, and a Clear list link resets the staging

### Fixed
- **Auto plot titles no longer overflow on many-file uploads**: when the joined group names exceed one line, the Time Course and Heatmap titles fall back to a compact group count (a custom title always wins)
- **Metrics group labels wrap instead of colliding**: long file-derived group names on the box/violin x axis wrap onto multiple lines

## [1.13.0] - 2026-08-14

### Added
- **Single file / Multiple files toggle on the upload step.** The uploader starts in single-file mode; switching to Multiple files accepts one file per experimental group for side-by-side comparison. Switching modes clears the pending selection, and single mode processes exactly one file, so the two workflows cannot mix

### Improved
- **Multi-file (batch) uploads render as proper group comparisons.** Uploading several files at once has always created one experimental group per file, but the display undermined it: every group's mean line drew in the same color because the single Line color picker overrode the group palette, the SEM ribbon was built as one self-crossing polygon sweeping across all groups, and heatmap facets carried no group labels. Multi-group uploads now keep per-group palette colors for means, traces, and ribbons (the Line color picker still applies to single-group plots), each group gets its own clean ribbon, and heatmap facets are labeled with their group name
- Upload step and format reference now say that several files can be selected at once, one per experimental group

## [1.12.3] - 2026-08-14

### Fixed
- **Time Course average line draws again**: the mean trace was silently dropped from the static plot (only the SEM ribbon rendered) because the per-point hover text used by the interactive view fragmented the line into single-point groups; the mean line now declares its grouping explicitly in both color modes

## [1.12.2] - 2026-08-14

### Fixed
- **FWHM and Rise Time explanation plots title ImageJ cells consistently**: their titles now go through the same display helper as every other explanation plot, so a Mean1 column titles as "Cell 1" instead of the raw identifier

## [1.12.1] - 2026-08-14

### Fixed
- **ΔF/F₀ labels render identically on every system**: plot titles, axis titles, and legend titles now use plain Unicode text instead of R plotmath expressions; the plotmath italic Delta drew overlapping the F on systems where the requested font is unavailable and a metric-mismatched fallback is substituted

## [1.12.0] - 2026-08-13

### Changed
- **Plot Controls are a collapsed strip inside each plot box**, directly above the figure: one slim bar by default, and expanding it puts the controls right above the plot so changes are visible while adjusting — replaces the bottom-of-page controls sheet, which forced scrolling away from the plot being modified
- Expanding a collapsed panel now re-measures sliders, fixing zero-width slider tracks after reveal

## [1.11.0] - 2026-08-13

### Changed
- **Time Course, Heatmap, and Metrics rebuilt on the Load Data pattern**: full-width stacked sections instead of a side controls column, so ragged column bottoms are structurally impossible and every plot spans the whole content width
  - Time Course: plot, then summary statistics, then a Plot Controls sheet (Display / Style / Labels & Text / Axis & Export as divided column groups)
  - Heatmap: full-width heatmap (much larger panels per group), controls sheet below
  - Metrics: the primary choices (Metric, Plot style) move to a slim header strip like Explanations; full-width plot; controls sheet below
- Custom axis limit fields lay out 2x2 to fit the new control columns

## [1.10.0] - 2026-08-13

### Changed
- **Load Data redesigned from scratch**: one full-width "Load & Process" box presents the workflow as three side-by-side numbered steps (Upload, Baseline correction, Process) sharing a single height — there is no status panel to inflate and no column ratio to balance. After processing, a slim one-line results bar (files, cells, timepoints, View Results button) replaces the old Data Overview box, whose three numbers could never fill a two-thirds-width panel. Quick Start is a full-width strip with the steps and format example side by side, shown only before processing

## [1.9.0] - 2026-08-13

### Changed
- **One control aesthetic everywhere**: dropdowns (selectize) were rendering ~56px tall with the text floating high — an overflow guard pushed their internal input to a second line — while every other control is 34px; all selects are now the same height, carry the same thin chevron as native selects (replacing selectize's solid triangle), and the file-upload text field gets the same rounded corners and 13px size as its neighbors
- **Load Data columns end on one shared bottom edge** in every state (empty, files selected, processed): the row is an opt-in equal-height flex row where Processing Options and Data Overview absorb the height difference
- **Time Course summary is discoverable**: the plot yields ~80px on shorter windows so the summary table's header is visible above the fold instead of hidden until scroll
- **Explanations row truly fits the window**: the shared box height offset is corrected (the row previously ended ~11px past the fold at any window height)
- **Heatmap legend is readable**: larger tick and title text, a taller and wider color bar, and proper title spacing

## [1.8.1] - 2026-08-13

### Changed
- **Load Data empty state states things once**: the input-format example table lives inside Quick Start; the separate Expected Format box now appears only after processing (when Quick Start leaves), so nothing is cut at the fold and the format is no longer described three times on one screen
- **Time Course controls are one screenful**: the Display accordion keeps only the toggles that change what is plotted (traces, transparency, average, SEM); line width, color, legend position, and theme move to a collapsed Style accordion
- **Explanations row ends on one bottom edge**: the Explanation & Controls box takes the exact computed height of the Visualization box (long content scrolls inside), so the two boxes align at every window size
- **Explanation plot labels never sit bare on data**: Response Amplitude's "Baseline (0)" tag moves to the clear right end of the baseline with white backing, and the 10/90% and 25/50/75% threshold tags get the same backing

## [1.8.0] - 2026-08-13

### Changed
- **Plots adapt to window height**: Time Course, Heatmap, Metrics, and Explanations plots now shrink on shorter windows (with a sensible minimum) instead of forcing the user to scroll to see the whole figure; large screens keep the previous size
- **Time Course summary sits under the plot**: the summary statistics table moves into the plot column directly beneath the figure, removing the dead grey band that separated them when the Controls column was taller
- **Load Data right column is balanced**: a compact Expected Format reference (example table plus the one-file-per-group and ImageJ MeanN notes) fills the space below Data Overview instead of leaving a grey field
- **Heatmap Labels & Text accordion starts collapsed** so the Controls column no longer towers over the heatmap
- **Formulas render as clean HTML math**: MathJax is removed — it typeset unreliably under the WebAssembly build (raw $$..$$ text, equations overlapping the column edge) and pulled an external CDN script; Explanations formulas now use styled HTML fractions/subscripts that render identically everywhere
- **Explanation plot annotations match the app palette**: raw R primaries (red/blue/purple/darkorange) replaced with a colorblind-safe Okabe-Ito set anchored on the brand blue; the Baseline SD plot's ±1 SD labels move to the clear right end of the lines with white backing instead of colliding with the y axis under a grey wash
- **Table pagination is legible**: the active page number in data tables is white-on-blue instead of dark-blue-on-blue

## [1.7.0] - 2026-08-13

### Changed
- **Systematic visual polish pass** across every tab at 1440/1920/2000-wide viewports:
  - Primary action buttons (Process Data, View Results) render solid brand blue again — Shiny's implicit `btn-default` class no longer wins the cascade over `btn-primary`
  - Full-width boxes (Time Course summary, Explanations header, Data & Export) drop their redundant `column()` wrappers, so every row shares the same content edges instead of sitting 8px inset
  - Accordion bodies no longer double-pad their content (24px+ → 12px), tightening all control panels
  - The idle file-upload progress strip is a slim brand-colored meter instead of a 20px text-height gap between the upload field and its helper text
  - Load Data's Processing Options box is visibly more compact: shorter explainer lines, tighter section and footer margins
  - Help tab boxes all use the primary header color, and info callouts use a quiet light-blue tint instead of AdminLTE's saturated cyan
  - Tables that end a box body no longer stack their own bottom margin on the box padding (removes trailing dead space under summary/DT tables)
  - Sidebar version badge actually gets its intended padding (the selector now out-ranks AdminLTE's `.sidebar-menu > li` rules) and aligns with the menu items
- **ImageJ-style cell names display as cells**: columns named `Mean1`, `Mean2`, … show as "Cell 1", "Cell 2" in the Explanations plot title, the cell selector, and metrics hover text; exported tables and data structures keep the raw identifiers

## [1.6.0] - 2026-08-13

### Changed
- **Flat sidebar navigation**: Metrics is a single one-click item (no submenu); "Metric Explanations" moves to a new Reference section alongside Help; the vestigial ANALYSIS header is gone. Removes the treeview component and all its special-case styling (the source of the mobile label bug)

## [1.5.0] - 2026-08-13

### Changed
- **Explanations tab**: the metric and cell selectors now sit together in the header strip instead of the cell selector being buried in a bottom accordion
- **Readable plot titles**: default titles and group axis labels display underscores as spaces (e.g. "Capsaicin 10 15 25 NG2"); data identifiers are unchanged
- **Wang Lab attribution** (University of Nebraska Medical Center) added to the sidebar, the Help tab's new About section, and the README
- README carries a provisional software citation and one-time Zenodo DOI instructions; PROTOCOL Step 1 now describes the actual processing options

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.4.0] - 2026-08-13

### Added
- **Instant updates**: the service worker now activates new builds immediately and the page reloads once when a new version takes control, so a plain refresh always shows the latest deploy (ends stale-cache confusion)

### Changed
- **Compact control panels**: tighter spacing, slimmer inputs, smaller labels, sliders without floating min/max chips — control columns use roughly a third less vertical space
- **Explanations plots** title as "Cell: <name>" (e.g. "Cell: Mean1"), since columns like Mean1 are individual cells/neurons
- **Time Course legend**: larger, without the redundant "Group" title

## [1.3.2] - 2026-08-13

### Removed
- **Post-Analysis tab**: removed to keep the app focused on the core single-session workflow (Load → Time Course → Heatmap → Metrics → Export); the pooling workflow confused users

### Fixed
- **Sidebar "Metrics" label invisible on mobile**: AdminLTE's touch hover/focus states painted the treeview parent dark while our text stayed dark; all states are now overridden at matching specificity
- **Plots no longer bleed past their panel** (heatmap and others): plot containers clip overflow
- **Load Data whitespace**: Data Overview and Processing Status merged into one compact panel with the post-processing call-to-action
- Explanations header: selector no longer adds stray bottom whitespace

## [1.3.1] - 2026-08-13

### Added
- **Informative loading screen** on the WebAssembly build: explains the first-visit runtime download, expected wait, and that data never leaves the device; fades out when the app is ready
- **Version badge** in the sidebar so it is always clear which build is on screen (helps diagnose stale service-worker caches)
- **"View Results" button** on Load Data after successful processing, jumping straight to Time Course

### Changed
- **Equal-height rows**: side-by-side panels in each tab now end on one baseline on desktop instead of ragged bottoms; Post-Analysis step columns get vertical dividers

## [1.3.0] - 2026-08-13

### Changed
- **Metrics plots now compare groups**: Box + Swarm and Violin styles place experimental groups side by side on the x axis with the group colors and per-group mean ± SEM overlays, instead of pooling all cells into one distribution
- **Time Course summary table is per group**: one column per experimental group (mean ± SEM, n) instead of a single pooled average across conditions
- **Lighter dependency set**: shinyWidgets toggles/pickers replaced with base Shiny inputs (checkboxes, styled segmented Static/Interactive toggle, selectize); gt and webshot2 removed — shrinks the WebAssembly download every visitor pays on first load

### Removed
- **Processed Data tab**: fully redundant with Data & Export (which has the same tables and downloads, per group) and its pooled "Average Metrics (All Cells)" table mixed experimental conditions into one mean; the gt table-image export went with it (CSV/Excel export of the same numbers remains in Data & Export)

### Fixed
- Sidebar "Metrics" label no longer disappears when one of its subtabs is active (dark-on-dark text)
- Long filenames in dropdowns clip with an ellipsis instead of overflowing onto neighboring panels; stat-card values scale to fit their card

## [1.2.0] - 2026-08-13

### Added
- **Unit test suite**: testthat tests verifying `calculate_cell_metrics()` against a synthetic pulse with hand-computed ground truth (peak, rise time, threshold crossings, FWHM, AUC, SNR), plus tests for data-loading and filename helpers (`tests/`)
- **Continuous integration**: GitHub Actions workflow parse-checks all R sources and runs the test suite on every push/PR (`.github/workflows/ci.yml`)
- **Shinylive/WebAssembly build**: `scripts/export_shinylive.R` exports the app as a static site that runs entirely in the browser; `.github/workflows/deploy-shinylive.yml` publishes it to Cloudflare Pages
- **Metrics baseline window for all F₀ methods**: the baseline window slider is now always visible and explicitly defines the baseline period used for metrics (noise SD, SNR, peak-search exclusion) under Rolling Minimum and Percentile methods too — previously a hidden frames-1–20 assumption

### Changed
- **Cells with unusable baselines are excluded, not silently altered**: a cell whose F₀ is zero, negative, or missing is now dropped with a warning notification; previously it silently got ΔF (or raw F) while other cells got ΔF/F₀, mixing units in the same plots
- **Duplicate file names no longer overwrite each other**: uploaded files sharing a base name are made unique (`name`, `name_1`, ...)
- **All skipped files are reported**: files with no usable numeric traces are always listed in the warning notification (one skip path was previously silent)
- **webshot2 is now optional**: the gt-table image export (which requires a local Chrome) hides itself when webshot2 is unavailable, so the app runs under Shinylive/webR and on servers without Chrome
- **Production errors are logged**: the generic error notification now also logs the real error message to the server log

### Removed
- Dead modules `R/mod_tables.R` and `R/mod_export.R` (superseded by `R/mod_data_export.R`), the placeholder `tests/verify_fixes.R`, and the unused shinyvalidate dependency

### Fixed
- `calculate_cell_metrics()` early returns now produce the same columns as full results (previously some paths were missing the Time-to-%-Peak and Calcium Entry Rate columns)
- Corrected the AUC source comment: AUC is the net trapezoid area relative to baseline (deflections below baseline subtract); the code was already correct, the comment was not

## [1.1.0] - 2024-11-10

### Added
- **Interactive Baseline Adjustment**: Users can now adjust baseline frames directly in Time Course and Heatmap tabs without reprocessing data
- **Baseline Period Protection**: Algorithm now excludes peaks that occur within the baseline period from analysis
- **Metric Explanations Tab**: Visual breakdown showing how each metric is calculated using actual data
- **Improved Error Handling**: Better handling of edge cases in ΔF/F₀ calculation

### Changed
- **Enhanced Peak Detection**: Peaks are now only searched for after the baseline period ends
- **Improved Metrics Calculation**: Fixed issue where metrics were incorrectly calculated from already-processed ΔF/F₀ data
- **Better Baseline Handling**: Improved handling of very small baseline values and already-normalized data

### Fixed
- **Metrics Calculation Bug**: Fixed issue where only 1 cell would have complete metrics when peaks occurred in baseline
- **ΔF/F₀ Calculation**: Now correctly passes `data_is_dFF0 = TRUE` flag to prevent double normalization
- **Baseline Frame Synchronization**: Baseline frame sliders now properly sync across all tabs

### Technical Improvements
- Refactored `calculate_cell_metrics()` function to handle edge cases
- Added validation for peaks occurring during baseline period
- Improved search window logic for threshold crossing detection

## [1.0.0] - 2024-10-23

### Initial Release
- Core calcium imaging analysis functionality
- Multi-file batch processing
- Time course visualization with individual traces and mean ± SEM
- Population heatmaps with sorting options
- Comprehensive metrics calculation (Peak, Time to Peak, Rise Time, FWHM, AUC, SNR, etc.)
- Interactive data tables with export functionality
- Multiple export formats (CSV, PNG, PDF, SVG, TIFF)
- Baseline correction with multiple methods
- Protocol documentation and built-in help

## Known Issues
- Inverted responses (fluorescence decreases) are treated as no response - future versions may add support for bidirectional analysis

## Upcoming Features (Planned)
- Support for analyzing inverted calcium responses
- Automated spike detection and counting
- Statistical comparisons between groups
- Custom metric definitions
- Batch export of all visualizations
