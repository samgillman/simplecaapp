# Phase 1: Theme System Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Establish unified theme system with design tokens that immediately improves visual consistency across the entire SimpleCa²⁺ application.

**Architecture:** Create centralized theme.R module containing all design tokens (colors, typography, spacing, shadows) as R variables and CSS. Apply global styles in app.R to standardize appearance of all UI components without breaking existing functionality.

**Tech Stack:** R (Shiny), CSS, shinydashboard

---

## Task 1: Create Theme System Module

**Files:**
- Create: `R/theme.R`

**Step 1: Create theme.R with design tokens**

Create the file with complete design token definitions:

```r
# R/theme.R
# SimpleCa²⁺ Theme System - Design Tokens & Styling

# ==================== Color Palette ====================

# Primary Colors
primary_blue      <- "#0072B2"  # Brand accent, buttons, active states
primary_light     <- "#E8F4F8"  # Subtle backgrounds, hover states
primary_dark      <- "#004D7A"  # Headers, emphasis

# Neutral Grays
white             <- "#FFFFFF"  # Main background
gray_50           <- "#F8F9FA"  # Subtle backgrounds, alternating rows
gray_100          <- "#E9ECEF"  # Borders, dividers
gray_600          <- "#6C757D"  # Secondary text
gray_900          <- "#212529"  # Primary text

# Semantic Colors
success           <- "#28A745"  # Validation, positive feedback
warning           <- "#FFC107"  # Cautions, processing states
danger            <- "#DC3545"  # Errors, critical warnings
info              <- "#17A2B8"  # Helpful hints, tooltips

# ==================== Typography ====================

# Font Stacks
font_primary <- "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"
font_mono    <- "'SF Mono', Monaco, 'Cascadia Code', 'Courier New', monospace"

# Type Scale (px)
type_hero  <- "24px"  # Page titles
type_h1    <- "20px"  # Section headers
type_h2    <- "16px"  # Subsection headers
type_body  <- "14px"  # Standard text, controls
type_small <- "12px"  # Help text, captions
type_tiny  <- "11px"  # Axis labels, dense tables

# Font Weights
weight_regular  <- 400
weight_medium   <- 500
weight_semibold <- 600

# ==================== Spacing System ====================

# 4px base unit
spacing_xs  <- "4px"
spacing_sm  <- "8px"
spacing_md  <- "16px"
spacing_lg  <- "24px"
spacing_xl  <- "32px"
spacing_2xl <- "48px"

# ==================== Elevation & Shadows ====================

shadow_none    <- "none"
shadow_level_1 <- "0 1px 3px rgba(0,0,0,0.12)"    # Cards, boxes
shadow_level_2 <- "0 4px 6px rgba(0,0,0,0.1)"     # Dropdowns, popovers
shadow_level_3 <- "0 10px 20px rgba(0,0,0,0.15)"  # Modals, overlays

# ==================== Border Radius ====================

radius_sm <- "3px"   # Buttons, inputs
radius_md <- "6px"   # Cards, boxes
radius_lg <- "8px"   # Modals

# ==================== Helper Functions ====================

#' Generate CSS custom properties
#'
#' Creates a CSS string with all design tokens as custom properties
#' @return Character string containing CSS :root block
get_theme_css_vars <- function() {
  glue::glue("
    :root {{
      /* Colors - Primary */
      --color-primary-blue: {primary_blue};
      --color-primary-light: {primary_light};
      --color-primary-dark: {primary_dark};

      /* Colors - Grays */
      --color-white: {white};
      --color-gray-50: {gray_50};
      --color-gray-100: {gray_100};
      --color-gray-600: {gray_600};
      --color-gray-900: {gray_900};

      /* Colors - Semantic */
      --color-success: {success};
      --color-warning: {warning};
      --color-danger: {danger};
      --color-info: {info};

      /* Typography */
      --font-primary: {font_primary};
      --font-mono: {font_mono};
      --type-hero: {type_hero};
      --type-h1: {type_h1};
      --type-h2: {type_h2};
      --type-body: {type_body};
      --type-small: {type_small};
      --type-tiny: {type_tiny};

      /* Spacing */
      --spacing-xs: {spacing_xs};
      --spacing-sm: {spacing_sm};
      --spacing-md: {spacing_md};
      --spacing-lg: {spacing_lg};
      --spacing-xl: {spacing_xl};
      --spacing-2xl: {spacing_2xl};

      /* Shadows */
      --shadow-level-1: {shadow_level_1};
      --shadow-level-2: {shadow_level_2};
      --shadow-level-3: {shadow_level_3};

      /* Border Radius */
      --radius-sm: {radius_sm};
      --radius-md: {radius_md};
      --radius-lg: {radius_lg};
    }}
  ")
}

#' Generate unified theme CSS
#'
#' Creates complete CSS for consistent styling across the app
#' @return Character string containing all theme CSS
get_unified_theme_css <- function() {
  css_vars <- get_theme_css_vars()

  glue::glue("
    {css_vars}

    /* ==================== Global Styles ==================== */

    body {{
      font-family: var(--font-primary);
      font-size: var(--type-body);
      color: var(--color-gray-900);
      background-color: var(--color-gray-50);
    }}

    /* ==================== Typography ==================== */

    h1, .h1 {{
      font-size: var(--type-h1);
      font-weight: {weight_semibold};
      color: var(--color-gray-900);
      margin-bottom: var(--spacing-md);
    }}

    h2, .h2 {{
      font-size: var(--type-h2);
      font-weight: {weight_semibold};
      color: var(--color-gray-900);
      margin-bottom: var(--spacing-sm);
    }}

    h3, h4, h5, h6 {{
      font-weight: {weight_medium};
      color: var(--color-gray-900);
    }}

    /* ==================== Boxes & Containers ==================== */

    .box {{
      background: var(--color-white);
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-md);
      box-shadow: var(--shadow-level-1);
      margin-bottom: var(--spacing-md);
    }}

    .box-header {{
      background: var(--color-primary-dark);
      color: var(--color-white);
      font-weight: {weight_semibold};
      padding: var(--spacing-md);
      border-radius: var(--radius-md) var(--radius-md) 0 0;
    }}

    .box-body {{
      padding: var(--spacing-md);
    }}

    /* Override shinydashboard box colors for consistency */
    .box.box-solid.box-primary > .box-header {{
      background: var(--color-primary-dark);
      color: var(--color-white);
    }}

    /* ==================== Buttons ==================== */

    .btn {{
      font-weight: {weight_medium};
      border-radius: var(--radius-sm);
      padding: var(--spacing-sm) var(--spacing-md);
      transition: all 150ms ease-in-out;
    }}

    .btn-primary {{
      background-color: var(--color-primary-blue);
      border-color: var(--color-primary-blue);
      color: var(--color-white);
    }}

    .btn-primary:hover {{
      background-color: var(--color-primary-dark);
      border-color: var(--color-primary-dark);
    }}

    .btn-default {{
      background-color: var(--color-white);
      border-color: var(--color-primary-blue);
      color: var(--color-primary-blue);
    }}

    .btn-default:hover {{
      background-color: var(--color-primary-light);
      border-color: var(--color-primary-blue);
    }}

    /* ==================== Form Controls ==================== */

    .form-group {{
      margin-bottom: var(--spacing-md);
    }}

    .form-group label {{
      font-size: var(--type-small);
      font-weight: {weight_medium};
      color: var(--color-gray-900);
      margin-bottom: var(--spacing-xs);
    }}

    .form-control {{
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-sm);
      padding: 6px 12px;
      font-size: var(--type-body);
    }}

    .form-control:focus {{
      border-color: var(--color-primary-blue);
      box-shadow: 0 0 0 2px rgba(0, 114, 178, 0.1);
      outline: none;
    }}

    select.form-control {{
      appearance: none;
      background-image: url('data:image/svg+xml;charset=UTF-8,%3csvg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"%3e%3cpolyline points=\"6 9 12 15 18 9\"%3e%3c/polyline%3e%3c/svg%3e');
      background-repeat: no-repeat;
      background-position: right 8px center;
      background-size: 16px;
      padding-right: 32px;
    }}

    /* ==================== Sidebar ==================== */

    .sidebar {{
      background: var(--color-white);
      border-right: 1px solid var(--color-gray-100);
    }}

    .sidebar-menu > li > a {{
      color: var(--color-gray-900);
      font-weight: {weight_regular};
      padding: var(--spacing-sm) var(--spacing-md);
    }}

    .sidebar-menu > li.active > a {{
      background-color: var(--color-primary-light);
      color: var(--color-primary-dark);
      font-weight: {weight_medium};
      border-left: 3px solid var(--color-primary-blue);
    }}

    .sidebar-menu > li > a:hover {{
      background-color: var(--color-gray-50);
    }}

    /* ==================== Dashboard Header ==================== */

    .main-header .logo {{
      background-color: var(--color-primary-blue);
      color: var(--color-white);
      font-weight: {weight_semibold};
    }}

    .main-header .navbar {{
      background-color: var(--color-primary-blue);
    }}

    /* ==================== Tables ==================== */

    table {{
      font-size: var(--type-body);
    }}

    table th {{
      font-weight: {weight_semibold};
      background-color: var(--color-gray-50);
      border-bottom: 2px solid var(--color-gray-100);
    }}

    table td {{
      border-bottom: 1px solid var(--color-gray-100);
    }}

    /* ==================== Loading Spinners ==================== */

    .spinner {{
      color: var(--color-primary-blue);
    }}

    /* ==================== Utility Classes ==================== */

    .text-muted {{
      color: var(--color-gray-600);
    }}

    .text-primary {{
      color: var(--color-primary-blue);
    }}

    .bg-light {{
      background-color: var(--color-gray-50);
    }}

    .border {{
      border: 1px solid var(--color-gray-100);
    }}

    .rounded {{
      border-radius: var(--radius-md);
    }}

    .shadow {{
      box-shadow: var(--shadow-level-1);
    }}
  ")
}
```

**Step 2: Verify theme.R loads without errors**

Run:
```bash
Rscript -e "source('R/theme.R'); cat('Theme loaded successfully\n')"
```

Expected output:
```
Theme loaded successfully
```

**Step 3: Commit theme module**

```bash
git add R/theme.R
git commit -m "feat: add theme system with design tokens

- Create R/theme.R with complete design token definitions
- Colors: primary blue palette, grays, semantic colors
- Typography: font stacks, type scale, weights
- Spacing: 4px base unit system
- Shadows and border radius tokens
- Helper functions: get_theme_css_vars(), get_unified_theme_css()
- Complete CSS for global styling consistency"
```

---

## Task 2: Apply Theme to App

**Files:**
- Modify: `app.R` (lines ~80-100 in `tags$head` section)

**Step 1: Update app.R to source theme and inject CSS**

Find the `tags$head` section in app.R (around line 85) and update it:

```r
# In app.R, within dashboardBody(), update tags$head section:

tags$head(
  # Inject unified theme CSS
  tags$style(HTML(get_unified_theme_css())),

  # Keep existing custom CSS for specific overrides
  tags$style(HTML("
    /* Hide all top-right Shiny interface elements */
    .navbar-nav.navbar-right { display: none !important; }
    .navbar-custom-menu { display: none !important; }
    .navbar-right { display: none !important; }
    .nav.navbar-nav.navbar-right { display: none !important; }

    /* Hide showcase mode and other controls */
    #shiny-disconnected-overlay { display: none !important; }
    .shiny-disconnected-overlay { display: none !important; }

    /* Remove any question mark or help toggles */
    .fa-question-circle { display: none !important; }
    .help-block { display: none !important; }
  "))
)
```

**Step 2: Test app loads with new theme**

Run:
```bash
Rscript -e "source('app.R', local = TRUE); cat('App with theme loaded successfully\n')" 2>&1 | grep -E "(✓|✗|Error|successfully)"
```

Expected output:
```
✓ Loaded: mod_export.R
✓ Loaded: mod_heatmap.R
...
✓ Loaded: theme.R
✓ Loaded: utils.R
App with theme loaded successfully
```

**Step 3: Manual visual verification**

Start the app and verify theme is applied:
```bash
Rscript -e "shiny::runApp(port = 3838)"
```

Checklist to verify in browser:
- [ ] Boxes have subtle shadows and rounded corners
- [ ] Primary buttons are blue (#0072B2)
- [ ] Typography looks clean with consistent font
- [ ] Sidebar menu items have hover states
- [ ] Active tab has blue left border
- [ ] Form inputs have blue focus outline
- [ ] All existing functionality still works

**Step 4: Commit theme integration**

```bash
git add app.R
git commit -m "feat: integrate theme system into app

- Source theme.R in app
- Inject unified theme CSS via get_unified_theme_css()
- Retain existing custom CSS for specific overrides
- All modules now benefit from consistent styling"
```

---

## Task 3: Standardize Box Styling Across Modules

**Files:**
- Modify: `R/mod_time_course.R` (box definitions)
- Modify: `R/mod_heatmap.R` (box definitions)
- Modify: `R/mod_metrics.R` (box definitions)
- Modify: `R/mod_load_data.R` (box definitions)
- Modify: `R/mod_preproc.R` (box definitions)
- Modify: `R/mod_tables.R` (box definitions)

**Step 1: Update Time Course module boxes**

In `R/mod_time_course.R`, ensure all boxes use `status = "primary"` and `solidHeader = TRUE`:

```r
# Around line 8 in mod_time_course_ui
box(
  title = "Time Course",
  status = "primary",
  solidHeader = TRUE,
  width = 12,
  # ... rest of box content
)
```

Verify no custom inline styles that override theme.

**Step 2: Update Heatmap module boxes**

In `R/mod_heatmap.R`, standardize boxes:

```r
# Controls box (around line 7)
box(
  title = "Controls",
  status = "primary",
  solidHeader = TRUE,
  width = 4,
  collapsible = FALSE,
  # ... content
)

# Heatmap display box (around line 41)
box(
  title = "Heatmap",
  status = "primary",
  solidHeader = TRUE,
  width = 8,
  collapsible = FALSE,
  # ... content
)
```

**Step 3: Update Metrics module boxes**

In `R/mod_metrics.R`, standardize all box definitions to use theme.

**Step 4: Update remaining modules**

Apply same standardization to:
- `R/mod_load_data.R`
- `R/mod_preproc.R`
- `R/mod_tables.R`

Pattern: Ensure all boxes use `status = "primary"`, `solidHeader = TRUE`, and remove any inline style overrides that conflict with theme.

**Step 5: Test all modules load correctly**

Run:
```bash
Rscript -e "source('app.R', local = TRUE); cat('All modules loaded\n')" 2>&1 | tail -5
```

Expected: No errors, all modules loaded.

**Step 6: Visual verification**

Start app and click through each tab:
```bash
Rscript -e "shiny::runApp(port = 3838)"
```

Verify each module:
- [ ] Load Data - consistent box styling
- [ ] Processed Data - consistent styling
- [ ] Time Course - consistent styling
- [ ] Heatmap - consistent styling
- [ ] Metrics - consistent styling
- [ ] Tables - consistent styling
- [ ] Export - consistent styling
- [ ] Help - consistent styling

**Step 7: Commit standardized boxes**

```bash
git add R/mod_*.R
git commit -m "refactor: standardize box styling across all modules

- Update all boxes to use status='primary' and solidHeader=TRUE
- Remove inline style overrides that conflict with theme
- Ensures consistent appearance across all tabs
- Verified all modules load and display correctly"
```

---

## Task 4: Update Button Styling

**Files:**
- Modify: `R/mod_time_course.R` (buttons)
- Modify: `R/mod_heatmap.R` (buttons)
- Modify: `R/mod_metrics.R` (buttons)
- Modify: `R/mod_export.R` (buttons)

**Step 1: Standardize action buttons in Time Course**

In `R/mod_time_course.R`, update action buttons to use theme classes:

```r
# Around line 11, update the settings toggle button
actionButton(
  ns("toggle_settings"),
  "⚙️ Graph Settings",
  class = "btn-primary",  # Let theme handle styling
  style = "margin-bottom: 15px;"  # Keep only positional styling
)
```

Remove background-color and color from inline styles - let theme CSS handle it.

**Step 2: Standardize download buttons**

Update download buttons across modules to use consistent styling:

```r
# Pattern for download buttons
downloadButton(
  ns("dl_timecourse_plot_local"),
  "Download Time Course",
  class = "btn-primary"
)
```

**Step 3: Update buttons in Heatmap, Metrics, and Export modules**

Apply same pattern:
- Remove inline `style` attributes with colors
- Add `class = "btn-primary"` for primary actions
- Add `class = "btn-default"` for secondary actions
- Keep only positional/spacing inline styles

**Step 4: Test buttons render correctly**

Visual check:
```bash
Rscript -e "shiny::runApp(port = 3838)"
```

Verify:
- [ ] Primary buttons are blue with white text
- [ ] Hover state darkens to primary-dark blue
- [ ] Download buttons styled consistently
- [ ] Settings toggle buttons look good

**Step 5: Commit button styling updates**

```bash
git add R/mod_time_course.R R/mod_heatmap.R R/mod_metrics.R R/mod_export.R
git commit -m "refactor: standardize button styling using theme

- Remove inline color styles from buttons
- Add theme CSS classes (btn-primary, btn-default)
- Consistent button appearance across all modules
- Retain only positional styling in inline styles"
```

---

## Task 5: Typography Consistency

**Files:**
- Modify: `R/mod_help.R` (text formatting)
- Modify: `R/mod_metrics_explained.R` (text formatting)
- Modify: `app.R` (header title)

**Step 1: Update Help module typography**

In `R/mod_help.R`, ensure headers use semantic HTML tags (h1, h2, h3) instead of styled spans:

```r
# Replace styled spans with proper headers
h1("SimpleCa²⁺ User Guide"),
h2("Getting Started"),
h3("Data Format Requirements"),
# ... etc
```

Let theme CSS handle font sizes and weights.

**Step 2: Update Metrics Explained typography**

In `R/mod_metrics_explained.R`, use proper header tags:

```r
h2("Calcium Imaging Metrics Explained"),
h3("Peak ΔF/F₀"),
p("Description of metric..."),
# ... etc
```

**Step 3: Ensure consistent spacing**

Add utility classes for spacing where needed:

```r
div(style = "margin-bottom: 16px;",  # Use spacing_md from theme
  h3("Metric Name"),
  p("Description")
)
```

**Step 4: Visual verification**

```bash
Rscript -e "shiny::runApp(port = 3838)"
```

Check:
- [ ] Help tab has consistent typography
- [ ] Metrics Explained tab has consistent headers
- [ ] Spacing looks balanced
- [ ] Font sizes follow type scale

**Step 5: Commit typography updates**

```bash
git add R/mod_help.R R/mod_metrics_explained.R
git commit -m "refactor: improve typography consistency

- Use semantic HTML headers (h1, h2, h3) in Help module
- Update Metrics Explained with proper header hierarchy
- Let theme CSS handle font sizes and weights
- Consistent spacing using theme tokens"
```

---

## Task 6: Final Phase 1 Verification

**Files:**
- None (testing only)

**Step 1: Comprehensive visual test**

Start the app:
```bash
Rscript -e "shiny::runApp(port = 3838)"
```

Complete checklist for each module:

**Load Data:**
- [ ] Box styling consistent
- [ ] Buttons styled correctly
- [ ] Form inputs have proper focus states
- [ ] File upload UI looks clean

**Processed Data:**
- [ ] Table styling consistent
- [ ] Download buttons match theme

**Time Course:**
- [ ] Box and buttons themed
- [ ] Settings toggle works
- [ ] Plot renders correctly
- [ ] Export options styled

**Heatmap:**
- [ ] Controls box styled
- [ ] Heatmap display box styled
- [ ] All inputs and selects themed
- [ ] Plot renders correctly

**Metrics:**
- [ ] Box styling consistent
- [ ] Plots render correctly
- [ ] Export buttons themed

**Metrics Explained:**
- [ ] Typography hierarchy clear
- [ ] Spacing consistent

**Tables:**
- [ ] Table styling applied
- [ ] Borders and headers consistent

**Export:**
- [ ] All export options styled
- [ ] Download buttons consistent

**Help:**
- [ ] Typography clean and readable
- [ ] Headers properly sized
- [ ] Content well-spaced

**Step 2: Test with sample data**

Load a sample dataset and verify:
- [ ] Data loads without errors
- [ ] Processing works correctly
- [ ] All plots generate successfully
- [ ] Exports work (download a PNG/PDF)
- [ ] No console errors in browser dev tools

**Step 3: Cross-browser check**

If possible, test in:
- [ ] Chrome
- [ ] Firefox
- [ ] Safari

Verify theme appears consistently.

**Step 4: Performance check**

- [ ] App loads quickly (no slowdown from CSS)
- [ ] No layout shifts or flashing
- [ ] Interactions feel smooth

**Step 5: Document completion**

Create a brief summary of what was accomplished:

```bash
cat > docs/plans/2025-11-02-phase1-completion-notes.md << 'EOF'
# Phase 1: Theme System - Completion Notes

**Date Completed:** 2025-11-02

## What Was Implemented

1. **Theme System (R/theme.R)**
   - Complete design token definitions
   - CSS custom properties
   - Helper functions for theme generation

2. **Global Styling**
   - Applied unified CSS to app.R
   - All components now use theme tokens

3. **Module Standardization**
   - All boxes use consistent status/header styling
   - Buttons standardized with theme classes
   - Typography uses semantic HTML + theme CSS

4. **Verification**
   - All modules tested and working
   - Visual consistency achieved across entire app
   - No regressions in functionality

## Before/After Comparison

**Before:**
- Inconsistent box styling across modules
- Ad-hoc button colors and sizes
- Mixed typography approaches
- No central theme management

**After:**
- Unified visual language throughout app
- Consistent component styling
- Professional, clean appearance
- Easy to maintain and extend

## Next Steps

Ready for Phase 2: Component Library
- Create reusable accordion components
- Build standard form control wrappers
- Retrofit Time Course module as proof-of-concept
EOF

git add docs/plans/2025-11-02-phase1-completion-notes.md
git commit -m "docs: add Phase 1 completion notes"
```

**Step 6: Create summary for user**

After all verification passes, summarize what was accomplished and confirm readiness for Phase 2.

---

## Success Criteria

Phase 1 is complete when:

- ✅ `R/theme.R` exists with all design tokens
- ✅ Theme CSS injected into app.R
- ✅ All modules use standardized box styling
- ✅ All buttons use theme classes
- ✅ Typography is consistent across modules
- ✅ App loads without errors
- ✅ All existing functionality preserved
- ✅ Visual appearance is noticeably more polished
- ✅ No performance degradation
- ✅ All changes committed with clear messages

---

## Rollback Plan

If issues arise:

```bash
# Return to commit before Phase 1 started
git log --oneline -10  # Find commit before theme work
git reset --hard <commit-hash>

# Or revert specific commits
git revert <commit-hash>
```

---

## Notes for Implementation

**Key Principles:**
- **No functionality changes** - Only visual/styling updates
- **Incremental commits** - One logical change per commit
- **Test after each task** - Don't accumulate changes
- **YAGNI** - Only implement what's specified, no extras
- **DRY** - Use theme tokens, not hardcoded values

**Common Pitfalls:**
- Don't add new features while styling
- Don't refactor code structure unnecessarily
- Don't change module logic
- Don't skip visual verification steps

**Dependencies:**
- Ensure `glue` package available (for string interpolation in theme.R)
- Check if any modules have custom CSS that might conflict

---

**End of Phase 1 Implementation Plan**
