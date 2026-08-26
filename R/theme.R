# R/theme.R
# SimpleCa²⁺ Theme System - Design Tokens & Styling

# App version shown in the sidebar so users can tell which build they see
# (Shinylive/service-worker caching makes stale builds common)
SIMPLECA_VERSION <- "1.15.0"

# ==================== Color Palette ====================

# Primary Colors
primary_blue <- "#0072B2" # Brand accent, buttons, active states
primary_light <- "#E8F4F8" # Subtle backgrounds, hover states
primary_dark <- "#004D7A" # Headers, emphasis

# Neutral Grays
white <- "#FFFFFF" # Main background
gray_50 <- "#F7F9FB" # Subtle backgrounds, alternating rows
gray_100 <- "#DEE4EA" # Borders, dividers
gray_600 <- "#65717D" # Secondary text
gray_900 <- "#1F2933" # Primary text

# Semantic Colors
success <- "#28A745" # Validation, positive feedback
warning <- "#FFC107" # Cautions, processing states
danger <- "#DC3545" # Errors, critical warnings
info <- "#17A2B8" # Helpful hints, tooltips

# ==================== Typography ====================

# Font Stacks
font_primary <- "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"
font_mono <- "'SF Mono', Monaco, 'Cascadia Code', 'Courier New', monospace"

# Type Scale (px)
type_hero <- "24px" # Page titles
type_h1 <- "20px" # Section headers
type_h2 <- "16px" # Subsection headers
type_body <- "14px" # Standard text, controls
type_small <- "12px" # Help text, captions
type_tiny <- "11px" # Axis labels, dense tables

# Font Weights
weight_regular <- 400
weight_medium <- 500
weight_semibold <- 600

# ==================== Spacing System ====================

# 4px base unit
spacing_xs <- "4px"
spacing_sm <- "8px"
spacing_md <- "16px"
spacing_lg <- "24px"
spacing_xl <- "32px"
spacing_2xl <- "48px"

# ==================== Elevation & Shadows ====================

shadow_none <- "none"
shadow_level_1 <- "0 1px 3px rgba(31,41,51,0.10)" # Cards, boxes
shadow_level_2 <- "0 5px 14px rgba(31,41,51,0.12)" # Dropdowns, popovers
shadow_level_3 <- "0 14px 32px rgba(31,41,51,0.16)" # Modals, overlays

# ==================== Border Radius ====================

radius_sm <- "5px" # Buttons, inputs
radius_md <- "8px" # Cards, boxes
radius_lg <- "12px" # Modals

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
      line-height: 1.45;
      color: var(--color-gray-900);
      background-color: var(--color-gray-50);
      -webkit-font-smoothing: antialiased;
      -moz-osx-font-smoothing: grayscale;
    }}

    button, input, select, textarea {{
      font: inherit;
    }}

    ::selection {{
      background: var(--color-primary-light);
      color: var(--color-primary-dark);
    }}

    /* ==================== Typography ==================== */

    h1, .h1 {{
      font-size: var(--type-h1);
      font-weight: {weight_semibold};
      color: var(--color-gray-900);
      line-height: 1.25;
      margin: 0 0 var(--spacing-md);
    }}

    h2, .h2 {{
      font-size: var(--type-h2);
      font-weight: {weight_semibold};
      color: var(--color-gray-900);
      line-height: 1.3;
      margin: 0 0 var(--spacing-sm);
    }}

    h3, h4, h5, h6 {{
      font-weight: {weight_medium};
      color: var(--color-gray-900);
      line-height: 1.3;
    }}

    p {{
      line-height: 1.5;
    }}

    /* ==================== Boxes & Containers ==================== */

    .box {{
      background: var(--color-white);
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-md);
      box-shadow: var(--shadow-level-1);
      margin-bottom: var(--spacing-md);
      overflow: visible;
    }}

    .box-header {{
      background: var(--color-primary-blue);
      color: var(--color-white);
      font-weight: {weight_semibold};
      font-size: var(--type-body);
      padding: 10px 14px;
      min-height: 40px;
      display: flex;
      align-items: center;
      border-radius: var(--radius-md) var(--radius-md) 0 0;
    }}

    .box-header .box-title {{
      font-size: var(--type-body) !important;
      line-height: 20px;
    }}

    /* Ensure icons in box headers are properly spaced */
    .box-header .fa, .box-header .fas, .box-header .far {{
      margin-right: 6px;
    }}

    .box-body {{
      padding: var(--spacing-md);
      overflow: visible;
      min-width: 0;
    }}

    .box-header > .box-tools {{
      top: 7px;
      right: 10px;
    }}

    /* Override shinydashboard box colors for consistency */
    .box.box-solid.box-primary > .box-header {{
      background: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
    }}

    .box.box-primary > .box-header {{
      background: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
    }}

    /* Ensure box titles are white text */
    .box-header > .box-title {{
      color: var(--color-white) !important;
    }}

    .box.box-solid.box-primary > .box-header > .box-title,
    .box.box-primary > .box-header > .box-title {{
      color: var(--color-white) !important;
    }}

    /* Info status boxes should also use our primary blue for consistency */
    .box.box-solid.box-info > .box-header,
    .box.box-info > .box-header {{
      background: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
    }}

    .box.box-solid.box-info > .box-header > .box-title,
    .box.box-info > .box-header > .box-title {{
      color: var(--color-white) !important;
    }}

    /* ==================== Buttons ==================== */

    .btn {{
      font-weight: {weight_medium};
      border-radius: var(--radius-sm);
      padding: var(--spacing-sm) var(--spacing-md);
      min-height: 36px;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      gap: 6px;
      line-height: 1.2;
      white-space: normal;
      transition:
        background-color 150ms ease,
        border-color 150ms ease,
        color 150ms ease,
        box-shadow 150ms ease,
        transform 150ms ease;
    }}

    .btn-primary {{
      background-color: var(--color-primary-blue);
      border-color: var(--color-primary-blue);
      color: var(--color-white);
    }}

    .btn-primary:hover {{
      background-color: var(--color-primary-dark);
      border-color: var(--color-primary-dark);
      color: var(--color-white);
    }}

    .btn-default {{
      background-color: var(--color-white);
      border-color: var(--color-primary-blue);
      color: var(--color-primary-blue);
    }}

    .btn-default:hover {{
      background-color: var(--color-primary-light);
      border-color: var(--color-primary-blue);
      color: var(--color-primary-dark);
    }}

    /*
     * Shiny's actionButton() always adds btn-default; when a caller also
     * passes btn-primary the default (outline) styling must not win the
     * cascade, or every primary CTA renders as a secondary button.
     */
    .btn.btn-default.btn-primary {{
      background-color: var(--color-primary-blue);
      border-color: var(--color-primary-blue);
      color: var(--color-white);
    }}

    .btn.btn-default.btn-primary:hover,
    .btn.btn-default.btn-primary:focus {{
      background-color: var(--color-primary-dark);
      border-color: var(--color-primary-dark);
      color: var(--color-white);
    }}

    .btn:focus,
    .btn.focus,
    .btn:focus-visible {{
      outline: none;
      box-shadow: 0 0 0 3px rgba(0, 114, 178, 0.22);
    }}

    .btn.disabled,
    .btn[disabled],
    fieldset[disabled] .btn {{
      opacity: 0.58;
      cursor: not-allowed;
      box-shadow: none;
      transform: none;
    }}

    .btn-sm {{
      min-height: 32px;
      padding: 6px 10px;
      font-size: var(--type-small);
    }}

    /* ==================== Form Controls ==================== */

    .form-group {{
      margin-bottom: 12px;
    }}

    .form-group label {{
      font-size: var(--type-small);
      font-weight: {weight_medium};
      color: var(--color-gray-900);
      margin-bottom: 6px;
    }}

    .form-control {{
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-sm);
      padding: 6px 12px;
      font-size: var(--type-body);
      min-height: 36px;
      line-height: 1.4;
      box-shadow: none;
      transition: border-color 150ms ease, box-shadow 150ms ease;
    }}

    .form-control:focus {{
      border-color: var(--color-primary-blue);
      box-shadow: 0 0 0 2px rgba(0, 114, 178, 0.1);
      outline: none;
    }}

    textarea.form-control {{
      min-height: 84px;
    }}

    input[type='checkbox'],
    input[type='radio'] {{
      accent-color: var(--color-primary-blue);
    }}

    .selectize-control.single .selectize-input,
    .selectize-control.multi .selectize-input {{
      min-height: 36px;
      border-color: var(--color-gray-100);
      border-radius: var(--radius-sm);
      box-shadow: none;
      padding: 7px 12px;
    }}

    .selectize-control.single .selectize-input.focus,
    .selectize-control.multi .selectize-input.focus {{
      border-color: var(--color-primary-blue);
      box-shadow: 0 0 0 2px rgba(0, 114, 178, 0.1);
    }}

    select.form-control {{
      appearance: none;
      background-image: url('data:image/svg+xml;charset=UTF-8,%3csvg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"%3e%3cpolyline points=\"6 9 12 15 18 9\"%3e%3c/polyline%3e%3c/svg%3e');
      background-repeat: no-repeat;
      background-position: right 8px center;
      background-size: 16px;
      padding-right: 32px;
    }}

    /* Selectize gets the SAME chevron as native selects (its default is a
       small solid triangle, which read as a different control style) */
    .selectize-control.single .selectize-input::after,
    .selectize-control.single .selectize-input.dropdown-active::after {{
      display: none !important;
    }}

    .selectize-control.single .selectize-input {{
      background-image: url('data:image/svg+xml;charset=UTF-8,%3csvg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"%3e%3cpolyline points=\"6 9 12 15 18 9\"%3e%3c/polyline%3e%3c/svg%3e');
      background-repeat: no-repeat;
      background-position: right 8px center;
      background-size: 16px;
      padding-right: 32px;
    }}

    /* File-input text field: match the rounded corners and 13px size of
       every other control (bootstrap strips the radius inside input-groups) */
    .input-group .btn-file + input[type='text'] {{
      border-radius: 0 var(--radius-sm) var(--radius-sm) 0 !important;
      font-size: 13px;
    }}

    /* ==================== Load & Process Steps ==================== */
    /* Keep the workflow either three columns or one unambiguous vertical
       sequence. Flex wrapping previously produced a misleading 1–2 / 3
       two-row arrangement at intermediate viewport widths. */
    .load-steps {{
      display: grid;
      grid-template-columns: repeat(3, minmax(0, 1fr));
      gap: 28px;
      align-items: start;
    }}

    .control-cols {{
      display: flex;
      gap: 28px;
      align-items: stretch;
      flex-wrap: wrap;
    }}

    .load-step {{
      min-width: 0;
    }}

    .control-col {{
      flex: 1 1 260px;
      min-width: 240px;
    }}

    .load-step + .load-step {{
      border-left: 1px solid var(--color-gray-100);
      padding-left: 28px;
    }}

    .control-col + .control-col {{
      border-left: 1px solid var(--color-gray-100);
      padding-left: 28px;
    }}

    @media (max-width: 1199px) {{
      .load-steps {{
        grid-template-columns: 1fr;
        gap: 16px;
      }}

      .load-step + .load-step {{
        border-left: none;
        padding-left: 0;
        border-top: 1px solid var(--color-gray-100);
        padding-top: 16px;
      }}
    }}

    @media (max-width: 991px) {{
      .control-col + .control-col {{
        border-left: none;
        padding-left: 0;
        border-top: 1px solid var(--color-gray-100);
        padding-top: 16px;
      }}
    }}

    .load-step-title,
    .control-col-title {{
      display: flex;
      align-items: center;
      gap: 8px;
      font-weight: {weight_semibold};
      font-size: 13px;
      color: var(--color-gray-900);
      margin-bottom: 12px;
    }}

    .control-col-title .fa,
    .control-col-title .fas {{
      color: var(--color-gray-600);
    }}

    .load-step-num {{
      display: inline-flex;
      align-items: center;
      justify-content: center;
      width: 22px;
      height: 22px;
      border-radius: 50%;
      background: var(--color-primary-blue);
      color: var(--color-white);
      font-size: 12px;
      font-weight: {weight_semibold};
      flex-shrink: 0;
    }}

    .column-mapping-file {{
      background: var(--color-gray-50);
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-sm);
      padding: 12px;
      margin: 10px 0;
    }}

    .column-mapping-title {{
      display: flex;
      align-items: center;
      gap: 7px;
      min-width: 0;
      margin-bottom: 3px;
      font-size: 12px;
      font-weight: {weight_semibold};
      overflow-wrap: anywhere;
    }}

    .column-mapping-detection {{
      margin: 4px 0 0;
      padding: 7px 9px;
      border-left: 3px solid var(--color-primary-blue);
      background: var(--color-white);
      color: var(--color-gray-700);
      font-size: 11px;
      line-height: 1.45;
    }}

    /* Slim post-processing results bar */
    .results-bar {{
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 16px;
      flex-wrap: wrap;
      background: var(--color-white);
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-md);
      box-shadow: var(--shadow-level-1);
      padding: 12px 18px;
      margin-bottom: var(--spacing-md);
    }}

    .results-bar-stats {{
      display: flex;
      align-items: center;
      gap: 10px;
      font-size: 14px;
    }}

    /* File Input Styling - Shiny specific */
    .form-group.shiny-input-container {{
      margin-bottom: var(--spacing-md);
    }}

    /* Style the input group for file inputs */
    .input-group {{
      display: flex;
      align-items: stretch;
      width: 100%;
      position: relative;
    }}

    /* Fix the input-group-btn label container that wraps the file button */
    .input-group-btn,
    .input-group-prepend {{
      display: flex !important;
      flex-shrink: 0;
      width: auto !important;
      min-width: fit-content !important;
      overflow: visible !important;
    }}

    /* Style the file input button (Shiny wraps it in a span with btn-file class) */
    .btn-file {{
      background-color: var(--color-primary-blue) !important;
      border-color: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
      border-radius: var(--radius-sm) 0 0 var(--radius-sm);
      padding: 6px 16px !important;
      font-size: var(--type-body);
      font-weight: {weight_medium};
      cursor: pointer !important;
      transition: background-color 150ms ease;
      white-space: nowrap;
      position: relative;
      display: inline-flex !important;
      align-items: center;
      min-height: 34px;
      min-width: 80px !important;
      overflow: visible !important;
    }}

    .btn-file:hover {{
      background-color: var(--color-primary-dark) !important;
      border-color: var(--color-primary-dark) !important;
    }}

    /* Override Shiny's default file input positioning that moves it off-screen */
    /* This is critical - Shiny adds inline styles that position the input at -99999px */
    .btn-file input[type='file'] {{
      position: absolute !important;
      top: 0 !important;
      left: 0 !important;
      right: 0 !important;
      bottom: 0 !important;
      width: 100% !important;
      height: 100% !important;
      opacity: 0 !important;
      cursor: pointer !important;
      z-index: 10 !important;
    }}

    /* Style the text input showing filename */
    .input-group input[type='text'][readonly] {{
      background-color: var(--color-gray-50);
      border: 1px solid var(--color-gray-100);
      border-left: none;
      border-radius: 0 var(--radius-sm) var(--radius-sm) 0;
      padding: 6px 12px;
      font-size: var(--type-body);
      color: var(--color-gray-600);
      flex: 1;
    }}

    /* Remove margin between button and text input */
    .input-group .btn-file + input[type='text'] {{
      margin-left: 0 !important;
    }}

    /* ==================== Sidebar ==================== */

    .main-sidebar {{
      background: var(--color-white) !important;
      width: 230px !important;
      /* Keep menu content below fixed header height so top tabs are not hidden */
      padding-top: 50px !important;
      margin-top: 0 !important;
      overflow-y: auto;
      box-shadow: 1px 0 0 var(--color-gray-100);
    }}

    .sidebar {{
      background: var(--color-white) !important;
      border-right: 1px solid var(--color-gray-100);
      padding-bottom: 0 !important;
      padding-top: 0 !important;
      margin-top: 0 !important;
      height: 100%;
    }}

    /* Keep menu flush while preserving visibility below header offset */
    .sidebar-menu {{
      padding-top: 0 !important;
      margin-top: 0 !important;
    }}

    .sidebar-menu > li > a {{
      color: var(--color-gray-900) !important;
      font-weight: {weight_regular};
      font-size: 13px !important;
      padding: 9px 16px !important;
      border-left: 3px solid transparent !important;
      transition: background-color 150ms ease, color 150ms ease, border-color 150ms ease;
    }}

    .sidebar-menu > li > a > .fa, .sidebar-menu > li > a > .fas, .sidebar-menu > li > a > .far {{
      width: 20px !important;
      text-align: center;
      margin-right: 8px;
      font-size: 13px;
      color: var(--color-gray-600);
    }}

    .sidebar-menu > li.active > a {{
      background-color: var(--color-primary-light) !important;
      color: var(--color-primary-dark) !important;
      font-weight: {weight_medium};
      border-left: 3px solid var(--color-primary-blue) !important;
    }}

    .sidebar-menu > li.active > a > .fa,
    .sidebar-menu > li.active > a > .fas,
    .sidebar-menu > li.active > a > .far {{
      color: var(--color-primary-blue) !important;
    }}

    .sidebar-menu > li > a:hover {{
      background-color: var(--color-gray-50) !important;
      color: var(--color-gray-900) !important;
    }}

    .sidebar-menu .treeview-menu {{
      background: var(--color-white) !important;
      padding: 4px 0 6px !important;
    }}

    .sidebar-menu .treeview-menu > li > a {{
      color: var(--color-gray-600) !important;
      font-size: var(--type-small);
      padding: 8px 16px 8px 44px !important;
    }}

    .sidebar-menu .treeview-menu > li.active > a,
    .sidebar-menu .treeview-menu > li > a:hover {{
      color: var(--color-primary-dark) !important;
      background: var(--color-primary-light) !important;
    }}

    /* Sidebar section headers (PRE-NORMALIZED DATA, NORMALIZED DATA, GENERAL) */
    .sidebar-menu > li.header {{
      background: transparent !important;
      color: var(--color-gray-600) !important;
      font-size: 10px !important;
      font-weight: {weight_semibold} !important;
      letter-spacing: 0.8px !important;
      text-transform: uppercase !important;
      padding: 14px 16px 6px 16px !important;
      border-bottom: none !important;
      line-height: 1.2 !important;
    }}

    /* First section header needs less top padding */
    .sidebar-menu > li.header:first-child {{
      padding-top: 10px !important;
    }}

    /* Remove any sidebar footer or extra elements */
    .sidebar-footer {{
      display: none !important;
    }}

    /* On phones the open sidebar overlays content; give it depth so the
       pushed-aside content reads as background, not broken layout */
    @media (max-width: 767px) {{
      .main-sidebar {{
        box-shadow: var(--shadow-level-3);
      }}
    }}

    /* Plot images must never bleed past their box */
    .shiny-plot-output {{
      overflow: hidden;
    }}

    /* Utility: strip the trailing form-group margin inside tight flex rows */
    .flex-tight .form-group {{
      margin-bottom: 0;
    }}

    /* ==================== Dashboard Header ==================== */

    .main-header .logo {{
      background-color: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
      font-weight: {weight_semibold};
      font-size: 18px;
      letter-spacing: 0.3px;
      width: 230px !important;
      max-width: 230px !important;
      min-width: 230px !important;
      text-align: center !important;
      padding: 0 !important;
      margin: 0 !important;
      height: 50px !important;
      line-height: 50px !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      box-sizing: border-box !important;
    }}

    .main-header .navbar {{
      background-color: var(--color-primary-blue) !important;
      margin-left: 230px !important;
      min-height: 50px !important;
      box-shadow: 0 1px 4px rgba(31, 41, 51, 0.14);
    }}

    .sidebar-toggle {{
      display: block !important;
    }}

    .main-header .sidebar-toggle:focus-visible {{
      outline: 2px solid rgba(255, 255, 255, 0.85);
      outline-offset: -4px;
    }}

    /* ==================== Content & Layout Alignment ==================== */

    .content-wrapper {{
      background-color: var(--color-gray-50);
      margin-left: 230px !important;
      min-height: calc(100vh - 50px) !important;
    }}

    .content {{
      width: 100%;
      max-width: 1680px;
      margin: 0 auto;
      padding: clamp(14px, 1.6vw, 24px);
    }}

    /* Keep sidebar collapse behavior functional. AdminLTE fully moves the
       sidebar off-screen in this build, so collapsed content must start at
       the viewport edge rather than reserving a 50px mini-sidebar rail. */
    body.sidebar-collapse .main-header .logo {{
      width: 50px !important;
      min-width: 50px !important;
      max-width: 50px !important;
      overflow: hidden;
      font-size: 0 !important;
    }}

    body.sidebar-collapse .main-header .navbar,
    body.sidebar-collapse .content-wrapper,
    body.sidebar-collapse .main-footer {{
      margin-left: 0 !important;
    }}

    /*
     * Match shinydashboard's native mobile breakpoint. Using 991px here puts
     * the header into mobile mode while the sidebar still behaves as desktop,
     * which causes the menu to overlap tablet-width content.
     */
    @media (max-width: 767px) {{
      /* The mobile header stacks into two 50px rows (logo + navbar), so the
         sidebar needs a 100px offset — with the desktop 50px, its first
         items hide behind the header. It must also scroll on its own so
         long menus stay reachable. */
      .main-sidebar {{
        padding-top: 100px !important;
        height: 100%;
        overflow-y: auto;
        -webkit-overflow-scrolling: touch;
      }}

      .main-header .logo {{
        width: 100% !important;
        min-width: 0 !important;
        max-width: none !important;
        float: none !important;
      }}

      .main-header .navbar,
      .content-wrapper,
      .main-footer {{
        margin-left: 0 !important;
      }}

      .content {{
        padding: 12px;
      }}
    }}

    /*
     * The desktop sidebar leaves a narrow working area on small tablets.
     * Stack only each tab's top-level columns at this range; nested controls,
     * overview cards, and processing steps retain their intended grid.
     */
    @media (min-width: 768px) and (max-width: 991px) {{
      .content {{
        padding: 14px;
      }}

      .content .tab-pane > .row > [class*='col-sm-'] {{
        width: 100%;
        float: none;
      }}
    }}

    /* Ensure fluidRow has no weird margins */
    .row {{
      margin-left: 0;
      margin-right: 0;
    }}

    /* Tighter column gutters for compact layout */
    [class*='col-'] {{
      padding-left: 8px;
      padding-right: 8px;
    }}

    /* Stat card refinements */
    .stat-card {{
      transition: box-shadow 150ms ease;
    }}

    .stat-card:hover {{
      box-shadow: var(--shadow-level-2);
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

    /* A table that ends a box body must not stack its own bottom margin
       on top of the box padding (kable/bootstrap default is 20px) */
    .box-body table:last-child,
    .box-body .shiny-html-output > .table:last-child {{
      margin-bottom: 0;
    }}

    /* DataTables (DT) styling */
    .dataTables_wrapper {{
      font-size: var(--type-small);
      overflow-x: auto;
      -webkit-overflow-scrolling: touch;
    }}

    .dataTables_wrapper .dataTables_filter input {{
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-sm);
      padding: 4px 8px;
      font-size: var(--type-small);
    }}

    .dataTables_wrapper .dataTables_length select {{
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-sm);
      padding: 2px 6px;
      font-size: var(--type-small);
    }}

    /* DT header cells */
    table.dataTable thead th {{
      font-weight: {weight_semibold};
      background-color: var(--color-gray-50);
      border-bottom: 2px solid var(--color-primary-blue) !important;
      color: var(--color-gray-900);
      padding: 10px 8px;
      font-size: var(--type-small);
    }}

    /* DT body rows */
    table.dataTable tbody td {{
      padding: 8px;
      vertical-align: middle;
      border-bottom: 1px solid var(--color-gray-100);
    }}

    table.dataTable tbody tr:hover {{
      background-color: var(--color-primary-light) !important;
    }}

    /* DT stripe rows */
    table.dataTable.stripe tbody tr.odd,
    table.dataTable.display tbody tr.odd {{
      background-color: rgba(0, 0, 0, 0.015);
    }}

    /* DT pagination */
    .dataTables_wrapper .dataTables_paginate .paginate_button {{
      border: 1px solid var(--color-gray-100) !important;
      border-radius: var(--radius-sm);
      padding: 4px 10px !important;
      margin: 0 2px;
      font-size: var(--type-small);
    }}

    /* The element selector (a.) out-ranks DataTables' own
       'color: inherit !important' rule, which loads after this sheet and
       otherwise wins the tie, leaving dark-blue-on-blue page numbers */
    .dataTables_wrapper .dataTables_paginate a.paginate_button.current,
    .dataTables_wrapper .dataTables_paginate a.paginate_button.current:hover {{
      background: var(--color-primary-blue) !important;
      border-color: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
    }}

    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {{
      background: var(--color-primary-light) !important;
      border-color: var(--color-primary-blue) !important;
      color: var(--color-primary-dark) !important;
    }}

    /* DT info text */
    .dataTables_wrapper .dataTables_info {{
      font-size: var(--type-tiny);
      color: var(--color-gray-600);
      padding-top: 10px;
    }}

    /* DT top filter inputs */
    table.dataTable thead .sorting_asc::after,
    table.dataTable thead .sorting_desc::after,
    table.dataTable thead .sorting::after {{
      opacity: 0.4;
    }}

    /* DT column filters (filter = 'top') */
    .dataTables_wrapper thead input[type=search],
    .dataTables_wrapper thead select {{
      font-size: var(--type-tiny);
      padding: 2px 4px;
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-sm);
      width: 100%;
      margin-top: 4px;
    }}

    /* DT Buttons extension */
    .dataTables_wrapper .dt-buttons {{
      margin-bottom: 10px;
    }}

    .dataTables_wrapper .dt-buttons .btn {{
      font-size: var(--type-tiny) !important;
      padding: 4px 10px !important;
    }}

    /* Viewport-adaptive plot heights. Shiny's validateCssUnit() rejects
       clamp(), so the height lives on a wrapper class and the output fills
       it. Every layer between wrapper and <img> must propagate the height
       or the output measures 0 and Shiny falls back to its default. */
    .plot-viewport {{
      height: clamp(380px, calc(100vh - 200px), 620px);
    }}

    .plot-viewport-short {{
      height: clamp(340px, calc(100vh - 340px), 560px);
    }}

    /* Time Course: shorter than the generic viewport so the summary table
       under the plot starts above the fold instead of being invisible */
    .plot-viewport-tc {{
      height: clamp(380px, calc(100vh - 340px), 620px);
    }}

    /* Metrics: a header strip AND the controls accordion sit above the
       plot, so it needs the shortest viewport to end above the fold */
    .plot-viewport-compact {{
      height: clamp(360px, calc(100vh - 420px), 620px);
    }}

    .plot-viewport .shiny-spinner-output-container,
    .plot-viewport .shiny-plot-output,
    .plot-viewport .plotly.html-widget,
    .plot-viewport-short .shiny-spinner-output-container,
    .plot-viewport-short .shiny-plot-output,
    .plot-viewport-short .plotly.html-widget,
    .plot-viewport-tc .shiny-spinner-output-container,
    .plot-viewport-tc .shiny-plot-output,
    .plot-viewport-tc .plotly.html-widget,
    .plot-viewport-compact .shiny-spinner-output-container,
    .plot-viewport-compact .shiny-plot-output,
    .plot-viewport-compact .plotly.html-widget {{
      height: 100% !important;
    }}

    /* Text column matched to the -short plot viewport: same computed
       height, so a text box and a plot box in one row end on the same
       bottom edge at every window size; long content scrolls inside */
    .match-plot-short {{
      height: clamp(340px, calc(100vh - 340px), 560px);
      overflow-y: auto;
      padding-right: 6px;
    }}

    /* HTML math (formula_line/frac helpers in R/components.R) */
    .formula {{
      text-align: center;
      font-family: Georgia, 'Times New Roman', serif;
      font-size: 15px;
      color: var(--color-gray-900);
      background: var(--color-gray-50);
      border-radius: var(--radius-sm);
      padding: 8px 12px;
      margin: 8px 0;
      line-height: 1.5;
      overflow-x: auto;
    }}

    .formula .frac {{
      display: inline-flex;
      flex-direction: column;
      vertical-align: middle;
      text-align: center;
      margin: 0 3px;
      line-height: 1.3;
    }}

    .formula .frac > span {{
      display: block;
      padding: 0 6px;
    }}

    .formula .frac > span:first-child {{
      border-bottom: 1px solid currentColor;
    }}

    /* Metrics explanation styling */
    .metric-data-box {{
      background-color: var(--color-gray-50);
      padding: 15px;
      border-radius: var(--radius-sm);
      border-left: 4px solid var(--color-primary-blue);
    }}

    .metric-data-box ul {{
      margin-bottom: 0;
    }}

    .metric-result-box {{
      background-color: #d4edda;
      padding: 10px;
      border-radius: var(--radius-sm);
      margin-top: 10px;
      border: 1px solid #c3e6cb;
    }}

    .metric-result-box h5 {{
      margin: 0;
      color: #155724;
    }}

    .metric-result-box p {{
      margin: 5px 0 0 0;
      font-weight: {weight_semibold};
      color: #155724;
    }}

    .metric-section-header {{
      color: var(--color-gray-900);
      border-bottom: 2px solid var(--color-primary-blue);
      padding-bottom: 5px;
    }}

    .metric-note {{
      font-style: italic;
      color: #856404;
    }}

    /* ==================== Tabs & Navigation ==================== */

    .nav-tabs,
    .nav-pills {{
      display: flex;
      flex-wrap: wrap;
      align-items: center;
      gap: 4px;
    }}

    .nav-tabs {{
      border-bottom: 1px solid var(--color-gray-100);
    }}

    .nav-tabs > li,
    .nav-pills > li {{
      float: none;
      margin: 0;
    }}

    .nav-tabs > li > a {{
      border-radius: var(--radius-sm) var(--radius-sm) 0 0;
      color: var(--color-gray-600);
      font-size: var(--type-small);
      padding: 9px 12px;
      margin: 0;
      transition: background-color 150ms ease, border-color 150ms ease, color 150ms ease;
    }}

    .nav-tabs > li > a:hover {{
      background: var(--color-primary-light);
      border-color: transparent;
      color: var(--color-primary-dark);
    }}

    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover,
    .nav-tabs > li.active > a:focus {{
      border: 1px solid var(--color-gray-100);
      border-bottom-color: var(--color-white);
      color: var(--color-gray-900);
      font-weight: {weight_medium};
    }}

    /* Tab pills styling */
    .nav-pills > li > a {{
      border-radius: var(--radius-sm);
      color: var(--color-gray-600);
      font-size: var(--type-small);
      padding: 8px 14px;
      margin: 0;
      transition: background-color 150ms ease, color 150ms ease, box-shadow 150ms ease;
    }}

    .nav-pills > li.active > a,
    .nav-pills > li.active > a:hover,
    .nav-pills > li.active > a:focus {{
      background-color: var(--color-primary-blue);
      color: var(--color-white);
      box-shadow: 0 1px 3px rgba(0, 114, 178, 0.22);
    }}

    .nav-pills > li > a:hover {{
      background-color: var(--color-primary-light);
      color: var(--color-primary-dark);
    }}

    .nav-tabs > li > a:focus-visible,
    .nav-pills > li > a:focus-visible {{
      outline: 2px solid rgba(0, 114, 178, 0.35);
      outline-offset: 2px;
    }}

    /* ==================== Alerts ==================== */

    /* AdminLTE paints .alert-info a saturated cyan with white text that
       clashes with the app palette; use a quiet informational tint */
    .alert-info {{
      background-color: var(--color-primary-light) !important;
      border: 1px solid rgba(0, 114, 178, 0.25) !important;
      color: var(--color-primary-dark) !important;
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

    /* ==================== Accordion Component ==================== */

    .accordion-section {{
      margin-bottom: 10px;
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-md);
      background: var(--color-white);
      /* Don't use overflow hidden - it clips dropdown menus */
    }}

    .accordion-header {{
      padding: 8px 12px;
      background: var(--color-gray-50);
      cursor: pointer;
      user-select: none;
      display: flex;
      align-items: center;
      gap: var(--spacing-sm);
      min-height: 40px;
      transition: background-color 150ms ease, color 150ms ease;
      font-weight: {weight_medium};
      font-size: 13px;
      color: var(--color-gray-900);
      border-radius: var(--radius-md) var(--radius-md) 0 0;
    }}

    .accordion-header:hover {{
      background: var(--color-primary-light);
    }}

    .accordion-header:active {{
      background: var(--color-gray-100);
    }}

    .accordion-header:focus-visible {{
      outline: 3px solid rgba(0, 114, 178, 0.22);
      outline-offset: 2px;
    }}

    .accordion-icon {{
      width: 16px;
      flex: 0 0 16px;
      text-align: center;
      color: var(--color-gray-600);
    }}

    .accordion-title {{
      flex: 1;
      font-size: var(--type-body);
    }}

    .accordion-chevron {{
      transition: transform 200ms ease;
      color: var(--color-gray-600);
      font-size: 12px;
    }}

    .accordion-body {{
      transition: max-height 220ms ease, padding 220ms ease;
      overflow: hidden;
    }}

    .accordion-body.collapsed {{
      max-height: 0 !important;
      padding: 0 !important;
      overflow: hidden;
    }}

    /* Padding lives on .accordion-content only — putting it here as well
       double-pads every expanded accordion (24px+ instead of 12px) */
    .accordion-body.expanded {{
      max-height: 2000px;
      padding: 0;
      overflow: visible;
    }}

    .accordion-content {{
      /* Content wrapper for additional spacing/styling */
    }}

    /* Nested input styling within accordions */
    .accordion-content .form-group {{
      margin-bottom: 6px;
    }}

    .accordion-content .form-group:last-child {{
      margin-bottom: 0;
    }}

    /* Ensure select dropdowns in accordions appear above other content */
    .accordion-content .selectize-dropdown,
    .accordion-content select {{
      z-index: 1000;
    }}

    .accordion-content .form-group {{
      position: relative;
      z-index: auto;
    }}

    /* Text overflow protection */
    .small-help, .box-body p, .text-muted.small {{
      overflow-wrap: break-word;
      word-wrap: break-word;
    }}

    /* Shiny selectInput dropdown positioning */
    .selectize-dropdown {{
      z-index: 1050 !important;
    }}

    /* ==================== Status Steps ==================== */

    .status-step {{
      text-align: center;
      min-height: 96px;
      padding: 12px 6px;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
    }}

    .status-step-icon {{
      font-size: 20px;
      margin-bottom: 6px;
      transition: color 180ms ease, transform 180ms ease;
    }}

    .status-step-label {{
      font-size: 11px;
      font-weight: {weight_semibold};
      color: var(--color-gray-900);
      margin-bottom: 2px;
      text-transform: uppercase;
      letter-spacing: 0.3px;
    }}

    .status-step-text {{
      font-size: 11px;
      color: var(--color-gray-600);
      line-height: 1.3;
    }}

    /* ==================== Enhanced Stat Cards ==================== */

    .stat-card {{
      transition: transform 150ms ease, box-shadow 150ms ease;
      min-height: 88px;
    }}

    .stat-card:hover {{
      transform: translateY(-1px);
      box-shadow: var(--shadow-level-2);
    }}

    /* ==================== Enhanced Buttons ==================== */

    .btn-primary {{
      box-shadow: 0 1px 3px rgba(0, 114, 178, 0.25);
    }}

    .btn-primary:hover {{
      box-shadow: 0 2px 6px rgba(0, 114, 178, 0.35);
      transform: translateY(-1px);
    }}

    .btn-primary:active {{
      transform: translateY(0);
      box-shadow: 0 1px 2px rgba(0, 114, 178, 0.2);
    }}

    .btn-default {{
      box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05);
    }}

    .btn-default:hover {{
      box-shadow: 0 2px 4px rgba(0, 0, 0, 0.08);
      transform: translateY(-1px);
    }}

    /*
     * Shiny's browser_download_button() always adds btn-default, even when callers add
     * btn-primary. Resolve that class collision explicitly so the default
     * button text color cannot make primary download labels blue-on-blue.
     */
    a.shiny-download-link.btn.btn-primary {{
      background-color: var(--color-primary-blue);
      border-color: var(--color-primary-blue);
      color: var(--color-white);
    }}

    a.shiny-download-link.btn.btn-primary:hover,
    a.shiny-download-link.btn.btn-primary:focus {{
      background-color: var(--color-primary-dark);
      border-color: var(--color-primary-dark);
      color: var(--color-white);
    }}

    a.shiny-download-link.btn.btn-primary:active {{
      background-color: var(--color-primary-dark);
      border-color: var(--color-primary-dark);
      color: var(--color-white);
    }}

    /* ==================== Enhanced Accordions ==================== */

    .accordion-section {{
      transition: box-shadow 150ms ease;
    }}

    .accordion-section:hover {{
      box-shadow: 0 1px 4px rgba(0, 0, 0, 0.06);
    }}

    .accordion-header {{
      border-radius: var(--radius-md);
    }}

    .accordion-body.expanded + .accordion-header,
    .accordion-section:has(.accordion-body.expanded) > .accordion-header {{
      border-radius: var(--radius-md) var(--radius-md) 0 0;
    }}

    /* ==================== Enhanced Empty States ==================== */

    .empty-state-container {{
      text-align: center;
      padding: 48px 24px;
    }}

    .empty-state-container .fa {{
      color: var(--color-primary-blue);
      opacity: 0.15;
      margin-bottom: 16px;
      font-size: 48px;
      display: block;
    }}

    .empty-state-container h4 {{
      font-weight: {weight_semibold};
      color: var(--color-gray-900);
      margin: 0 0 8px 0;
    }}

    .empty-state-container p {{
      color: var(--color-gray-600);
      font-size: 13px;
      margin: 0;
      max-width: 320px;
      margin-left: auto;
      margin-right: auto;
    }}

    /* Center validation messages inside plot canvases to avoid a broken-looking
       label in the upper-left corner when no data has been loaded yet. */
    .shiny-plot-output.shiny-output-error-validation {{
      display: flex !important;
      align-items: center;
      justify-content: center;
      text-align: center;
      padding: var(--spacing-xl);
      color: var(--color-gray-600);
      line-height: 1.5;
    }}

    .shiny-bound-output.recalculating {{
      opacity: 0.72;
      transition: opacity 120ms ease;
    }}

    @media (max-width: 575px) {{
      .box-body {{
        padding: 14px;
      }}

      .nav-tabs,
      .nav-pills {{
        gap: 3px;
      }}

      .nav-tabs > li > a,
      .nav-pills > li > a {{
        padding: 8px 10px;
      }}

      .status-step {{
        min-height: 86px;
      }}
    }}

    @media (prefers-reduced-motion: reduce) {{
      *,
      *::before,
      *::after {{
        scroll-behavior: auto !important;
        transition-duration: 0.01ms !important;
        animation-duration: 0.01ms !important;
        animation-iteration-count: 1 !important;
      }}

      .btn:hover,
      .stat-card:hover {{
        transform: none;
      }}
    }}

    /* ==================== Logo in Header ==================== */

    .main-header .logo img {{
      filter: brightness(0) invert(1);
    }}

    /* ==================== Overflow Guards ==================== */

    /* Long unbroken values (e.g. file names) must not escape their column */
    .selectize-control {{ max-width: 100%; }}

    .selectize-input {{
      max-width: 100%;
      overflow: hidden;
    }}

    /* inline-block (not block): block pushed selectize's internal ghost
       input onto a second line, making every dropdown ~56px tall while
       all other controls are 34px */
    .selectize-input > .item {{
      display: inline-block;
      max-width: calc(100% - 12px);
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
      vertical-align: middle;
    }}

    .selectize-dropdown .option {{
      overflow-wrap: anywhere;
    }}

    /* Stat cards: long values wrap inside the card instead of overflowing */
    .stat-card {{
      overflow: hidden;
      min-width: 0;
    }}

    .stat-card h3 {{
      overflow-wrap: anywhere;
    }}

    /* ==================== Segmented Toggle ==================== */
    /* Inline radios styled as a compact button group (e.g. Static/Interactive) */

    .segmented-toggle .form-group {{
      margin-bottom: 0;
    }}

    .segmented-toggle .radio-inline {{
      margin: 0;
      padding: 5px 14px;
      border: 1px solid var(--color-primary-blue);
      background: var(--color-white);
      color: var(--color-primary-blue);
      font-size: 12px;
      font-weight: {weight_medium};
      cursor: pointer;
      user-select: none;
    }}

    .segmented-toggle .radio-inline + .radio-inline {{
      margin-left: -1px;
    }}

    .segmented-toggle .radio-inline:first-of-type {{
      border-radius: var(--radius-sm) 0 0 var(--radius-sm);
    }}

    .segmented-toggle .radio-inline:last-of-type {{
      border-radius: 0 var(--radius-sm) var(--radius-sm) 0;
    }}

    .segmented-toggle .radio-inline input[type='radio'] {{
      position: absolute;
      opacity: 0;
    }}

    .segmented-toggle .radio-inline:has(input:checked) {{
      background: var(--color-primary-blue);
      color: var(--color-white);
    }}

    /* ==================== Compact Controls ==================== */
    /* Density pass: control panels were spending ~900px on six inputs.
       Tighter gaps, slimmer inputs, smaller labels, no floating min/max
       chips on sliders. */

    .form-group {{
      margin-bottom: 12px;
    }}

    label {{
      font-size: 12.5px;
      margin-bottom: 4px;
      font-weight: {weight_semibold};
    }}

    .form-control {{
      min-height: 34px;
      padding: 6px 10px;
      font-size: 13px;
    }}

    .selectize-input {{
      min-height: 34px;
      padding: 6px 10px;
      font-size: 13px;
    }}

    .checkbox {{
      margin-top: 4px;
      margin-bottom: 4px;
    }}

    .checkbox label {{
      font-size: 13px;
      font-weight: {weight_regular};
    }}

    /* Sliders: drop the floating min/max chips; grid labels remain */
    .irs--shiny .irs-min,
    .irs--shiny .irs-max {{
      display: none;
    }}

    .accordion-content {{
      padding: 12px 14px;
    }}

    /* ==================== Sidebar Version Badge ==================== */

    /* The selector must out-rank AdminLTE's '.sidebar-menu > li' rules —
       a bare class here silently loses and the badge hugs the gutter */
    .sidebar-menu > li.sidebar-version {{
      list-style: none;
      margin-top: 14px;
      padding: 12px 16px !important;
      border-top: 1px solid var(--color-gray-100);
      font-size: 11px;
      line-height: 1.7;
      color: var(--color-gray-600);
      letter-spacing: 0.4px;
    }}

    /* Slim, brand-colored upload meter: the default strip reserves a full
       20px text-height row between the file input and its helper text
       even while idle */
    .shiny-file-input-progress {{
      height: 8px;
      margin: 6px 0 0;
      border-radius: 4px;
      background: var(--color-gray-100);
      box-shadow: none;
    }}

    .shiny-file-input-progress .progress-bar {{
      font-size: 0;
      background-color: var(--color-primary-blue);
      box-shadow: none;
    }}
  ")
}

#' Get the JavaScript for accordion functionality and file input fix
#'
#' @return Character string with accordion JavaScript and file input fix
get_accordion_js <- function() {
  HTML("
  <script>
  function toggleAccordion(id, header) {
    const section = header
      ? header.closest('.accordion-section')
      : Array.from(document.querySelectorAll('.accordion-section'))
          .find(function(candidate) {
            return candidate.dataset.accordionId === id;
          });
    const body = section ? section.querySelector('.accordion-body') : null;
    const chevron = section ? section.querySelector('.accordion-chevron') : null;
    const activeHeader = header || (section ? section.querySelector('.accordion-header') : null);

    if (!body || !chevron) return;

    const willExpand = body.classList.contains('collapsed');
    body.classList.toggle('collapsed', !willExpand);
    body.classList.toggle('expanded', willExpand);
    chevron.style.transform = willExpand ? 'rotate(180deg)' : 'rotate(0deg)';
    body.setAttribute('aria-hidden', String(!willExpand));
    if (activeHeader) {
      activeHeader.setAttribute('aria-expanded', String(willExpand));
    }
    // Sliders (ionRangeSlider) initialized while hidden draw with zero
    // widths; a resize event after expanding makes them re-measure
    if (willExpand) {
      setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 60);
      setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 380);
    }
  }

  // Fix Shiny file input positioning
  // Shiny adds inline styles with !important that position file inputs off-screen
  // We need to override these inline styles with JavaScript
  function fixFileInput(input) {
    if (!(input instanceof HTMLInputElement) || input.type !== 'file') return;
    input.style.setProperty('position', 'absolute', 'important');
    input.style.setProperty('top', '0', 'important');
    input.style.setProperty('left', '0', 'important');
    input.style.setProperty('right', '0', 'important');
    input.style.setProperty('bottom', '0', 'important');
    input.style.setProperty('width', '100%', 'important');
    input.style.setProperty('height', '100%', 'important');
    input.style.setProperty('opacity', '0', 'important');
    input.style.setProperty('cursor', 'pointer', 'important');
    input.style.setProperty('z-index', '10', 'important');
  }

  function fixFileInputs(root) {
    const scope = root || document;
    if (scope.matches && scope.matches('input[type=\"file\"]')) {
      fixFileInput(scope);
    }
    if (scope.querySelectorAll) {
      scope.querySelectorAll('input[type=\"file\"]').forEach(fixFileInput);
    }
  }

  let fileInputObserver = null;
  let fileInputFixQueued = false;

  function scheduleFileInputFix() {
    if (fileInputFixQueued) return;
    fileInputFixQueued = true;
    window.requestAnimationFrame(function() {
      fileInputFixQueued = false;
      fixFileInputs(document);
    });
  }

  function setupFileInputObserver() {
    if (!window.MutationObserver || !document.body) return;
    if (fileInputObserver) return;

    fileInputObserver = new MutationObserver(function(mutations) {
      const hasNewFileInput = mutations.some(function(mutation) {
        return Array.from(mutation.addedNodes).some(function(node) {
          return node.nodeType === Node.ELEMENT_NODE &&
            ((node.matches && node.matches('input[type=\"file\"]')) ||
             (node.querySelector && node.querySelector('input[type=\"file\"]')));
        });
      });
      if (hasNewFileInput) {
        scheduleFileInputFix();
      }
    });

    fileInputObserver.observe(document.body, { childList: true, subtree: true });
  }

  // Run once on load and when Shiny reconnects/updates DOM.
  document.addEventListener('DOMContentLoaded', function() {
    fixFileInputs(document);
    setupFileInputObserver();
  });

  $(document).on('shiny:connected', function() {
    scheduleFileInputFix();
    setupFileInputObserver();
  });

  // On phones the sidebar slides in and pushes the content off-screen, and
  // AdminLTE never closes it after a tab is chosen — the app then looks
  // permanently broken. Auto-close the menu on tab selection on small screens.
  $(document).on('click', '.sidebar-menu a[href^=\"#shiny-tab-\"]', function() {
    if (window.innerWidth < 768) {
      document.body.classList.remove('sidebar-open');
      document.body.classList.add('sidebar-collapse');
    }
  });
  </script>
  ")
}

#' Client-side file saver.
#'
#' Receives base64 file content over the live Shiny connection and hands it
#' to the browser as a direct save. Used by browser_download() for every
#' download in the app: HTTP download links fail in the WebAssembly build,
#' while this path rides the same channel as all app traffic.
get_blob_download_js <- function() {
  HTML("
  <script>
  // This script can run before Shiny's own bundle, so poll until the
  // message API exists before registering
  (function registerSaveFile() {
    if (!(window.Shiny && Shiny.addCustomMessageHandler)) {
      setTimeout(registerSaveFile, 100);
      return;
    }
    Shiny.addCustomMessageHandler('simpleca_save_file', function(msg) {
      try {
        var b64 = String(msg.b64 || '').replace(/\\s/g, '');
        var bin = atob(b64);
        var bytes = new Uint8Array(bin.length);
        for (var i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
        var blob = new Blob([bytes], { type: 'application/octet-stream' });
        var a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = msg.filename || 'download';
        document.body.appendChild(a);
        a.click();
        setTimeout(function() { URL.revokeObjectURL(a.href); a.remove(); }, 4000);
      } catch (e) {
        console.error('simpleca_save_file failed:', e);
      }
    });
  })();
  </script>
  ")
}

# ==================== ggplot Theme Builder ====================

#' Build a consistent ggplot theme from standard inputs
#'
#' This is the ONLY function that should be used to create ggplot themes.
#' All modules MUST use this function to ensure visual consistency.
#' No module should create its own theme logic.
#'
#' @param theme_name One of "classic", "minimal", "light", "dark"
#' @param title_size Title font size (default: 18)
#' @param bold_title Logical, bold title (default: TRUE)
#' @param axis_title_size Axis title font size (default: 14)
#' @param bold_axis_title Logical, bold axis titles (default: TRUE)
#' @param axis_size Axis text size (default: 12)
#' @param bold_axis_text Logical, bold axis text (default: FALSE)
#' @param font Font family name (default: "Arial")
#' @param legend_pos Legend position - "none", "bottom", "right", "top", "left" (default: "none")
#' @param grid_major Show major gridlines (default: FALSE)
#' @param grid_minor Show minor gridlines (default: FALSE)
#'
#' @return A ggplot2 theme object
#' @export
build_plot_theme <- function(
  theme_name = "classic",
  title_size = 18,
  bold_title = TRUE,
  axis_title_size = 14,
  bold_axis_title = TRUE,
  axis_size = 12,
  bold_axis_text = FALSE,
  font = "Arial",
  legend_pos = "none",
  grid_major = FALSE,
  grid_minor = FALSE
) {
  # Base theme selection
  base_theme <- switch(theme_name %||% "classic",
    classic = ggplot2::theme_classic(),
    minimal = ggplot2::theme_minimal(),
    light = ggplot2::theme_light(),
    dark = ggplot2::theme_dark(),
    ggplot2::theme_classic()
  )

  # Build customized theme
  theme_result <- base_theme + ggplot2::theme(
    # Title
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      size = title_size %||% 18,
      face = if (isTRUE(bold_title)) "bold" else "plain",
      family = font %||% "Arial"
    ),
    # Subtitle
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5,
      size = max(8, (title_size %||% 18) - 4),
      family = font %||% "Arial"
    ),
    # Axis titles
    axis.title = ggplot2::element_text(
      size = axis_title_size %||% 14,
      face = if (isTRUE(bold_axis_title)) "bold" else "plain",
      family = font %||% "Arial"
    ),
    # Axis text
    axis.text = ggplot2::element_text(
      size = axis_size %||% 12,
      face = if (isTRUE(bold_axis_text)) "bold" else "plain",
      family = font %||% "Arial"
    ),
    # Legend
    legend.position = legend_pos %||% "none",
    legend.title = ggplot2::element_text(
      face = "bold",
      size = (axis_title_size %||% 14) * 0.85
    ),
    legend.text = ggplot2::element_text(
      size = (axis_size %||% 12) * 0.9
    ),
    # Margins
    plot.margin = ggplot2::margin(10, 25, 10, 10)
  )

  # Gridlines
  if (isTRUE(grid_major) || isTRUE(grid_minor)) {
    theme_result <- theme_result + ggplot2::theme(
      panel.grid.major = if (isTRUE(grid_major)) {
        ggplot2::element_line(color = "grey90", linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = if (isTRUE(grid_minor)) {
        ggplot2::element_line(color = "grey95", linewidth = 0.2)
      } else {
        ggplot2::element_blank()
      }
    )
  } else {
    theme_result <- theme_result + ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )
  }

  return(theme_result)
}

#' Get theme parameters from Shiny inputs
#'
#' Helper function to extract theme parameters from Shiny input object.
#' Ensures consistent parameter extraction across all modules.
#'
#' @param input Shiny input object
#' @param prefix Input ID prefix (e.g., "tc", "pa", "metric")
#'
#' @return A list of theme parameters suitable for build_plot_theme()
get_theme_params_from_input <- function(input, prefix = "tc") {
  # Support consolidated typography: base_font_size + bold_labels
  # Derives individual sizes: title = base+4, axis_title = base, axis_text = max(8, base-2)
  base_size <- input[[paste0(prefix, "_base_font_size")]]
  bold_labels <- input[[paste0(prefix, "_bold_labels")]]

  if (!is.null(base_size)) {
    # New consolidated pattern
    bold <- isTRUE(bold_labels %||% TRUE)
    list(
      theme_name = input[[paste0(prefix, "_theme")]] %||% "classic",
      title_size = base_size + 4,
      bold_title = bold,
      axis_title_size = base_size,
      bold_axis_title = bold,
      axis_size = max(8, base_size - 2),
      bold_axis_text = FALSE,
      font = input[[paste0(prefix, "_font")]] %||% "Arial",
      legend_pos = input[[paste0(prefix, "_legend_pos")]] %||% "none",
      grid_major = FALSE,
      grid_minor = FALSE
    )
  } else {
    # Legacy individual controls pattern
    list(
      theme_name = input[[paste0(prefix, "_theme")]] %||% "classic",
      title_size = input[[paste0(prefix, "_title_size")]] %||% 18,
      bold_title = input[[paste0(prefix, "_bold_title")]] %||% TRUE,
      axis_title_size = input[[paste0(prefix, "_axis_title_size")]] %||% 14,
      bold_axis_title = input[[paste0(prefix, "_bold_axis_title")]] %||% TRUE,
      axis_size = input[[paste0(prefix, "_axis_size")]] %||% 12,
      bold_axis_text = input[[paste0(prefix, "_bold_axis_text")]] %||% FALSE,
      font = input[[paste0(prefix, "_font")]] %||% "Arial",
      legend_pos = input[[paste0(prefix, "_legend_pos")]] %||% "none",
      grid_major = input[[paste0(prefix, "_grid_major")]] %||% FALSE,
      grid_minor = input[[paste0(prefix, "_grid_minor")]] %||% FALSE
    )
  }
}

#' Build plot theme from Shiny inputs
#'
#' Convenience function that combines get_theme_params_from_input and build_plot_theme.
#' This is the recommended way to get a theme in module server code.
#'
#' @param input Shiny input object
#' @param prefix Input ID prefix (e.g., "tc", "pa", "metric")
#'
#' @return A ggplot2 theme object
build_plot_theme_from_input <- function(input, prefix = "tc") {
  params <- get_theme_params_from_input(input, prefix)
  do.call(build_plot_theme, params)
}
