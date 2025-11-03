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
      background: var(--color-primary-blue);
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
      background: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
    }}

    .box.box-primary > .box-header {{
      background: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
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
      background: var(--color-white) !important;
      border-right: 1px solid var(--color-gray-100);
    }}

    .sidebar-menu > li > a {{
      color: var(--color-gray-900) !important;
      font-weight: {weight_regular};
      padding: var(--spacing-sm) var(--spacing-md);
    }}

    .sidebar-menu > li.active > a {{
      background-color: var(--color-primary-light) !important;
      color: var(--color-primary-dark) !important;
      font-weight: {weight_medium};
      border-left: 3px solid var(--color-primary-blue) !important;
    }}

    .sidebar-menu > li > a:hover {{
      background-color: var(--color-gray-50) !important;
      color: var(--color-gray-900) !important;
    }}

    /* ==================== Dashboard Header ==================== */

    .main-header .logo {{
      background-color: var(--color-primary-blue);
      color: var(--color-white);
      font-weight: {weight_semibold};
      text-align: center;
      padding: 15px;
    }}

    .main-header .navbar {{
      background-color: var(--color-primary-blue);
    }}

    /* ==================== Content & Layout Alignment ==================== */

    .content-wrapper {{
      background-color: var(--color-gray-50);
    }}

    .content {{
      padding: 20px;
    }}

    /* Ensure fluidRow has no weird margins */
    .row {{
      margin-left: 0;
      margin-right: 0;
    }}

    /* Make sure columns don't have excessive padding */
    .col-sm-6 {{
      padding-left: 7.5px;
      padding-right: 7.5px;
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

    /* ==================== Accordion Component ==================== */

    .accordion-section {{
      margin-bottom: var(--spacing-md);
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-md);
      background: var(--color-white);
      overflow: hidden;
    }}

    .accordion-header {{
      padding: var(--spacing-sm) var(--spacing-md);
      background: var(--color-gray-50);
      cursor: pointer;
      user-select: none;
      display: flex;
      align-items: center;
      gap: var(--spacing-sm);
      transition: background-color 150ms ease;
      font-weight: {weight_medium};
      color: var(--color-gray-900);
    }}

    .accordion-header:hover {{
      background: var(--color-primary-light);
    }}

    .accordion-header:active {{
      background: var(--color-gray-100);
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
      transition: max-height 300ms ease, padding 300ms ease;
      overflow: hidden;
    }}

    .accordion-body.collapsed {{
      max-height: 0 !important;
      padding: 0 !important;
    }}

    .accordion-body.expanded {{
      max-height: 2000px;
      padding: var(--spacing-md);
    }}

    .accordion-content {{
      /* Content wrapper for additional spacing/styling */
    }}

    /* Nested input styling within accordions */
    .accordion-content .form-group {{
      margin-bottom: var(--spacing-sm);
    }}

    .accordion-content .form-group:last-child {{
      margin-bottom: 0;
    }}
  ")
}

#' Get the JavaScript for accordion functionality
#'
#' @return Character string with accordion JavaScript
get_accordion_js <- function() {
  HTML("
  <script>
  function toggleAccordion(id) {
    // Find the accordion elements
    const body = document.querySelector(`[data-accordion-id='${id}'] .accordion-body`);
    const chevron = document.querySelector(`[data-accordion-id='${id}'] .accordion-chevron`);

    if (!body || !chevron) return;

    // Toggle collapsed/expanded classes
    const isCollapsed = body.classList.contains('collapsed');

    if (isCollapsed) {
      // Expand
      body.classList.remove('collapsed');
      body.classList.add('expanded');
      chevron.style.transform = 'rotate(180deg)';
    } else {
      // Collapse
      body.classList.remove('expanded');
      body.classList.add('collapsed');
      chevron.style.transform = 'rotate(0deg)';
    }
  }
  </script>
  ")
}
