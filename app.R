# =============================== Load Libraries ===============================
# Increase max upload size to 100MB (default is 5MB)
options(shiny.maxRequestSize = 100 * 1024^2)
# Use ragg for raster rendering when installed. The package name is assembled
# at runtime so Shinylive's dependency scanner doesn't require it in
# WebAssembly builds (webR uses its own canvas graphics device).
if (requireNamespace(paste0("ra", "gg"), quietly = TRUE)) {
  options(shiny.useragg = TRUE)
}

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(purrr)
library(RColorBrewer)
library(scales)
library(colourpicker)
library(knitr)
library(kableExtra)
library(plotly)
library(bslib)
library(stringr)
library(tools)
library(ggbeeswarm)

# ============================ Source Modules and Helpers =============================
# Check if R folder exists
if (!dir.exists("R")) {
  stop("R folder not found. Please make sure you're in the correct directory and have all module files.")
}

# Source all R files with enhanced error handling
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
if (length(r_files) == 0) {
  stop("No R files found in R/ directory. Please ensure all module files are present.")
}

# Source each file with detailed error reporting
for (file in r_files) {
  tryCatch(
    {
      source(file, local = FALSE)
      cat("✓ Loaded:", basename(file), "\n")
    },
    error = function(e) {
      cat("✗ Error loading", basename(file), ":", e$message, "\n")
      cat("File path:", file, "\n")
      stop(paste("Failed to source", basename(file), ":", e$message))
    }
  )
}

# ============================== UI =================================
ui <- dashboardPage(
  skin = "blue",
  header = dashboardHeader(
    title = "SimpleCa\u00b2\u207a"
  ),
  sidebar = dashboardSidebar(
    width = 230,
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Load Data", tabName = "load", icon = icon("database")),
      menuItem("Time Course", tabName = "time", icon = icon("chart-line")),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
      menuItem("Metrics", tabName = "metrics", icon = icon("chart-bar")),
      menuItem("Data & Export", tabName = "data_export", icon = icon("download")),
      tags$li(class = "header", "REFERENCE"),
      menuItem("Metric Explanations", tabName = "metrics_explained", icon = icon("lightbulb")),
      menuItem("Help", tabName = "help", icon = icon("circle-question")),
      tags$li(
        class = "sidebar-version",
        div(paste0("v", SIMPLECA_VERSION)),
        div("Wang Lab · UNMC")
      )
    )
  ),
  body = dashboardBody(
    # Initialize JavaScript (formulas render as plain HTML via formula_line();
    # MathJax typeset unreliably under Shinylive and pulled a CDN script)
    useShinyjs(),

    # Apply unified theme system
    tags$head(
      # Inject unified theme CSS
      tags$style(HTML(get_unified_theme_css())),

      # Inject accordion JavaScript
      get_accordion_js(),

      # Client-side file saver: downloads travel over the Shiny connection
      get_blob_download_js(),

      # Keep existing custom CSS for specific overrides
      tags$style(HTML("
        /* Hide all top-right Shiny interface elements */
        .navbar-nav.navbar-right { display: none !important; }
        .navbar-custom-menu { display: none !important; }
        .navbar-right { display: none !important; }
        .nav.navbar-nav.navbar-right { display: none !important; }

        /* Style the disconnect overlay instead of hiding it */
        #shiny-disconnected-overlay,
        .shiny-disconnected-overlay {
          background: rgba(255, 255, 255, 0.92) !important;
          color: #333 !important;
          font-family: 'Source Sans Pro', 'Helvetica Neue', sans-serif !important;
          font-size: 16px !important;
          text-align: center !important;
          padding-top: 40vh !important;
        }
        #shiny-disconnected-overlay::after,
        .shiny-disconnected-overlay::after {
          content: 'Connection lost. Please refresh the page to reconnect.' !important;
        }

        /* Remove any question mark or help toggles from shinydashboard chrome */
        .fa-question-circle { display: none !important; }

        /* Collapsible sections */
        .collapsible-section {
          margin-bottom: 10px;
        }

        .collapsible-header {
          cursor: pointer;
          font-weight: 600;
          color: var(--color-primary-blue);
          padding: 8px 0;
          border-bottom: 1px solid var(--color-gray-100);
          user-select: none;
          transition: color 0.3s ease;
        }

        .collapsible-header:hover {
          color: var(--color-primary-dark);
        }

        .collapsible-content {
          margin-top: 10px;
          padding-left: 16px;
          border-left: 2px solid var(--color-gray-100);
        }

        /* Advanced options styling */
        details {
          border: 1px solid var(--color-gray-100);
          border-radius: var(--radius-md);
          padding: 8px;
          margin-top: 10px;
          background-color: var(--color-gray-50);
        }

        summary {
          cursor: pointer;
          font-weight: 600;
          color: var(--color-primary-blue);
          outline: none;
          padding: 5px 0;
        }

        summary:hover {
          color: var(--color-primary-dark);
        }

        /* Custom spacing utilities */
        .mt-10 { margin-top: 10px; }
        .mt-15 { margin-top: 15px; }
        .mt-20 { margin-top: 20px; }
        .mb-10 { margin-bottom: 10px; }
        .mb-15 { margin-bottom: 15px; }
        .mb-20 { margin-bottom: 20px; }

        .small-help {
          color: var(--color-gray-600);
          font-size: 11px;
          margin-top: 2px;
          margin-bottom: 6px;
          font-style: normal;
          line-height: 1.4;
        }

        .text-center {
          text-align: center;
        }
      "))
    ),

    # Main content tabs
    tabItems(
      mod_load_data_ui("load_data"),
      mod_time_course_ui("time_course"),
      mod_heatmap_ui("heatmap"),
      mod_metrics_ui("metrics"),
      mod_metrics_explained_ui("metrics_explained"),
      mod_data_export_ui("data_export"),
      mod_help_ui("help")
    )
  )
)

# ============================= Server =============================
server <- function(input, output, session) {
  # ================== Reactive Values & Modules ===================

  # Main reactive values store
  rv <- reactiveValues(
    files = NULL,
    groups = NULL,
    dts = list(),
    long = NULL,
    summary = NULL,
    metrics = NULL,
    colors = NULL,
    raw_traces = list(),
    baselines = list(),
    baseline_method = NULL,
    baseline_frames = NULL
  )

  # ================== Call All Modules (ONCE EACH) ===================

  tryCatch(
    {
      # Core data modules
      mod_load_data_server("load_data", rv)

      # Analysis modules
      time_course_outputs <- mod_time_course_server("time_course", rv)
      heatmap_outputs <- mod_heatmap_server("heatmap", rv)
      metrics_outputs <- mod_metrics_server("metrics", rv)
      mod_metrics_explained_server("metrics_explained", rv)

      # Combined Data & Export module
      metrics_plot_reactive <- metrics_outputs$plot %||% reactive({
        NULL
      })
      heatmap_plot_reactive <- heatmap_outputs$plot %||% reactive({
        NULL
      })
      time_course_plot_reactive <- time_course_outputs$plot %||% reactive({
        NULL
      })

      mod_data_export_server("data_export", rv,
        metrics_plot_reactive = metrics_plot_reactive,
        heatmap_plot_reactive = heatmap_plot_reactive,
        time_course_plot_reactive = time_course_plot_reactive
      )

      # Help module
      mod_help_server("help")

      cat("✓ All modules loaded successfully\n")
    },
    error = function(e) {
      cat("✗ Error initializing modules:", e$message, "\n")
      showNotification(
        paste("Error initializing modules:", e$message),
        type = "error",
        duration = 10
      )
    }
  )

  # ================== Global Error Handling ===================

  # In production, show a friendly notification but log the real error to the
  # server log so failures stay diagnosable. Set SHINY_ENV=development to let
  # errors propagate normally for debugging.
  if (!identical(Sys.getenv("SHINY_ENV"), "development")) {
    options(shiny.error = function() {
      message("[SimpleCa] Unhandled error: ", geterrmessage())
      showNotification(
        "An error occurred. Please check your data and try again.",
        type = "error",
        duration = 5
      )
    })
  }
}

# ============================= Run App =============================
# Print startup info (verbose logging for development/debugging)
if (getOption("simpleca.verbose", default = interactive())) {
  cat("\n=== SimpleCa²⁺ Starting ===\n")
  cat("renv project detected\n")
  cat("Current working directory:", getwd(), "\n")
  cat("R version:", R.version.string, "\n")
}

# Run the app
shinyApp(ui, server)
