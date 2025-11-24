# R/components.R
# Reusable UI Components for SimpleCa²⁺

#' Create an accordion section (collapsible panel)
#'
#' @param id The namespace ID for this accordion
#' @param title Title text for the accordion header
#' @param content UI elements to show when expanded
#' @param expanded Logical, whether the accordion starts expanded (default: FALSE)
#' @param icon Optional Font Awesome icon name (e.g., "cog", "chart-bar")
#'
#' @return A div containing the accordion HTML structure
accordion <- function(id, title, content, expanded = FALSE, icon = NULL) {
  # Icon HTML if provided
  icon_html <- if (!is.null(icon)) {
    tags$i(class = paste0("fa fa-", icon), style = "margin-right: 8px;")
  } else {
    NULL
  }

  # Chevron icon that rotates
  chevron <- tags$i(
    class = "fa fa-chevron-down accordion-chevron",
    style = if (expanded) "transform: rotate(180deg);" else ""
  )

  # Accordion structure
  div(
    class = "accordion-section",
    `data-accordion-id` = id,

    # Header (clickable)
    div(
      class = "accordion-header",
      onclick = sprintf("toggleAccordion('%s')", id),
      icon_html,
      span(title, class = "accordion-title"),
      chevron
    ),

    # Body (collapsible)
    div(
      class = paste("accordion-body", if (expanded) "expanded" else "collapsed"),
      div(
        class = "accordion-content",
        content
      )
    )
  )
}

#' Standardized Box Component
#'
#' A wrapper around shinydashboard::box with consistent styling presets.
#'
#' @param title Box title
#' @param ... Content
#' @param status Bootstrap status (primary, success, etc.)
#' @param solidHeader Logical, whether header has background color
#' @param width Width (1-12)
#' @param collapsible Logical
#' @param collapsed Logical
#'
#' @return A box UI element
theme_box <- function(title, ..., status = "primary", solidHeader = TRUE, width = 12, collapsible = FALSE, collapsed = FALSE) {
  shinydashboard::box(
    title = title,
    ...,
    status = status,
    solidHeader = solidHeader,
    width = width,
    collapsible = collapsible,
    collapsed = collapsed
    # Note: Additional custom CSS styling is handled by R/theme.R targeting .box class
  )
}

#' Statistic Card Component
#'
#' A styled card for displaying a single key metric/statistic.
#'
#' @param value The main value to display (reactive output or string)
#' @param label The label describing the value
#' @param width Width of the card (bootstrap columns)
#' @param style Additional CSS styles
#'
#' @return A div containing the stat card
stat_card <- function(value, label, width = 12, style = "") {
  div(
    class = "stat-card",
    style = paste(
      "background: var(--color-gray-50);",
      "border: 1px solid var(--color-gray-100);",
      "text-align: center;",
      "padding: 24px 16px;",
      "border-radius: var(--radius-md);",
      "box-shadow: var(--shadow-level-1);",
      "margin-bottom: 12px;",
      style
    ),
    h3(value, style = "margin: 0 0 6px 0; font-size: 40px; font-weight: 700; line-height: 1; color: var(--color-primary-blue);"),
    p(label, style = "margin: 0; font-size: 13px; color: var(--color-gray-600); font-weight: 500; letter-spacing: 0.3px; text-transform: uppercase;")
  )
}

#' Processing Status Step
#'
#' A visual indicator for a step in the processing pipeline.
#'
#' @param icon_name FontAwesome icon name
#' @param title Step title
#' @param status_text Status text output
#' @param color Color variable (e.g., "var(--color-success)")
#'
#' @return A UI element for the status step
status_step <- function(icon_name, title, status_text, color = "var(--color-primary-blue)") {
  column(3, align = "center",
    div(style = "padding: 8px;",
      icon(icon_name, class = "fa-2x", style = paste0("color: ", color, "; margin-bottom: 10px; display: block; transition: all 0.3s ease;")),
      h5(title, style = "margin: 0 0 6px 0; font-weight: 600; font-size: 14px; color: var(--color-gray-900);"),
      div(status_text, style = "font-size: 12px; color: var(--color-gray-600); line-height: 1.4;")
    )
  )
}

#' Primary Action Button
#'
#' A consistently styled primary button.
#'
#' @param inputId Input ID
#' @param label Button label
#' @param icon Optional icon
#' @param width Width (css unit)
#' @param ... Additional arguments to actionButton
#'
#' @return An actionButton
primary_button <- function(inputId, label, icon = NULL, width = NULL, ...) {
  actionButton(
    inputId = inputId,
    label = label,
    icon = icon,
    width = width,
    class = "btn-primary",
    style = if (!is.null(width)) paste0("width: ", width, ";") else "",
    ...
  )
}
