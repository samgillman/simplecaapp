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
