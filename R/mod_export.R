# R/mod_export.R

mod_export_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "export",
          fluidRow(
            column(width = 12,
                   theme_box(
                     title = "Global Export Center",
                     icon = icon("download"),
                     status = "primary",
                     width = 12,
                     collapsible = FALSE,
                     
                     uiOutput(ns("export_content"))
                   )
            )
          )
  )
}

mod_export_server <- function(id, rv, metrics_plot_reactive, heatmap_plot_reactive, time_course_plot_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$export_content <- renderUI({
      if (is.null(rv$dts) || length(rv$dts) == 0) {
        div(style = "text-align: center; padding: 40px; color: var(--color-gray-600);",
            icon("cloud-upload-alt", class = "fa-3x", style = "margin-bottom: 15px; color: var(--color-gray-400);"),
            h4("No Data Available", style = "margin-top: 0;"),
            p("Please load and process data in the 'Load Data' tab to enable export options.")
        )
      } else {
        fluidRow(
          # Left Column: Settings
          column(4,
                 h5("Export Settings", style = "font-weight: 600; border-bottom: 1px solid var(--color-gray-100); padding-bottom: 10px;"),
                 
                 radioButtons(ns("exp_fmt"), "Format", 
                              choices = c("PNG"="png", "PDF"="pdf", "TIFF"="tiff", "SVG"="svg"),
                              inline = TRUE, selected="png"),
                 
                 fluidRow(
                   column(6, numericInput(ns("exp_w"), "Width (in)", 12, min=4, max=30, step=0.5)),
                   column(6, numericInput(ns("exp_h"), "Height (in)", 8, min=4, max=30, step=0.5))
                 ),
                 
                 numericInput(ns("exp_dpi"), "DPI (for raster)", 300, min=72, max=600, step=10),
                 
                 conditionalPanel(paste0("input['", ns("exp_fmt"), "'] == 'tiff'"),
                                  selectInput(ns("tiff_comp"), "TIFF compression", 
                                              choices = c("LZW"="lzw", "Zip"="zip", "None"="none"), selected="lzw")
                 )
          ),
          
          # Middle Column: Data Downloads
          column(4, style = "border-left: 1px solid var(--color-gray-100); padding-left: 20px;",
                 h5("Data Files", style = "font-weight: 600; border-bottom: 1px solid var(--color-gray-100); padding-bottom: 10px;"),
                 
                 p("Download tabular data for analysis in other software.", class = "text-muted small"),
                 
                 div(style = "display: flex; flex-direction: column; gap: 10px;",
                     downloadButton(ns("dl_metrics_csv"), "Download All Metrics (CSV)", class = "btn-primary btn-block"),
                     downloadButton(ns("dl_summary_csv"), "Download Summary Stats (CSV)", class = "btn-default btn-block"),
                     
                     tags$hr(style = "margin: 15px 0;"),
                     
                     div(style = "background: var(--color-gray-50); padding: 10px; border-radius: 4px;",
                         h6("Processed Raw Data", style = "margin-top: 0; font-weight: 600;"),
                         selectInput(ns("exp_dl_group"), NULL, choices = names(rv$dts), width = "100%"),
                         downloadButton(ns("dl_processed_wide_exp"), "Download Dataset (CSV)", class = "btn-default btn-block")
                     )
                 )
          ),
          
          # Right Column: Figure Downloads
          column(4, style = "border-left: 1px solid var(--color-gray-100); padding-left: 20px;",
                 h5("Figure Downloads", style = "font-weight: 600; border-bottom: 1px solid var(--color-gray-100); padding-bottom: 10px;"),
                 
                 p("Download high-resolution plots using the settings on the left.", class = "text-muted small"),
                 
                 div(style = "display: flex; flex-direction: column; gap: 10px;",
                     downloadButton(ns("dl_timecourse_plot"), "Time Course Plot", class = "btn-primary btn-block"),
                     downloadButton(ns("dl_heatmap_plot"), "Heatmap Plot", class = "btn-primary btn-block"),
                     downloadButton(ns("dl_metrics_plot"), "Current Metrics Plot", class = "btn-primary btn-block")
                 ),
                 
                 div(class = "alert alert-info small", style = "margin-top: 20px;",
                     icon("info-circle"), " Note: Plot appearance is determined by the settings in each respective tab."
                 )
          )
        )
      }
    })
    
    output$dl_processed_wide_exp <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = c("processed", input$exp_dl_group %||% "data"),
          ext = "csv"
        )
      },
      content = function(file) { req(rv$dts, input$exp_dl_group); data.table::fwrite(rv$dts[[input$exp_dl_group]], file) }
    )

    output$dl_metrics_csv <- downloadHandler(
      filename = function() {
        n_cells <- if (!is.null(rv$metrics)) nrow(rv$metrics) else 0
        build_export_filename(
          rv,
          parts = c("all_metrics", paste0(n_cells, "_cells")),
          ext = "csv"
        )
      },
      content = function(file) data.table::fwrite(rv$metrics, file)
    )

    output$dl_summary_csv <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = "timecourse_summary",
          ext = "csv"
        )
      },
      content = function(file) data.table::fwrite(rv$summary, file)
    )
    
    output$dl_timecourse_plot <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = "timecourse_plot",
          ext = input$exp_fmt %||% "png"
        )
      },
      content = function(file) {
        req(time_course_plot_reactive())
        ggplot2::ggsave(file, plot = time_course_plot_reactive(), width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
      }
    )
    
    output$dl_heatmap_plot <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = "heatmap",
          ext = input$exp_fmt %||% "png"
        )
      },
      content = function(file) {
        req(heatmap_plot_reactive())
        ggplot2::ggsave(file, plot = heatmap_plot_reactive(), width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
      }
    )
    
    output$dl_metrics_plot <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = "metrics_plot",
          ext = input$exp_fmt %||% "png"
        )
      },
      content = function(file) {
        req(metrics_plot_reactive())
        ggplot2::ggsave(file, plot = metrics_plot_reactive(), width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
      }
    )
    
  })
}
