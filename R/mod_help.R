# R/mod_help.R

mod_help_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "help",
          fluidRow(
            column(
              width = 8, offset = 2,
              
              # Header Box
              div(style = "text-align: center; margin-bottom: 30px;",
                  h1("SimpleCa²⁺", style = "font-weight: 700; color: var(--color-primary-blue); margin-bottom: 10px;"),
                  p("A modern workflow for calcium imaging data analysis", style = "font-size: 18px; color: var(--color-gray-600);")
              ),
              
              theme_box(
                title = "Quick Start Guide",
                icon = icon("rocket"),
                status = "primary",
                width = 12,
                collapsible = FALSE,
                
                div(class = "help-section",
                    tags$ol(style = "padding-left: 20px; font-size: 16px; line-height: 1.6;",
                            tags$li(tags$b("Load Data:"), " Upload wide-format CSV/Excel files. Time may appear anywhere; use Advanced Options to confirm its source or exclude columns."),
                            tags$li(tags$b("Process:"), " Select a stable baseline frame window with the slider or typed frame bounds, and click 'Process Data'."),
                            tags$li(tags$b("Visualize:"), " Check the Time Course and Heatmap tabs to inspect signal quality."),
                            tags$li(tags$b("Analyze:"), " Use the Metrics tab to quantify peaks, AUC, rise times, etc."),
                            tags$li(tags$b("Explain:"), " Use Metric Explanations (under Reference) to visualize how metrics are calculated on your data."),
                            tags$li(tags$b("Export:"), " Download high-res figures and data tables from the Data & Export tab.")
                    )
                )
            ),
              
              theme_box(
                title = "Input Data Format",
                icon = icon("file-csv"),
                status = "info",
                width = 12,
                collapsible = TRUE,
                
                div(style = "padding: 10px;",
                    p("Your input file should be formatted as a standard wide table:", style = "margin-bottom: 15px;"),
                    
                    tags$table(class = "table table-bordered table-sm", style = "width: 100%; font-family: var(--font-mono); font-size: 13px;",
                               tags$thead(
                                 tags$tr(style = "background: var(--color-gray-50);",
                                         tags$th("Time"), tags$th("Cell1"), tags$th("Cell2"), tags$th("...")
                                 )
                               ),
                               tags$tbody(
                                 tags$tr(tags$td("0.0"), tags$td("120.5"), tags$td("98.2"), tags$td("...")),
                                 tags$tr(tags$td("0.1"), tags$td("121.0"), tags$td("99.1"), tags$td("...")),
                                 tags$tr(tags$td("0.2"), tags$td("125.3"), tags$td("105.4"), tags$td("..."))
                               )
                    ),
                    
                    div(class = "alert alert-info small", style = "margin-top: 15px;",
                        icon("info-circle"), " A Time column in seconds is recommended. A Frame column or unnamed sequential ImageJ index is converted using the sampling rate; otherwise Time is generated. Advanced Options confirms the detected source and lets you exclude unwanted numeric columns before processing."
                    )
                )
              ),
              
              theme_box(
                title = "Metric Definitions",
                icon = icon("book"),
                # Primary like every other box header — a lone green header
                # reads as a status signal rather than a section
                status = "primary",
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                
                div(class = "help-metrics",
                    tags$dl(style = "line-height: 1.6;",
                            tags$dt("Peak ΔF/F₀"), tags$dd("Maximum normalized fluorescence value after baseline correction."),
                            br(),
                            tags$dt("AUC (Area Under Curve)"), tags$dd("Signed net integral of ΔF/F₀ over the recording, calculated by the trapezoidal method. Values below zero contribute negatively."),
                            br(),
                            tags$dt("Time to Peak"), tags$dd("Time-axis coordinate of the post-baseline maximum. This is not stimulus latency unless Time = 0 is aligned to stimulus onset."),
                            br(),
                            tags$dt("Rise Time (10-90%)"), tags$dd("Time taken for signal to rise from 10% to 90% of peak amplitude."),
                            br(),
                            tags$dt("FWHM"), tags$dd("Full Width at Half Maximum—the duration above 50% of response amplitude. If the recording ends before the falling crossing, the exact value is missing and an observed lower bound is exported."),
                            br(),
                            tags$dt("Derived Half-Width"), tags$dd("One-half of an exact FWHM; not a separately measured peak-to-crossing interval."),
                            br(),
                            tags$dt("10–90% ΔF/F₀ Rise Rate"), tags$dd("Average normalized-fluorescence slope between the 10% and 90% rising crossings. It is not a direct calcium-influx measurement."),
                            br(),
                            tags$dt("SNR"), tags$dd("Signal-to-Noise Ratio: Peak Amplitude divided by Baseline Standard Deviation.")
                    )
                )
              ),

              theme_box(
                title = "About",
                icon = icon("university"),
                status = "info",
                width = 12,
                collapsible = FALSE,
                div(style = "text-align: center; padding: 6px 0;",
                    p(style = "margin: 0 0 6px 0; font-weight: 600;",
                      paste0("SimpleCa²⁺ v", SIMPLECA_VERSION)),
                    p(style = "margin: 0 0 6px 0;",
                      "Wang Lab · University of Nebraska Medical Center"),
                    p(style = "margin: 0; font-size: 12px; color: var(--color-gray-600);",
                      "If you use SimpleCa²⁺ in your research, please cite the Wang Lab and this application. A formal citation will accompany the manuscript.")
                )
              )
            )
          )
  )
}

mod_help_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Static content
  })
}
