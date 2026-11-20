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
              
              div(class = "help-start", open_load_button("Open Quick Start in Load Data")),
              theme_box(
                title = "Input Data Format",
                icon = icon("file-csv"),
                status = "info",
                width = 12,
                collapsible = TRUE,
                
                div(style = "padding: 10px;",
                    p("Expand Recording details beneath an uploaded filename to set the display name, plot color and order. Optional condition and experimental unit IDs are saved in the processing manifest. You can edit these details after processing."),
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
                    ),
                    div(class = "alert alert-info small", style = "margin-top: 10px;",
                        icon("info-circle"), " If your export includes a cell-free background ROI, tick “My file includes a background ROI column” on Load Data. Columns named BG or Background are selected automatically; otherwise choose the column yourself. The background is subtracted from every cell at each frame before F₀, is not counted as a cell, and is recorded in the processing manifest. Leave the box unticked for traces that were already background-corrected."
                    ),
                    div(class = "alert alert-warning small", style = "margin-top: 10px;",
                        icon("exclamation-triangle"), " Select the input type and units explicitly: raw or corrected fluorescence, ratio, or an already normalized ΔF/F₀ or ΔR/R₀ fraction/percent. Percent values are converted once to fractions. Raw-baseline brightness QC is unavailable for already normalized inputs. Loading-baseline settings and local response-baseline settings serve different purposes."
                    )
                )
              ),

              theme_box(
                title = "Percent Responding", icon = icon("percent"), width = 12,
                collapsible = TRUE, collapsed = TRUE,
                p("Name the protocol and confirm stimulus onset, baseline start/end, and scoring start/end against your acquisition log. The baseline may end before a delayed scoring interval. Use explicit per-recording overrides when delivery times differ. The overview previews the draft timing."),
                p("The existing sample-mode preset retains a 10% local fractional rise, 3 baseline SDs, and two consecutive acquired samples. The optional one-second slow-evoked preset is provisional and may reject brief real transients. Validate the rule using controls before comparing conditions; no preset is a universal 50B11 optimum."),
                p("The larger of the floor and the SD-based threshold sets the required rise. Both are relative to each cell’s local prestimulus fluorescence or ratio. Duration is the sampled interval from the first to last qualifying point. Missing data or excessive gaps cannot bridge a response. Three SD is a noise-based rule, not a guaranteed whole-recording false-positive probability."),
                p("Apply analysis settings to create an immutable run. Editing settings leaves the applied results and exports unchanged and shows an unapplied-changes message. Changing processed data, preprocessing, condition, or experimental unit IDs invalidates the old run. Import/export JSON protocols to preserve the scoring contract and timing overrides."),
                p("Margin is peak rise divided by threshold. A spike can have margin above one and still fail persistence. Select a distribution point to review its cell. Quality flags are warnings, not automatic exclusions; large peaks and flat traces are not discarded as outliers."),
                p("Population traces shows one labeled panel per recording with shared axes. Choose Individual traces, Mean ± SEM, or Both; Focus enlarges one recording. Teal solid lines mark responders and orange dashed lines mark nonresponders. Gray shading marks the actual local baseline, the vertical dotted line marks stimulus delivery, and blue shading marks scoring. Clicking selects the cell for review. Visibility does not change percentages or exports."),
                p("Pooled percent is responders divided by all mapped, evaluable cells, including valid nonresponders. Excluded and unscorable cells are counted separately. A whole-field claim requires including all target cells independently of response. The optional positive control defines a separately reported population, such as stimulus responders among KCl responders; it is not an automatic universal viability criterion."),
                p("The complete analysis ZIP contains the applied protocol, calls, counts, stimulus-window peak/AUC/latency measurements, quality diagnostics, normalized traces, processing metadata, and experimental-unit summaries. The run ID and protocol hash connect these outputs. Legacy whole-recording metrics remain separate from stimulus-window measurements.")
              ),
              theme_box(
                title = "Interpreting Sample Size",
                icon = icon("users"),
                status = "primary",
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                div(class = "alert alert-warning", style = "margin: 0;",
                    "N_Cells is not the number of independent animals or preparations. For response summaries, assign experimental unit IDs in Recording details: fields are pooled within each ID, then unit percentages are weighted equally within condition. Missing IDs are reported and omitted from unit summaries. The software does not infer biological independence from a label; descriptive cell/field summaries remain available.")
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
                            tags$dt("Rise Time (10-90%)"), tags$dd("Time between the first 10% and 90% rising crossings before the post-baseline maximum. These whole-recording crossings can fall in different responses when multiple stimuli are present. Use Percent Responding stimulus-window metrics to analyze an individual stimulus."),
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
