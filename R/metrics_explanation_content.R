get_metric_explanation_content <- function(metric, ns) {
    switch(metric,
        "peak_dff0" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Peak ΔF/F₀ represents the maximum fluorescence response intensity of the cell. It is the highest point reached in the signal after baseline correction and indicates the strength of the cellular response."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>F(t):</b> Raw fluorescence intensity at time t")),
                tags$li(HTML("<b>F₀ (Baseline):</b> Average fluorescence during the baseline period (stable, pre-response phase)")),
                tags$li(HTML("<b>ΔF/F₀:</b> Normalized change in fluorescence: (F(t) - F₀) / F₀")),
                tags$li(HTML("<b>Peak:</b> The maximum value of the ΔF/F₀ trace"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("peak_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            p("The peak ΔF/F₀ is calculated by finding the maximum value after baseline correction:"),
            formula_line("Peak ΔF/F<sub>0</sub> = max&#8202;(&#8202;", frac("F(t) − F<sub>0</sub>", "F<sub>0</sub>"), "&#8202;)"),
            uiOutput(ns("peak_calculation_ui"))
        ),
        "response_amplitude" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Response Amplitude measures the magnitude of the cellular response from the baseline (resting state) to the peak fluorescence. In ΔF/F₀ normalized data, this is equivalent to the Peak ΔF/F₀ value, as the baseline is set to zero during normalization."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Baseline (F₀):</b> The resting fluorescence level before stimulation")),
                tags$li(HTML("<b>Peak Response:</b> The maximum fluorescence value reached")),
                tags$li(HTML("<b>Response Amplitude:</b> The difference between peak and baseline")),
                tags$li(HTML("<b>Normalized Data:</b> After ΔF/F₀ transformation, baseline = 0"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("response_amp_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            p("Response Amplitude is calculated as the peak value minus the baseline:"),
            formula_line("Response Amplitude = Peak − Baseline"),
            p("For ΔF/F₀ normalized data where baseline = 0:"),
            formula_line("Response Amplitude = Peak ΔF/F<sub>0</sub> − 0 = Peak ΔF/F<sub>0</sub>"),
            p("Units: ΔF/F₀ (unitless)"),
            uiOutput(ns("response_amp_calculation_ui"))
        ),
        "time_to_peak" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Time to Peak is the time coordinate at which the post-baseline ΔF/F₀ trace reaches its maximum value. It uses the uploaded or generated Time axis; it is not a stimulus-to-peak latency unless Time = 0 is explicitly aligned to stimulus onset."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Time Axis:</b> Uploaded elapsed time, or time generated from frame number and sampling rate")),
                tags$li(HTML("<b>Peak ΔF/F₀:</b> The maximum fluorescence response value")),
                tags$li(HTML("<b>Peak Time:</b> The exact time point when the signal first reaches its maximum")),
                tags$li(HTML("<b>Stimulus Latency:</b> Not calculated because the app does not collect a stimulus-onset time"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("ttpk_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            p("Time to Peak is determined by identifying when the signal first reaches its maximum value:"),
            formula_line("t<sub>peak</sub> = argmax<sub>t</sub>&#8202;(&#8202;", frac("F(t) − F<sub>0</sub>", "F<sub>0</sub>"), "&#8202;)"),
            uiOutput(ns("ttpk_calculation_ui"))
        ),
        "snr" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Signal-to-Noise Ratio (SNR) quantifies the strength of the cellular response relative to background noise. A higher SNR indicates a clearer, more reliable signal that can be distinguished from random fluctuations."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Signal:</b> The response amplitude (Peak ΔF/F₀ value)")),
                tags$li(HTML("<b>Noise:</b> Random fluctuations in the baseline fluorescence")),
                tags$li(HTML("<b>Baseline SD:</b> Standard deviation of fluorescence during the stable baseline period")),
                tags$li(HTML("<b>Response Amplitude:</b> The magnitude of the peak response above baseline"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("snr_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            p("SNR is calculated by dividing the response amplitude by the baseline noise:"),
            formula_line("SNR = ", frac("Response Amplitude", "Baseline SD")),
            uiOutput(ns("snr_calculation_ui"))
        ),
        "baseline_sd" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Baseline Standard Deviation (Baseline SD) quantifies the level of noise or random fluctuation in the fluorescence signal during the resting state before stimulation. It represents the inherent variability of the measurement system and is crucial for assessing signal quality."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Baseline Period:</b> The time window before stimulation when the cell is at rest")),
                tags$li(HTML("<b>Standard Deviation (SD):</b> A measure of variability or spread in the data")),
                tags$li(HTML("<b>Noise Floor:</b> The minimum detectable signal change above background fluctuations")),
                tags$li(HTML("<b>Signal Quality:</b> Lower baseline SD indicates cleaner, more reliable measurements"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("baseline_sd_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            p("Baseline SD is the standard deviation of the ΔF/F₀ values during the baseline period:"),
            formula_line("Baseline SD = √(&#8202;", frac("Σ<sub>i</sub> (x<sub>i</sub> − x̄)<sup>2</sup>", "n − 1"), "&#8202;)"),
            p("where x₁, x₂, ..., xₙ are the ΔF/F₀ values during the baseline frames and x̄ is their mean."),
            p("Units: ΔF/F₀ (unitless)"),
            uiOutput(ns("baseline_sd_calculation_ui"))
        ),
        "rise_time" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Rise Time (10-90%) measures the speed of the signal's initial ascent during activation. It quantifies how quickly the fluorescence signal increases from 10% to 90% of its peak amplitude, indicating the rapidity of the cellular response."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>10% Point:</b> Time when signal first reaches 10% of response amplitude")),
                tags$li(HTML("<b>90% Point:</b> Time when signal first reaches 90% of response amplitude")),
                tags$li(HTML("<b>Response Amplitude:</b> Peak ΔF/F₀ value above baseline")),
                tags$li(HTML("<b>Rise Interval:</b> The interval between the first 10% and 90% rising crossings after the baseline window"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("rise_time_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            p("Rise time is calculated as the duration between the 10% and 90% amplitude points:"),
            formula_line("Rise Time = t<sub>90%</sub> − t<sub>10%</sub>"),
            uiOutput(ns("rise_time_calculation_ui"))
        ),
        "time_to_percent_peak" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Time to % Peak reports the time coordinates at which the post-baseline signal first reaches 25%, 50%, and 75% of its response amplitude. These are positions on the uploaded or generated Time axis, not stimulus latencies unless Time = 0 is aligned to stimulus onset."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Peak Amplitude:</b> Maximum ΔF/F₀ value reached by the cell")),
                tags$li(HTML("<b>25% Threshold:</b> One-quarter of the peak amplitude")),
                tags$li(HTML("<b>50% Threshold:</b> Half of the peak amplitude")),
                tags$li(HTML("<b>75% Threshold:</b> Three-quarters of the peak amplitude")),
                tags$li(HTML("<b>Rising Phase:</b> Period when signal increases toward peak"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("ttp_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            p("Each time point is determined when the signal first crosses the threshold:"),
            formula_line("t<sub>25%</sub> = time when ΔF/F<sub>0</sub> first reaches 0.25 × Peak"),
            formula_line("t<sub>50%</sub> = time when ΔF/F<sub>0</sub> first reaches 0.50 × Peak"),
            formula_line("t<sub>75%</sub> = time when ΔF/F<sub>0</sub> first reaches 0.75 × Peak"),
            uiOutput(ns("ttp_calculation_ui"))
        ),
        "fwhm" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Full-Width at Half-Maximum (FWHM) measures the duration that a signal remains above 50% of its response amplitude. An exact FWHM requires observing both the rising and falling half-maximum crossings."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Half-Maximum:</b> Baseline plus 50% of the response amplitude")),
                tags$li(HTML("<b>Left Crossing:</b> Last interpolated rising crossing between the baseline window and the peak (t<sub>left</sub>)")),
                tags$li(HTML("<b>Right Crossing:</b> First interpolated falling crossing after the peak (t<sub>right</sub>)")),
                tags$li(HTML("<b>Right-Censored:</b> The recording ends before the right crossing, so only a lower bound is known")),
                tags$li(HTML("<b>Derived Half-Width:</b> FWHM ÷ 2; it is not a separately measured peak-to-crossing interval and is not calculated for censored responses"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("fwhm_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            p("FWHM is calculated as the time difference between crossing points at half-maximum:"),
            formula_line("FWHM = t<sub>right</sub> − t<sub>left</sub>"),
            p("If the right crossing is not observed, exact FWHM is reported as missing and the observed duration from the left crossing to the last sample is reported as a lower bound."),
            uiOutput(ns("fwhm_calculation_ui"))
        ),
        "auc" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Area Under Curve (AUC) is the signed net integral of the ΔF/F₀ trace relative to zero over the entire recording. Values above zero contribute positively and values below zero contribute negatively. Its units are ΔF/F₀ × seconds."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Integration:</b> Mathematical process of calculating the area under a curve")),
                tags$li(HTML("<b>Trapezoidal Rule:</b> Numerical method for approximating definite integrals")),
                tags$li(HTML("<b>Time Points:</b> Discrete sampling intervals (t₀, t₁, t₂, ... tₙ)")),
                tags$li(HTML("<b>Signal Values:</b> ΔF/F₀ measurements at each time point")),
                tags$li(HTML("<b>Missing Samples:</b> Only adjacent finite sample pairs are integrated; the calculation does not bridge an unobserved interval"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("auc_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            p("In this app, AUC is calculated using the trapezoidal rule with the following steps:"),
            tags$ol(
                tags$li("Calculate time differences between consecutive points: Δt = t(i+1) - t(i)"),
                tags$li("Calculate average heights between consecutive points: heights = (y(i+1) + y(i)) / 2"),
                tags$li("Sum the products: AUC = Σ(Δt × heights)")
            ),
            formula_line("AUC = Σ<sub>i</sub> Δt<sub>i</sub> × ", frac("y<sub>i</sub> + y<sub>i+1</sub>", "2")),
            p(HTML("where Δt<sub>i</sub> = t<sub>i+1</sub> − t<sub>i</sub> and y<sub>i</sub> is the ΔF/F₀ value at each time point. The sum includes only adjacent pairs with finite, increasing Time values and finite signals.")),
            uiOutput(ns("auc_calculation_ui"))
        ),
        "ca_entry_rate" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("The 10–90% ΔF/F₀ Rise Rate measures the average slope of the normalized fluorescence signal between its 10% and 90% rising crossings. It is a fluorescence-kinetics metric, not a direct measurement of calcium influx."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>10% Point:</b> The time and amplitude when the signal first reaches 10% of its peak value")),
                tags$li(HTML("<b>90% Point:</b> The time and amplitude when the signal first reaches 90% of its peak value")),
                tags$li(HTML("<b>Rise Time:</b> The duration between the 10% and 90% points (t<sub>90%</sub> - t<sub>10%</sub>)")),
                tags$li(HTML("<b>Rise Rate:</b> The slope of the line connecting these two fluorescence points, in ΔF/F₀/s"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("ca_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            p("The 10–90% ΔF/F₀ rise rate is calculated as the slope between the 10% and 90% fluorescence points:"),
            uiOutput(ns("ca_calculation_ui"))
        )
    )
}
