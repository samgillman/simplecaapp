# SimpleCa²⁺: Complete Application Specification
## Exhaustive Feature Documentation for AI-Assisted Rebuild

**Version:** 1.1.0  
**Last Updated:** 2025-11-02  
**Purpose:** Complete specification document describing every feature, control, calculation, and UI element for rebuilding this application in any framework.

---

## TABLE OF CONTENTS

1. [Application Architecture](#1-application-architecture)
2. [Data Input & Processing](#2-data-input--processing)
3. [Time Course Visualization](#3-time-course-visualization)
4. [Metrics Calculation & Visualization](#4-metrics-calculation--visualization)
5. [Heatmap Module](#5-heatmap-module)
6. [Metric Explanations Module](#6-metric-explanations-module)
7. [Data Tables Module](#7-data-tables-module)
8. [Export Module](#8-export-module)
9. [Help Module](#9-help-module)
10. [UI Components & Design System](#10-ui-components--design-system)
11. [Future Features: Group Analysis](#11-future-features-group-analysis)

---

## 1. APPLICATION ARCHITECTURE

### 1.1 Technology Stack
- **Language:** R (≥4.0)
- **Web Framework:** Shiny (≥1.8)
- **UI Framework:** Shinydashboard
- **Data Processing:** data.table, dplyr, purrr
- **Visualization:** ggplot2 (static), plotly (interactive)
- **Tables:** DT (DataTables), gt, kableExtra
- **File I/O:** readxl, data.table::fwrite
- **Styling:** Custom CSS with CSS Variables (Design Tokens)
- **JavaScript:** Custom accordion toggle functions

### 1.2 Application Structure
- **Main Entry:** `app.R` (284 lines)
- **Module Pattern:** Shiny modules (one UI function, one server function per module)
- **Shared State:** Single `reactiveValues` object (`rv`) passed to all modules
- **Module Files:**
  - `R/mod_load_data.R` - Data upload and processing
  - `R/mod_preproc.R` - Processed data review
  - `R/mod_time_course.R` - Time course plots
  - `R/mod_heatmap.R` - Population heatmaps
  - `R/mod_metrics.R` - Metrics visualization
  - `R/mod_metrics_explained.R` - Educational metric breakdowns
  - `R/mod_tables.R` - Interactive data tables
  - `R/mod_export.R` - Centralized export hub
  - `R/mod_help.R` - User documentation
- **Utility Files:**
  - `R/utils.R` - Core calculation functions
  - `R/components.R` - Reusable UI components
  - `R/theme.R` - Design system CSS

### 1.3 Sidebar Navigation
Nine menu items in order:
1. **Load Data** (icon: database)
2. **Processed Data** (icon: sliders)
3. **Time Course** (icon: chart-line)
4. **Heatmap** (icon: th)
5. **Metrics** (icon: chart-bar)
6. **Metric Explanations** (icon: lightbulb)
7. **Tables** (icon: table)
8. **Export** (icon: download)
9. **Help** (icon: circle-question)

---

## 2. DATA INPUT & PROCESSING

### 2.1 File Upload (`mod_load_data.R`)

#### 2.1.1 Upload Interface
- **Component:** `fileInput` widget
- **Label:** "Upload CSV or Excel (wide format)"
- **Features:**
  - **Multiple file selection:** Enabled (`multiple = TRUE`)
  - **Accepted formats:** `.csv`, `.xlsx`, `.xls`
  - **Width:** 100% of container
  - **Helper text:** "Format: First column must be 'Time', subsequent columns are cells."
- **JavaScript Enhancement:** Custom script that makes entire file input area clickable (overlay technique with opacity: 0)

#### 2.1.2 Baseline Correction Methods
**Dropdown:** `selectInput` with label "Method"

**Three Options:**
1. **"Frame Range"** (`value = "frame_range"`)
   - **Default selection:** Yes
   - **Conditional Control:** When selected, shows `sliderInput` for "Baseline Frames"
   - **Slider Range:** 1 to 100 frames
   - **Default Value:** `c(1, 20)` (start frame, end frame)
   - **Step:** 1 frame
   - **Width:** 100% of column

2. **"Rolling Minimum"** (`value = "rolling_min"`)
   - **Conditional Control:** When selected, shows `numericInput` for "Window Size (frames)"
   - **Default Value:** 50 frames
   - **Minimum:** 5 frames
   - **Step:** 1 frame
   - **Width:** 100% of column

3. **"Percentile"** (`value = "percentile"`)
   - **Conditional Control:** When selected, shows `numericInput` for "Percentile (1-50)"
   - **Default Value:** 10th percentile
   - **Range:** 1 to 50
   - **Step:** 1
   - **Width:** 100% of column

#### 2.1.3 Advanced Options Accordion
- **Title:** "Advanced Options"
- **Icon:** sliders-h
- **Default State:** Collapsed
- **Content:**
  - **Sampling Rate Override:** `numericInput`
    - **Label:** "Sampling Rate (Hz) - used if Time column missing"
    - **Default:** 1 Hz
    - **Minimum:** 0.0001
    - **Step:** 0.1
    - **Helper Text:** "Note: Only applies if the uploaded file lacks a valid 'Time' column."

#### 2.1.4 Process Data Button
- **Component:** `primary_button` (custom styled)
- **Label:** "Process Data"
- **Icon:** play icon
- **Width:** 160px
- **Location:** Footer bar with formula display
- **Formula Display:** Shows "ΔF/F₀ = (F - F₀)/F₀" next to button

### 2.2 Data Overview Dashboard

#### 2.2.1 Stat Cards (Three Cards in Row)
1. **Files Card**
   - **Label:** "Files"
   - **Value:** `textOutput("n_files_text")` - displays count of loaded datasets
   - **Styling:** Large blue number (40px), uppercase label

2. **Total Cells Card**
   - **Label:** "Total Cells"
   - **Value:** `textOutput("n_cells_text")` - displays total cell count across all files
   - **Styling:** Same as Files card

3. **Timepoints Card**
   - **Label:** "Timepoints"
   - **Value:** `textOutput("n_timepoints_text")` - displays sum of all timepoints
   - **Styling:** Same as Files card

#### 2.2.2 Processing Status Pipeline
**Four-Step Visual Pipeline:**

1. **Files Loaded** (icon: file-import)
   - **Status Text:** `textOutput("status_files_loaded")`
   - **Displays:** "No files" or "N file(s)"
   - **Color:** Info blue

2. **Processing** (icon: check-circle)
   - **Status Text:** `textOutput("status_processing")`
   - **Displays:** "Not started" or "Complete"
   - **Color:** Success green

3. **Metrics** (icon: calculator)
   - **Status Text:** `textOutput("status_metrics")`
   - **Displays:** "Not calculated" or "N cells"
   - **Color:** Warning orange

4. **Ready** (icon: chart-line)
   - **Status Text:** `textOutput("status_ready")`
   - **Displays:** "Waiting" or "Ready"
   - **Color:** Primary blue

### 2.3 Data Processing Logic

#### 2.3.1 File Reading
- **CSV Files:** Uses `data.table::fread()`
- **Excel Files:** Uses `readxl::read_excel()` with `.name_repair = "minimal"`
- **Error Handling:** Wrapped in `safe_read()` function with try-catch

#### 2.3.2 Time Column Validation
- **Check:** Ensures first column is numeric and monotonically increasing
- **Auto-Generation:** If invalid, generates time vector using sampling rate:
  - Formula: `seq(0, by = 1/sampling_rate, length.out = nrow)`
- **Renaming:** First column always renamed to "Time"

#### 2.3.3 Baseline Calculation (F₀)

**Method 1: Frame Range**
```r
start_frame <- max(1, baseline_frames[1])
end_frame <- min(nrow(dt), baseline_frames[2])
F0 <- mean(dt[[cell_column]][start_frame:end_frame], na.rm=TRUE)
```

**Method 2: Rolling Minimum**
```r
window_size <- max(5, window_size_input)
rolling_mean <- zoo::rollmean(signal, k=window_size, fill=NA)
F0 <- min(rolling_mean, na.rm=TRUE)
```

**Method 3: Percentile**
```r
percentile_value <- max(1, min(50, percentile_input))
F0 <- stats::quantile(signal, probs=percentile_value/100, na.rm=TRUE, names=FALSE)
```

#### 2.3.4 ΔF/F₀ Normalization
**Formula:** `(F(t) - F₀) / F₀`

**Edge Case Handling:**
- **If F₀ > 1e-6:** Standard normalization: `(F - F₀) / F₀`
- **If F₀ ≤ 1e-6:** Subtraction only: `F - F₀` (prevents division by zero)
- **If F₀ is not finite:** Returns NA for that cell

#### 2.3.5 Data Structure Creation
After processing, creates:
- **`rv$dts`:** Named list of processed data.tables (one per file/group)
- **`rv$raw_traces`:** Named list of original (unprocessed) data.tables
- **`rv$baselines`:** Named list of baseline values (one vector per group, named by cell)
- **`rv$long`:** Long-format data.table with columns: Time, Cell, dFF0, Group, Cell_ID
- **`rv$summary`:** Aggregated summary with columns: Group, Time, mean_dFF0, sem_dFF0, sd_dFF0, n_cells
- **`rv$metrics`:** Data.table with one row per cell, all calculated metrics

#### 2.3.6 Progress Indicator
- **Component:** `withProgress()` wrapper
- **Message:** "Processing data..."
- **Detail:** Shows current file being processed: "Loading: [filename]"
- **Progress:** Increments by 1/n_files for each file

---

## 3. TIME COURSE VISUALIZATION

### 3.1 Plot Display Area

#### 3.1.1 Static/Interactive Toggle
- **Component:** `radioGroupButtons`
- **Location:** Top-right of plot box
- **Options:** "Static" (default), "Interactive"
- **Size:** Small (`size = "sm"`)
- **Status:** Primary blue

#### 3.1.2 Static Plot Output
- **Component:** `plotOutput("timecourse_plot")`
- **Height:** 760px
- **Spinner:** Type 4 (shinycssloaders)
- **Empty State:** Shows instructional text if no data

#### 3.1.3 Interactive Plot Output
- **Component:** `plotlyOutput("timecourse_plotly")`
- **Height:** 760px
- **Spinner:** Type 4
- **Features:** Tooltip with Group, Cell, Time, Value
- **Layout:** Zoom enabled, drag mode: zoom
- **Config:** Display mode bar ON, logo OFF, removes lasso/select tools

### 3.2 Controls Sidebar (Left Column, Width: 4)

#### 3.2.1 Display Options Accordion (Expanded by Default)
- **Icon:** eye
- **Controls:**
  1. **Show Individual Traces**
     - **Type:** `switchInput` (toggle switch)
     - **Default:** TRUE (ON)
     - **Size:** mini
     - **Effect:** Overlays all individual cell traces on plot
  
  2. **Trace Transparency**
     - **Type:** `sliderInput`
     - **Label:** "Trace transparency (%)"
     - **Range:** 0% to 100%
     - **Default:** 50%
     - **Step:** 1%
     - **Effect:** Controls alpha of individual trace lines
     - **Calculation:** `alpha = (100 - transparency_pct) / 100`, then `alpha^1.5` for darker appearance
  
  3. **Show SEM Ribbon**
     - **Type:** `switchInput`
     - **Default:** TRUE (ON)
     - **Effect:** Shows/hides shaded ribbon around mean line
  
  4. **Line Width**
     - **Type:** `sliderInput`
     - **Range:** 0.5 to 4
     - **Default:** 1.6
     - **Step:** 0.1
     - **Effect:** Controls thickness of mean line
  
  5. **Y-Axis Scale Step Size**
     - **Type:** `sliderInput`
     - **Label:** "Y-axis step size"
     - **Range:** 0.1 to 1.0
     - **Default:** 0.5
     - **Step:** 0.1
     - **Effect:** Controls spacing of Y-axis tick marks

#### 3.2.2 Colors & Style Accordion (Collapsed by Default)
- **Icon:** palette
- **Controls:**
  1. **Line Color**
     - **Type:** `colourpicker::colourInput`
     - **Default:** #000000 (black)
     - **Effect:** If set, applies color to all groups; if NULL, uses group-specific colors
  
  2. **Legend Position**
     - **Type:** `selectInput`
     - **Options:** "none" (default), "bottom", "right", "top", "left"
     - **Effect:** Controls legend placement
  
  3. **Theme**
     - **Type:** `selectInput`
     - **Options:** "classic" (default), "minimal", "light", "dark"
     - **Effect:** Changes ggplot2 theme
  
  4. **Major Gridlines**
     - **Type:** `checkboxInput`
     - **Default:** FALSE
     - **Effect:** Shows/hides major grid lines
  
  5. **Minor Gridlines**
     - **Type:** `checkboxInput`
     - **Default:** FALSE
     - **Effect:** Shows/hides minor grid lines

#### 3.2.3 Labels Accordion (Collapsed by Default)
- **Icon:** tag
- **Controls:**
  1. **Title**
     - **Type:** `textInput`
     - **Default:** Auto-populated with group names (comma-separated)
     - **Reset Button:** "Reset" button next to title input
     - **Reset Action:** Restores default (group names)
  
  2. **X Axis Label**
     - **Type:** `textInput`
     - **Default:** "Time (s)"
  
  3. **Y Axis Label**
     - **Type:** `textInput`
     - **Default:** "ΔF/F₀"
     - **Special Handling:** If left as default, renders as mathematical expression with subscript
  
  4. **Log10 Y Axis**
     - **Type:** `checkboxInput`
     - **Default:** FALSE
     - **Effect:** Applies logarithmic scale to Y-axis

#### 3.2.4 Typography Accordion (Collapsed by Default)
- **Icon:** font
- **Controls:**
  1. **Title Size**
     - **Type:** `sliderInput`
     - **Range:** 10 to 24
     - **Default:** 18
     - **Step:** 1
  
  2. **Bold Title**
     - **Type:** `checkboxInput`
     - **Default:** TRUE
  
  3. **Axis Title Size**
     - **Type:** `sliderInput`
     - **Range:** 8 to 24
     - **Default:** 14
     - **Step:** 1
  
  4. **Bold Axis Titles**
     - **Type:** `checkboxInput`
     - **Default:** TRUE
  
  5. **Axis Text Size**
     - **Type:** `sliderInput`
     - **Range:** 8 to 24
     - **Default:** 12
     - **Step:** 1
  
  6. **Bold Axis Text**
     - **Type:** `checkboxInput`
     - **Default:** FALSE
  
  7. **Font Family**
     - **Type:** `selectInput`
     - **Options:** "Arial" (default), "Helvetica", "Times", "Courier"

#### 3.2.5 Axis Limits Accordion (Collapsed by Default)
- **Icon:** arrows-alt
- **Controls:**
  1. **Enable Custom Axis Limits**
     - **Type:** `checkboxInput`
     - **Default:** FALSE
     - **Effect:** When checked, reveals four numeric inputs
  
  2. **X Min** (conditional)
     - **Type:** `numericInput`
     - **Default:** NA (empty)
  
  3. **X Max** (conditional)
     - **Type:** `numericInput`
     - **Default:** NA (empty)
  
  4. **Y Min** (conditional)
     - **Type:** `numericInput`
     - **Default:** NA (empty)
  
  5. **Y Max** (conditional)
     - **Type:** `numericInput`
     - **Default:** NA (empty)

#### 3.2.6 Advanced Options Accordion (Collapsed by Default)
- **Icon:** cog
- **Controls:**
  1. **X Axis Breaks**
     - **Type:** `textInput`
     - **Label:** "X axis breaks (comma-separated)"
     - **Default:** Empty
     - **Format:** "0, 10, 20, 30"
     - **Effect:** Sets explicit tick positions
  
  2. **Y Axis Breaks**
     - **Type:** `textInput`
     - **Label:** "Y axis breaks (comma-separated)"
     - **Default:** Empty
     - **Format:** "0, 0.5, 1.0, 1.5"
     - **Effect:** Overrides scale step slider
  
  3. **Tick Format**
     - **Type:** `selectInput`
     - **Options:** "number" (default), "scientific", "percent"
     - **Effect:** Controls how numbers are displayed on axes

#### 3.2.7 Export Options Accordion (Collapsed by Default)
- **Icon:** download
- **Controls:**
  1. **Format**
     - **Type:** `selectInput`
     - **Options:** PNG (default), PDF, TIFF, SVG
  
  2. **Size Preset**
     - **Type:** `selectInput`
     - **Options:** "6x4 in", "7x5 in", "8x6 in" (default), "10x7.5 in", "12x8 in"
     - **Effect:** Auto-fills width/height inputs
  
  3. **Width (in)**
     - **Type:** `numericInput`
     - **Range:** 4 to 30
     - **Default:** 8
     - **Step:** 0.5
  
  4. **Height (in)**
     - **Type:** `numericInput`
     - **Range:** 4 to 30
     - **Default:** 6
     - **Step:** 0.5
  
  5. **DPI**
     - **Type:** `numericInput`
     - **Range:** 72 to 600
     - **Default:** 300
     - **Step:** 5
  
  6. **Download Button**
     - **Label:** "Download Time Course"
     - **Class:** btn-primary
     - **Width:** 100%
     - **Filename Format:** `{basename}_{timecourse}_{date}.{ext}`

### 3.3 Plot Construction Logic

#### 3.3.1 Individual Traces Layer
- **Condition:** Only if `tc_show_traces == TRUE`
- **Data Source:** `rv$long`
- **Geometry:** `geom_line()`
- **Grouping:** `group = interaction(Group, Cell)`
- **Color Logic:**
  - **Single Group:** All traces gray50
  - **Multiple Groups:** Colored by Group (uses `rv$colors`)
- **Alpha:** Calculated from transparency slider (non-linear transformation)
- **Line Width:** 0.4 (fixed, thin)
- **Tooltip:** Includes Group, Cell, Time, Value (for plotly)

#### 3.3.2 SEM Ribbon Layer
- **Condition:** Only if `tc_show_ribbon == TRUE`
- **Data Source:** `rv$summary`
- **Geometry:** `geom_ribbon()`
- **Y Range:** `ymin = mean_dFF0 - sem_dFF0`, `ymax = mean_dFF0 + sem_dFF0`
- **Fill Color:** Uses custom line color if set, else gray50
- **Alpha:** 0.25 (fixed)

#### 3.3.3 Mean Line Layer
- **Data Source:** `rv$summary`
- **Geometry:** `geom_line()`
- **Y Value:** `mean_dFF0`
- **Color Logic:**
  - **If custom color set:** Maps to Group (all groups same color)
  - **If no custom color:** Black (single group) or Group colors (multiple groups)
- **Line Width:** Controlled by slider (default 1.6)
- **Tooltip:** Includes Group, Time, Mean, SEM

#### 3.3.4 Color Scale Application
- **Condition:** Only if multiple groups OR custom color set
- **Scale Type:** `scale_color_manual()`
- **Color Source:** `rv$colors` (auto-generated from group names)
- **Fallback:** If groups missing colors, uses rainbow() palette

#### 3.3.5 Y-Axis Break Generation
**Priority Order:**
1. **If custom Y breaks provided:** Use those exactly
2. **If scale step slider set:** Generate breaks from 0 to rounded max
   - Formula: `seq(0, ceiling(max/step)*step, by=step)`
3. **If custom Y limits set:** Regenerate breaks within limits using step
4. **Default:** ggplot2 automatic breaks

### 3.4 Summary Statistics Table
- **Location:** Below main plot (full width)
- **Title:** "Time Course Summary Statistics"
- **Content:** HTML table showing Mean ± SEM and n for key metrics
- **Metrics Displayed:** Peak ΔF/F₀, AUC, Half-Width, Ca²⁺ Entry Rate, Time to Peak, Time to 25/50/75% Peak, Rise Time, SNR
- **Formatting:** Rounded to 4 decimal places
- **Styling:** Bootstrap table (condensed, striped, hover)

---

## 4. METRICS CALCULATION & VISUALIZATION

### 4.1 Calculated Metrics (Complete List)

#### 4.1.1 Peak Metrics
- **Peak_dFF0:** Maximum ΔF/F₀ value (excludes baseline period)
- **Response_Amplitude:** Peak_dFF0 - Baseline (always equals Peak_dFF0 for normalized data)

#### 4.1.2 Temporal Metrics
- **Time_to_Peak:** Time (seconds) from recording start to peak
- **Time_to_25_Peak:** Time to reach 25% of peak amplitude
- **Time_to_50_Peak:** Time to reach 50% of peak amplitude
- **Time_to_75_Peak:** Time to reach 75% of peak amplitude
- **Rise_Time:** Time from 10% to 90% of peak amplitude

#### 4.1.3 Shape Metrics
- **FWHM:** Full Width at Half Maximum (duration above 50% peak)
- **Half_Width:** Half of FWHM (HWHM)

#### 4.1.4 Integrated Metrics
- **AUC:** Area Under Curve (trapezoidal integration of entire trace)

#### 4.1.5 Quality Metrics
- **Baseline_SD:** Standard deviation of baseline period
- **SNR:** Signal-to-Noise Ratio = Response_Amplitude / Baseline_SD

#### 4.1.6 Rate Metrics
- **Calcium_Entry_Rate:** (90% amplitude - 10% amplitude) / Rise_Time

### 4.2 Metric Calculation Algorithm

#### 4.2.1 Peak Detection (Critical Feature)
- **Baseline Exclusion:** Peak search starts AFTER baseline period ends
- **Implementation:** Sets `search_region[1:end_frame] = -Inf`
- **Peak Index:** `which.max(search_region)`
- **Validation:** If peak_idx ≤ end_frame, returns NA for all metrics

#### 4.2.2 Rise Time Calculation
- **Threshold:** 10% and 90% of Response_Amplitude
- **Search Window:** From baseline end to peak index
- **Method:** Linear interpolation between time points
- **Function:** `find_rising_crossing_time()` (custom utility)
- **Result:** `t_90% - t_10%`

#### 4.2.3 FWHM Calculation
- **Half-Maximum:** Peak_dFF0 / 2
- **Left Crossing:** First time signal rises above half-max (before peak)
- **Right Crossing:** First time signal falls below half-max (after peak)
- **Interpolation:** Linear interpolation for sub-frame accuracy
- **Edge Case:** If signal never falls below half-max, uses end of trace (sustained response)

#### 4.2.4 AUC Calculation
- **Method:** Trapezoidal rule
- **Formula:** `Σ[(y_i + y_{i+1})/2 × (t_{i+1} - t_i)]`
- **Integration:** Over entire trace (all time points)

#### 4.2.5 Time to % Peak Calculation
- **Thresholds:** 25%, 50%, 75% of Peak_dFF0
- **Search:** From baseline end to peak index
- **Method:** Linear interpolation
- **Returns:** Time (seconds) for each threshold

### 4.3 Metrics Visualization Controls

#### 4.3.1 Metric & Display Accordion (Expanded by Default)
- **Icon:** chart-bar
- **Controls:**
  1. **Metric Selection**
     - **Type:** `selectInput`
     - **Options:** All 11 metrics listed above
     - **Default:** "Peak ΔF/F₀"
  
  2. **Plot Style**
     - **Type:** `selectInput`
     - **Options:**
       - "Box + swarm" (`boxswarm`) - Boxplot with jittered points
       - "Bars" (`bars`) - Bar chart (default)
       - "Violin" (`violin`) - Violin plot with jittered points
  
  3. **Bar Color** (conditional, only for bars style)
     - **Type:** `colourpicker::colourInput`
     - **Default:** #B3B3B3 (light gray)
     - **Transparency:** Not allowed
  
  4. **Sort Cell Bars**
     - **Type:** `checkboxInput`
     - **Default:** TRUE
     - **Effect:** Sorts bars by metric value (ascending)
  
  5. **Show Mean ± SEM**
     - **Type:** `checkboxInput`
     - **Default:** TRUE
     - **Effect:** Overlays horizontal line (mean) and shaded region (±SEM)

#### 4.3.2 Labels Accordion (Collapsed by Default)
- **Icon:** tag
- **Controls:**
  1. **Custom Title**
     - **Type:** `textInput`
     - **Default:** Empty (auto-generates from metric name)
     - **Auto-Label:** Uses `metric_title()` function for default
  
  2. **Auto Y-Label**
     - **Type:** `checkboxInput`
     - **Default:** TRUE
     - **Effect:** Uses `metric_label()` function for scientific notation
  
  3. **Y Label** (conditional, only if auto unchecked)
     - **Type:** `textInput`
     - **Default:** "Value"

#### 4.3.3 Appearance Accordion (Collapsed by Default)
- **Icon:** paint-brush
- **Controls:**
  1. **Inset Size**
     - **Type:** `sliderInput`
     - **Range:** 0.5 to 3.0
     - **Default:** 1.0
     - **Step:** 0.1
     - **Effect:** Scales the summary statistics label (bars only)
  
  2. **Highlight Top/Bottom K**
     - **Type:** `numericInput`
     - **Range:** 0 to 100
     - **Default:** 0
     - **Step:** 1
     - **Effect:** Highlights K highest and K lowest bars in cyan (#5bc0de)
  
  3. **Bold Axis Titles**
     - **Type:** `checkboxInput`
     - **Default:** TRUE
  
  4. **Font Family**
     - **Type:** `selectInput`
     - **Options:** Sans-Serif (default), Serif, Monospace
  
  5. **Base Font Size**
     - **Type:** `sliderInput`
     - **Range:** 8 to 22
     - **Default:** 14
     - **Step:** 1

### 4.4 Plot Types Detailed

#### 4.4.1 Bar Chart Style
- **X-Axis:** Cell_Idx (sequential number, 1 to N)
- **Y-Axis:** Selected metric value
- **Bar Width:** 0.85 (85% of bin width)
- **Bar Fill:** User-selected color (default gray)
- **Bar Border:** Black, linewidth 0.2
- **Bar Alpha:** 0.9
- **X-Axis Labels:** Rotated 90°, small font (60% of base size, minimum 7pt)
- **Highlighting:** Top/bottom K bars colored cyan (#5bc0de)
- **Summary Label:** Positioned at x=1.5, y=98% of max value
- **Label Content:** "Mean ± SEM: X.XXX ± X.XXX\nn = N"
- **Label Size:** Scaled by `metric_size × 0.18 × inset_scale`

#### 4.4.2 Box + Swarm Style
- **X-Axis:** Single category "Cells"
- **Y-Axis:** Selected metric value
- **Boxplot:** Width 0.25, fill gray85, no outliers
- **Jitter Points:** Width 0.12, height 0, size 1, alpha 0.5
- **Mean ± SEM:** Horizontal line at mean, shaded rectangle ±SEM

#### 4.4.3 Violin Style
- **X-Axis:** Single category "Cells"
- **Y-Axis:** Selected metric value
- **Violin:** Width 0.8, fill gray85, trim=FALSE
- **Jitter Points:** Width 0.12, height 0, size 1, alpha 0.4
- **Mean ± SEM:** Same overlay as box+swarm

### 4.5 Export Controls (Below Plot)
- **Format:** PNG (default), PDF, SVG, TIFF
- **Size Preset:** 6x4, 7x5, 8x6 (default), 10x7.5, 12x8 inches
- **Width/Height:** Numeric inputs (3-20 inches, step 0.5)
- **DPI:** 72-600 (default 300, step 5)
- **Download Button:** "Download Plot"
- **Filename:** `{basename}_{metric_name}_plot_{date}.{ext}`

---

## 5. HEATMAP MODULE

### 5.1 Plot Display
- **Type:** Static only (no interactive toggle - removed due to rendering errors)
- **Component:** `plotOutput("heatmap_plot")`
- **Height:** 760px
- **Spinner:** Type 4
- **Geometry:** `geom_raster()` (efficient for large datasets)

### 5.2 Controls Sidebar (Width: 4)

#### 5.2.1 Display & Sorting Accordion (Expanded by Default)
- **Icon:** th
- **Controls:**
  1. **Sort Cells By**
     - **Type:** `selectInput`
     - **Options:**
       - "Time to Peak" (`tpeak`) - Default
       - "Peak Amplitude" (`amp`)
       - "Original" (`orig`)
     - **Effect:** Reorders Y-axis (cells)
  
  2. **Color Palette**
     - **Type:** `selectInput`
     - **Options:** plasma (default), viridis, magma, inferno, cividis
     - **Effect:** Changes color scale
  
  3. **Scale Step Size**
     - **Type:** `sliderInput`
     - **Range:** 0.1 to 1.0
     - **Default:** 0.5
     - **Step:** 0.1
     - **Effect:** Controls legend break spacing

#### 5.2.2 Labels Accordion (Collapsed by Default)
- **Icon:** tag
- **Controls:**
  1. **Plot Title**
     - **Type:** `textInput`
     - **Default:** "Population Heatmap"
  
  2. **Center Title**
     - **Type:** `checkboxInput`
     - **Default:** TRUE
  
  3. **X Label**
     - **Type:** `textInput`
     - **Default:** "Time (s)"
  
  4. **Y Label**
     - **Type:** `textInput`
     - **Default:** "Cell"

#### 5.2.3 Typography Accordion (Collapsed by Default)
- **Icon:** font
- **Controls:**
  1. **Title Size:** 10-24 (default 16)
  2. **Bold Title:** TRUE (default)
  3. **Axis Title Size:** 8-24 (default 14)
  4. **Bold Axis Titles:** TRUE (default)
  5. **Axis Text Size:** 8-24 (default 12)
  6. **Bold Axis Text:** FALSE (default)
  7. **Legend Text Size:** 6-24 (default 10)
  8. **Bold Legend Text:** FALSE (default)
  9. **Font Family:** Arial (default), Helvetica, Times, Courier

### 5.3 Heatmap Construction Logic

#### 5.3.1 Data Preparation
- **Input:** `rv$dts` (list of processed data.tables)
- **Processing:** For each group:
  1. Extract time vector and cell columns
  2. Convert to matrix (cells as columns)
  3. Filter out all-NA columns
  4. Sort columns based on user selection
  5. Create `expand.grid()` for Time × Cell
  6. Add Group and Cell_Label columns

#### 5.3.2 Sorting Algorithms
**Time to Peak Sort:**
```r
tpeak <- apply(matrix, 2, function(x) which.max(x))
order(tpeak)  # Ascending (earliest responders first)
```

**Peak Amplitude Sort:**
```r
amp <- apply(matrix, 2, function(x) max(x, na.rm=TRUE))
order(amp, decreasing=TRUE)  # Highest amplitude first
```

#### 5.3.3 Color Scale Handling
- **Negative Values:** Clamped to 0 for visualization (original data preserved)
- **Scale Range:** Always starts from 0
- **Upper Bound:** `ceiling(max_value / scale_step) * scale_step`
- **Breaks:** `seq(0, upper, by = scale_step)`
- **Scale Type:** `scale_fill_viridis_c()` with user-selected palette
- **NA Values:** Displayed as gray90 (light gray)
- **Out-of-Bounds:** Values above upper saturate (squish)

#### 5.3.4 Faceting
- **Method:** `facet_wrap(~ Group, ncol = 1, scales = "free_y")`
- **Effect:** Each experimental group gets its own heatmap row
- **Y-Scale:** Free (each group can have different number of cells)
- **Strip Labels:** Hidden (blank background, no text)

### 5.4 Export Controls
- **Format:** PNG (default), PDF, TIFF, SVG
- **Size Preset:** Same options as Time Course
- **Width/Height:** Same as Time Course
- **DPI:** Same as Time Course
- **Download Button:** "Download Heatmap"
- **Filename:** `{basename}_heatmap_{date}.{ext}`

---

## 6. METRIC EXPLANATIONS MODULE

### 6.1 Module Purpose
Educational tab that shows how each metric is calculated using the user's actual data.

### 6.2 Metric Selector
- **Component:** `selectInput` at top of page
- **Label:** "Select Metric to Explain:"
- **Options:** 11 metrics (same as Metrics tab)
- **Default:** "Peak ΔF/F₀"

### 6.3 Cell Selector Accordion (Expanded by Default)
- **Icon:** flask
- **Component:** `selectInput` (rendered via `renderUI`)
- **Label:** "Select a Cell to Visualize:"
- **Choices:** All cells from `rv$metrics`, formatted as "Group - Cell_Label"
- **Default:** First cell in list

### 6.4 Explanation Panel (Left Column, Width: 5)

#### 6.4.1 Content Structure (Per Metric)
Each metric explanation has four sections:

1. **Definition**
   - **Heading:** H4, styled with blue underline
   - **Content:** 1-2 paragraph explanation

2. **Key Terms**
   - **Heading:** H4
   - **Format:** Bulleted list (`<ul>`)
   - **Items:** 3-5 key terms with bold labels

3. **For This Cell**
   - **Heading:** H4
   - **Component:** `uiOutput()` (dynamically generated)
   - **Content:** Box with actual data values:
     - Baseline fluorescence (F₀)
     - Peak fluorescence (F)
     - Time of peak
     - Calculated metric value
     - Other relevant values

4. **Calculation**
   - **Heading:** H4
   - **Component:** `withMathJax()` wrapper
   - **Content:**
     - General formula (LaTeX)
     - Step-by-step calculation with actual numbers
     - Final result in green highlighted box

### 6.5 Visualization Panel (Right Column, Width: 7)

#### 6.5.1 Plot Type: Static Only
- **Component:** `plotOutput("explanation_plot")`
- **Height:** 600px
- **Reason:** Complex annotations don't convert to plotly

#### 6.5.2 Plot Annotations (Per Metric)

**Peak ΔF/F₀:**
- Baseline period shaded rectangle (if frame_range method)
- Vertical dashed line at peak time
- Red point at peak
- Text label showing peak value

**Time to Peak:**
- Vertical dashed line at peak
- Red point at peak
- Purple arrow from time=0 to peak time
- Label: "Time to Peak = X.XX s"

**Response Amplitude:**
- Baseline period shaded (if frame_range)
- Horizontal line at y=0 (baseline)
- Blue arrow from baseline to peak
- Label showing amplitude value

**SNR:**
- Baseline period ribbon showing ±1 SD
- Blue point at peak (labeled "Signal")
- Red ribbon showing noise range
- Label: "Baseline Noise (SD)"

**Baseline SD:**
- Baseline period highlighted
- Horizontal line at baseline mean
- Red ribbon showing ±1 SD
- Dashed lines at ±1 SD
- Labels showing SD values

**Rise Time:**
- Horizontal dotted lines at 10% and 90% thresholds
- Vertical dashed lines at t_10% and t_90%
- Orange points at crossing points
- Red arrow showing rise time duration
- Labels: "10%" and "90%"

**Time to % Peak:**
- Three horizontal lines: 25% (seagreen), 50% (goldenrod), 75% (firebrick)
- Vertical dashed lines at each time point
- Labels: "25%", "50%", "75%"

**FWHM:**
- Horizontal dashed line at half-maximum
- Vertical dotted lines at left and right crossings
- Red arrow showing FWHM width
- Orange arrow showing half-width
- Labels: "FWHM = X.XX s", "Half-Width = X.XX s"
- Note if sustained response

**AUC:**
- Green ribbon filling area under curve
- Shows total integrated area

**Calcium Entry Rate:**
- Gray dotted reference lines at 10% and 90%
- Blue thick line connecting 10% to 90% points
- Red points at 10% and 90%
- Blue arrow showing time interval
- Green label box showing final rate value

### 6.6 Download Options Accordion (Collapsed by Default)
- **Icon:** download
- **Controls:**
  - Format: PNG, PDF, SVG, TIFF
  - DPI: 72-600 (default 300)
  - Download Button: "Download Plot"
  - Filename: `{basename}_metric_explanation_{metric}_{cell}_{date}.{ext}`

### 6.7 Calculation Functions

#### 6.7.1 Baseline Frames Helper
- **Function:** `get_bl()`
- **Purpose:** Safely retrieves baseline frame range
- **Fallback:** Returns `c(1, 20)` if `rv$baseline_frames` is NULL
- **Critical:** Prevents crashes when non-frame-range baseline methods are used

#### 6.7.2 Rising Crossing Time
- **Function:** `find_rising_crossing_time()`
- **Parameters:** signal, time_vec, threshold, search_start_idx, search_end_idx
- **Method:** Linear interpolation between time points
- **Returns:** Interpolated time (seconds) or NA

---

## 7. DATA TABLES MODULE

### 7.1 Tab Structure
Four tabs in `tabsetPanel` (type: "pills"):

#### 7.1.1 Tab 1: Cell Metrics
- **Icon:** calculator
- **Title:** "Individual Cell Metrics"
- **Table:** `DT::DTOutput("cell_metrics_table")`
- **Data Source:** `rv$metrics`
- **Columns:** All metric columns plus Group, Cell, Cell_ID, Cell_Label
- **Features:**
  - **Page Length:** 25 rows
  - **Scroll:** Horizontal enabled
  - **Buttons:** Copy, CSV, Excel
  - **Filter:** Top row filter inputs
  - **Styling:** Compact, striped, hover
  - **Numeric Formatting:** 4 decimal places
- **Download Button:** "Download CSV"
- **Filename:** `{basename}_cell_metrics_{n_cells}_cells_{date}.csv`

#### 7.1.2 Tab 2: Summary Statistics
- **Icon:** chart-bar
- **Title:** "Summary Statistics by Group"
- **Table:** `DT::DTOutput("summary_stats_table")`
- **Data Processing:**
  - Pivots metrics to long format
  - Groups by Group and Metric
  - Calculates: Mean, SD, N, SEM
  - Pivots back to wide format
  - Column naming: `{Metric}_Mean`, `{Metric}_SEM`, `{Metric}_N`
- **Features:**
  - **Page Length:** 10 rows
  - **Scroll:** Horizontal enabled
  - **Buttons:** Copy, CSV, Excel
  - **Styling:** Compact, striped, hover
  - **Numeric Formatting:** 4 decimal places
- **Download Button:** "Download CSV"
- **Filename:** `{basename}_summary_statistics_{n_groups}_groups_{date}.csv`

#### 7.1.3 Tab 3: Time Course Summary
- **Icon:** clock
- **Title:** "Time Course Summary (Mean ± SEM)"
- **Table:** `DT::DTOutput("timecourse_summary_table")`
- **Data Source:** `rv$summary`
- **Processing:**
  - Creates "Mean ± SEM" column: `paste0(round(Mean, 4), " ± ", round(SEM, 4))`
  - Pivots wide: Groups as columns, Time as rows
- **Features:**
  - **Page Length:** 25 rows
  - **Scroll:** Horizontal enabled
  - **Buttons:** Copy, CSV, Excel
- **Download Button:** "Download CSV"
- **Filename:** `{basename}_timecourse_summary_{date}.csv`

#### 7.1.4 Tab 4: Processed Data
- **Icon:** database
- **Title:** "Processed Data (Wide Format)"
- **Table:** `DT::DTOutput("raw_data_table")`
- **Data Source:** `rv$dts[[selected_group]]`
- **Group Selector:** `selectInput` dropdown (updates when data loaded)
- **Features:**
  - **Page Length:** 25 rows
  - **Scroll:** Horizontal enabled
  - **Buttons:** Copy, CSV, Excel
  - **Numeric Formatting:** 4 decimal places
- **Download Button:** "Download CSV"
- **Filename:** `{basename}_processed_{group_name}_{date}.csv`

---

## 8. EXPORT MODULE

### 8.1 Module Layout
Three-column layout:

#### 8.1.2 Column 1: Export Settings (Width: 4)
- **Title:** "Export Settings"
- **Controls:**
  1. **Format:** Radio buttons (PNG default, PDF, TIFF, SVG)
  2. **Width (in):** Numeric input (4-30, default 12, step 0.5)
  3. **Height (in):** Numeric input (4-30, default 8, step 0.5)
  4. **DPI:** Numeric input (72-600, default 300, step 10)
  5. **TIFF Compression:** Conditional (only if TIFF selected)
     - Options: LZW (default), Zip, None

#### 8.1.3 Column 2: Data Files (Width: 4)
- **Title:** "Data Files"
- **Description:** "Download tabular data for analysis in other software."
- **Buttons:**
  1. **Download All Metrics (CSV)**
     - **Class:** btn-primary btn-block
     - **Filename:** `{basename}_all_metrics_{n_cells}_cells_{date}.csv`
     - **Content:** Full `rv$metrics` table
  
  2. **Download Summary Stats (CSV)**
     - **Class:** btn-default btn-block
     - **Filename:** `{basename}_timecourse_summary_{date}.csv`
     - **Content:** `rv$summary` table
  
  3. **Download Dataset (CSV)** (in gray box)
     - **Group Selector:** Dropdown to choose group
     - **Filename:** `{basename}_processed_{group_name}_{date}.csv`
     - **Content:** Selected group's processed data

#### 8.1.4 Column 3: Figure Downloads (Width: 4)
- **Title:** "Figure Downloads"
- **Description:** "Download high-resolution plots using the settings on the left."
- **Buttons:**
  1. **Time Course Plot**
     - **Class:** btn-primary btn-block
     - **Source:** `time_course_plot_reactive()` from Time Course module
     - **Filename:** `{basename}_timecourse_plot_{date}.{ext}`
  
  2. **Heatmap Plot**
     - **Class:** btn-primary btn-block
     - **Source:** `heatmap_plot_reactive()` from Heatmap module
     - **Filename:** `{basename}_heatmap_{date}.{ext}`
  
  3. **Current Metrics Plot**
     - **Class:** btn-primary btn-block
     - **Source:** `metrics_plot_reactive()` from Metrics module
     - **Filename:** `{basename}_metrics_plot_{date}.{ext}`

- **Info Alert:** "Note: Plot appearance is determined by the settings in each respective tab."

### 8.2 Empty State
- **Condition:** If `rv$dts` is NULL or empty
- **Display:**
  - Large upload icon (cloud-upload-alt, fa-3x)
  - Heading: "No Data Available"
  - Text: "Please load and process data in the 'Load Data' tab to enable export options."

---

## 9. PROCESSED DATA MODULE

### 9.1 Average Metrics Table
- **Title:** "Average Metrics (All Cells)"
- **Icon:** table
- **Status:** info (blue)
- **Table Type:** DT (DataTables)
- **Data Processing:**
  - Calculates Mean, SEM, n for each metric
  - Formats metric names (e.g., "Peak ΔF/F₀")
  - Rounds to 4 decimal places
- **Table Features:**
  - **Buttons:** Copy, CSV (custom filename), Excel (custom filename)
  - **Page Length:** 15 rows
  - **Search:** Enabled ("Search metrics:")
  - **Styling:** Compact, striped, hover
- **Export Table Image:**
  - **Format:** PNG (default), PDF, TIFF
  - **Width/Height:** Numeric inputs (2-20 inches, step 0.5)
  - **Method:** Converts gt table to HTML, then webshot2 to image
  - **Filename:** `{basename}_average_metrics_{n_groups}_groups_{date}.{ext}`

### 9.2 Download Processed Data Box
- **Title:** "Download Processed Data"
- **Icon:** file-download
- **Status:** primary (blue)
- **Group Selector:** Dropdown (updates when data loaded)
- **Download Button:** "Download CSV"
- **Filename:** `{basename}_processed_{group_name}_{date}.csv`

---

## 10. HELP MODULE

### 10.1 Layout
- **Width:** 8 columns, centered (offset: 2)
- **Header:** Centered title "SimpleCa²⁺" with subtitle

### 10.2 Quick Start Guide Box (Expanded)
- **Title:** "Quick Start Guide"
- **Icon:** rocket
- **Status:** primary
- **Content:** Numbered list (6 steps):
  1. Load Data: Upload wide-format files
  2. Process: Choose baseline method, click Process
  3. Visualize: Check Time Course and Heatmap
  4. Analyze: Use Metrics tab
  5. Explain: Use Metric Explanations tab
  6. Export: Download figures and tables

### 10.3 Input Data Format Box (Collapsible)
- **Title:** "Input Data Format"
- **Icon:** file-csv
- **Status:** info
- **Content:**
  - Example table showing Time, Cell1, Cell2 columns
  - Alert box: "The first column must be time (seconds). If time is missing, use the 'Sampling Rate' override in Load Data."

### 10.4 Metric Definitions Box (Collapsed by Default)
- **Title:** "Metric Definitions"
- **Icon:** book
- **Status:** success (green)
- **Content:** Definition list (`<dl>`) with 6 metrics:
  - Peak ΔF/F₀
  - AUC
  - Time to Peak
  - Rise Time (10-90%)
  - FWHM
  - SNR

---

## 11. UI COMPONENTS & DESIGN SYSTEM

### 11.1 Accordion Component (`R/components.R`)
- **Function:** `accordion(id, title, content, expanded=FALSE, icon=NULL)`
- **Features:**
  - Clickable header with icon and chevron
  - Smooth expand/collapse animation
  - Chevron rotates 180° when expanded
  - Custom CSS classes: `.accordion-section`, `.accordion-header`, `.accordion-body`
  - JavaScript: `toggleAccordion(id)` function
  - Overflow handling: Prevents dropdown clipping

### 11.2 Theme Box Component
- **Function:** `theme_box(title, ..., status="primary", solidHeader=TRUE, width=12, collapsible=FALSE, collapsed=FALSE)`
- **Wrapper:** Around `shinydashboard::box()`
- **Default Styling:** Blue header, solid background
- **CSS:** Custom styling via `R/theme.R`

### 11.3 Stat Card Component
- **Function:** `stat_card(value, label, width=12, style="")`
- **Features:**
  - Large number (40px, bold, blue)
  - Uppercase label (13px, gray)
  - Centered alignment
  - Shadow and border styling

### 11.4 Status Step Component
- **Function:** `status_step(icon_name, title, status_text, color="var(--color-primary-blue)")`
- **Features:**
  - Large icon (fa-2x)
  - Title text (14px, bold)
  - Status text (12px, gray)
  - Centered alignment

### 11.5 Primary Button Component
- **Function:** `primary_button(inputId, label, icon=NULL, width=NULL, ...)`
- **Features:**
  - Applies `btn-primary` class
  - Optional width styling
  - Passes through all actionButton arguments

### 11.6 Design Tokens (CSS Variables)
Defined in `R/theme.R`:
- **Colors:**
  - Primary Blue: `#3c8dbc`
  - Success Green: `#00a65a`
  - Warning Orange: `#f39c12`
  - Info Blue: `#5bc0de`
  - Gray scale: 50, 100, 200, 300, 400, 500, 600, 700, 800, 900
- **Typography:**
  - Font families: Sans-serif, Serif, Monospace
  - Font sizes: 12px base, scale up/down
- **Spacing:**
  - Radius: Small (4px), Medium (6px), Large (8px)
  - Shadows: Level 1, 2, 3

### 11.7 Filename Sanitization
- **Function:** `sanitize_filename_component(x, fallback=NULL)`
- **Rules:**
  - Removes all non-alphanumeric except `_` and `-`
  - Collapses multiple underscores
  - Trims leading/trailing underscores
  - Handles NULL/NA/empty strings

### 11.8 Export Filename Builder
- **Function:** `build_export_filename(rv, parts=character(), ext="csv", include_date=TRUE)`
- **Components:**
  1. Base name (from first uploaded file)
  2. Additional parts (sanitized)
  3. Date (YYYY-MM-DD format)
  4. Extension
- **Format:** `{base}_{part1}_{part2}_{date}.{ext}`

---

## 12. FUTURE FEATURES: GROUP ANALYSIS

### 12.1 Planned Statistical Comparisons

#### 12.1.1 Between-Group Comparisons
**Feature:** Statistical tests comparing metrics across experimental groups

**Planned Tests:**
- **t-test:** Independent samples t-test for each metric
- **ANOVA:** One-way ANOVA for multiple groups
- **Post-hoc Tests:** Tukey HSD, Bonferroni correction
- **Non-parametric:** Mann-Whitney U, Kruskal-Wallis

**UI Components:**
- **Group Selection:** Multi-select dropdown to choose groups for comparison
- **Metric Selection:** Dropdown to select metric to test
- **Test Selection:** Radio buttons (t-test, ANOVA, etc.)
- **Results Table:** Shows test statistic, p-value, effect size, confidence intervals
- **Visualization:** Box plots with significance brackets

**Output:**
- Summary table of all comparisons
- Export as CSV
- Publication-ready figure with significance markers

#### 12.1.2 Effect Size Calculations
**Metrics:**
- **Cohen's d:** For t-tests
- **Eta-squared (η²):** For ANOVA
- **Cramér's V:** For categorical comparisons
- **95% Confidence Intervals:** For all effect sizes

**Display:**
- Effect size interpretation guide (small, medium, large)
- Confidence interval visualization

### 12.2 Planned Visualization Enhancements

#### 12.2.1 Group Comparison Plots
**Feature:** Side-by-side comparison visualizations

**Plot Types:**
1. **Grouped Bar Chart**
   - X-axis: Groups
   - Y-axis: Metric value
   - Error bars: SEM or SD
   - Significance brackets above bars

2. **Violin + Box Plot**
   - Violin shows distribution
   - Box plot shows quartiles
   - Points overlay individual cells
   - Color-coded by group

3. **Raincloud Plot**
   - Half-violin (distribution)
   - Box plot (quartiles)
   - Scatter points (individual cells)
   - All aligned horizontally

**Controls:**
- Group selection (multi-select)
- Metric selection
- Error bar type (SEM, SD, 95% CI)
- Significance level (0.05, 0.01, 0.001)
- Color scheme (auto or custom)

#### 12.2.2 Correlation Analysis
**Feature:** Analyze relationships between metrics

**Visualizations:**
- **Scatter Plot Matrix:** All metrics vs all metrics
- **Correlation Heatmap:** Color-coded correlation coefficients
- **Pairwise Scatter:** Select two metrics to compare

**Statistics:**
- Pearson correlation coefficient
- Spearman rank correlation
- P-values and confidence intervals
- Regression line overlay option

### 12.3 Planned Data Management Features

#### 12.3.1 Session Save/Load
**Feature:** Save entire analysis session

**Save Format:** RDS file containing:
- All uploaded files metadata
- Processing parameters
- Calculated metrics
- Plot settings
- Current selections

**Load Features:**
- Restore all data
- Restore all plot settings
- Resume analysis from saved point

#### 12.3.2 Batch Processing
**Feature:** Process multiple analysis sessions

**Workflow:**
1. Load multiple datasets
2. Process each with same parameters
3. Compare results across sessions
4. Generate batch report

**Output:**
- Summary table across all sessions
- Combined visualizations
- Batch export of all figures

### 12.4 Planned Advanced Analysis

#### 12.4.1 Response Classification
**Feature:** Automatically classify cells by response type

**Categories:**
- **Responder:** Peak > threshold
- **Non-responder:** Peak < threshold
- **Sustained:** Signal doesn't return to baseline
- **Oscillatory:** Multiple peaks detected
- **Delayed:** Time to peak > threshold

**Visualization:**
- Pie chart of response types
- Bar chart by group
- Heatmap colored by response type

#### 12.4.2 Peak Detection Refinement
**Feature:** Advanced peak detection algorithms

**Methods:**
- **Threshold-based:** User-defined threshold
- **Statistical:** Peaks > mean + N×SD
- **Derivative-based:** Find inflection points
- **Wavelet:** Multi-scale peak detection

**Controls:**
- Method selection
- Parameter tuning (threshold, SD multiplier, etc.)
- Preview detected peaks on plot
- Manual peak adjustment

#### 12.4.3 Temporal Pattern Analysis
**Feature:** Analyze temporal dynamics

**Metrics:**
- **Oscillation Frequency:** If multiple peaks detected
- **Decay Rate:** Exponential fit to decay phase
- **Response Onset:** More precise than time to 10%
- **Response Duration:** Time above baseline threshold

**Visualizations:**
- Phase plots (amplitude vs. time derivative)
- Frequency domain analysis (FFT)
- Autocorrelation plots

### 12.5 Planned Export Enhancements

#### 12.5.1 Batch Export
**Feature:** Export all figures at once

**Options:**
- Select which plots to export
- Apply same settings to all
- Generate zip file with all exports
- Include metadata file (JSON/CSV)

#### 12.5.2 Report Generation
**Feature:** Generate PDF report

**Sections:**
1. **Methods:** Processing parameters
2. **Results:** Key metrics summary
3. **Figures:** All plots embedded
4. **Tables:** Summary statistics
5. **Appendix:** Full data tables

**Customization:**
- Title page
- Author information
- Custom sections
- Logo insertion

### 12.6 Planned User Experience Enhancements

#### 12.6.1 Undo/Redo
**Feature:** Undo processing steps

**Implementation:**
- Store state history
- Allow reverting to previous processing
- Redo after undo

#### 12.6.2 Keyboard Shortcuts
**Planned Shortcuts:**
- `Ctrl+P`: Process data
- `Ctrl+S`: Save session
- `Ctrl+E`: Export current plot
- `Ctrl+H`: Toggle help
- Arrow keys: Navigate tabs

#### 12.6.3 Tutorial Mode
**Feature:** Interactive guided tour

**Components:**
- Step-by-step instructions
- Highlight relevant controls
- Auto-advance after actions
- Skip option

### 12.7 Planned Data Integration

#### 12.7.1 Database Connection
**Feature:** Connect to external databases

**Sources:**
- SQL databases
- REDCap
- Lab information systems

**Features:**
- Query builder UI
- Automatic data formatting
- Batch import

#### 12.7.2 API Integration
**Feature:** Connect to analysis APIs

**Use Cases:**
- Submit data for cloud processing
- Retrieve pre-processed data
- Share results with collaborators

### 12.8 Implementation Priority

**Phase 1 (High Priority):**
1. Between-group statistical comparisons (t-test, ANOVA)
2. Group comparison visualizations with significance markers
3. Batch export functionality

**Phase 2 (Medium Priority):**
4. Session save/load
5. Effect size calculations
6. Correlation analysis

**Phase 3 (Lower Priority):**
7. Response classification
8. Advanced peak detection
9. Report generation
10. Tutorial mode

---

## APPENDIX: COMPLETE FEATURE CHECKLIST

### Data Input
- [x] Multi-file upload (CSV, Excel)
- [x] File format validation
- [x] Time column auto-generation
- [x] Three baseline correction methods
- [x] Sampling rate override
- [x] Progress indicator
- [x] Real-time statistics display
- [x] Processing status pipeline

### Visualization
- [x] Time course plots (static + interactive)
- [x] Individual trace overlay
- [x] SEM ribbon display
- [x] Customizable colors
- [x] Multiple themes
- [x] Custom axis limits
- [x] Custom axis breaks
- [x] Log scale option
- [x] Grid lines toggle
- [x] Typography controls
- [x] Legend positioning
- [x] Heatmap visualization
- [x] Cell sorting options
- [x] Color palette selection
- [x] Scale step control

### Metrics
- [x] 11 calculated metrics
- [x] Three plot styles (bar, box+swarm, violin)
- [x] Cell highlighting (top/bottom K)
- [x] Summary statistics overlay
- [x] Custom titles and labels
- [x] Font controls
- [x] Interactive mode (plotly)

### Educational
- [x] 11 metric explanations
- [x] Cell-specific calculations
- [x] Visual annotations
- [x] Mathematical formulas
- [x] Step-by-step derivations

### Data Tables
- [x] Cell metrics table
- [x] Summary statistics table
- [x] Time course summary table
- [x] Processed data table
- [x] Search and filter
- [x] Sortable columns
- [x] Copy/CSV/Excel export

### Export
- [x] Multiple formats (PNG, PDF, SVG, TIFF)
- [x] Customizable dimensions
- [x] DPI control
- [x] Size presets
- [x] Centralized export hub
- [x] Filename sanitization
- [x] Date-stamped filenames

### UI/UX
- [x] Accordion sidebar controls
- [x] Consistent theme system
- [x] Responsive layout
- [x] Loading spinners
- [x] Empty state messages
- [x] Error handling
- [x] Help documentation
- [x] Quick start guide

### Future Features (Planned)
- [ ] Statistical comparisons
- [ ] Effect size calculations
- [ ] Group comparison plots
- [ ] Correlation analysis
- [ ] Session save/load
- [ ] Batch processing
- [ ] Response classification
- [ ] Advanced peak detection
- [ ] Report generation
- [ ] Tutorial mode

---

**END OF SPECIFICATION**

This document contains every feature, control, calculation, and UI element in SimpleCa²⁺ v1.1.0, plus planned features for future group analysis capabilities.

