# Fix Accordion Dropdowns and Interactive Plots Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix accordion overflow issues preventing dropdown menus from displaying, and remove problematic interactive plot toggle from Metric Explanations module where ggplot annotations don't convert properly to plotly.

**Architecture:** CSS overflow adjustments to accordion components to allow dropdowns to render outside container bounds, and UI simplification to remove Static/Interactive toggle from Metric Explanations where annotations are essential.

**Tech Stack:** R Shiny, shinydashboard, CSS, ggplot2

---

### Task 1: Fix Accordion Overflow for Dropdown Menus

**Files:**
- Modify: `R/theme.R:502-557` (accordion CSS styles)

**Current Problem:** The accordion containers have `overflow: hidden` which clips dropdown menus (like the cell selector in Metric Explanations), making them impossible to use. The dropdown opens but gets cut off at the accordion boundary.

**Step 1: Remove overflow hidden from accordion-section**

In `R/theme.R`, find the `.accordion-section` style block and remove `overflow: hidden`:

```r
    .accordion-section {{
      margin-bottom: var(--spacing-md);
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-md);
      background: var(--color-white);
      /* Don't use overflow hidden - it clips dropdown menus */
    }}
```

**Step 2: Add border-radius to accordion-header to maintain rounded corners**

Since we removed overflow:hidden from the parent, add border-radius directly to the header:

```r
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
      border-radius: var(--radius-md) var(--radius-md) 0 0;
    }}
```

**Step 3: Change accordion-body overflow to visible when expanded**

Update the `.accordion-body.collapsed` and `.accordion-body.expanded` styles:

```r
    .accordion-body.collapsed {{
      max-height: 0 !important;
      padding: 0 !important;
      overflow: hidden;
    }}

    .accordion-body.expanded {{
      max-height: 2000px;
      padding: var(--spacing-md);
      overflow: visible;
    }}
```

**Step 4: Add z-index rules for selectize dropdowns**

After the `.accordion-content .form-group:last-child` block, add:

```r
    /* Ensure select dropdowns in accordions appear above other content */
    .accordion-content .selectize-dropdown,
    .accordion-content select {{
      z-index: 1000;
    }}

    .accordion-content .form-group {{
      position: relative;
      z-index: auto;
    }}

    /* Shiny selectInput dropdown positioning */
    .selectize-dropdown {{
      z-index: 1050 !important;
    }}
```

**Step 5: Test the changes**

1. Run the Shiny app
2. Navigate to "Metric Explanations" tab
3. Expand the "Cell Selection" accordion
4. Click on the cell selector dropdown
5. Verify that the full dropdown menu is visible and you can scroll through all cells
6. Expected: Dropdown appears fully visible, not clipped

**Step 6: Commit**

```bash
git add R/theme.R
git commit -m "fix: allow accordion dropdowns to display without clipping

- Remove overflow:hidden from accordion-section
- Add overflow:visible to accordion-body.expanded
- Add border-radius to accordion-header to maintain styling
- Add z-index rules for selectize dropdowns
- Fixes cell selector dropdown being cut off in Metric Explanations"
```

---

### Task 2: Remove Interactive Plot Toggle from Metric Explanations

**Files:**
- Modify: `R/mod_metrics_explained.R:257-278` (UI remove toggle)
- Modify: `R/mod_metrics_explained.R:883-922` (Server remove plotly output)

**Current Problem:** The metric explanation plots have complex ggplot annotations (labels, arrows, segments, mathematical expressions) that don't convert properly to plotly. The interactive view loses critical information, making the plots confusing and "sloppy". Since these plots are educational/explanatory in nature, the annotations are essential.

**Step 1: Remove the Static/Interactive toggle from UI**

In `R/mod_metrics_explained.R`, locate the UI section around lines 257-278. Replace the toggle and conditionalPanels with a single static plotOutput:

```r
            # Right Column: The plot itself (static only - annotations don't convert to plotly)
            column(width = 7,
              plotOutput(ns("explanation_plot"), height = "600px")
            )
```

Remove these lines:
- The `fluidRow` with `radioGroupButtons` for plot_type_toggle (lines 257-268)
- Both `conditionalPanel` blocks (lines 269-274)

**Step 2: Remove the plotly output from server**

In `R/mod_metrics_explained.R`, locate and delete the entire `output$explanation_plotly` block (lines 883-922):

Delete from:
```r
    output$explanation_plotly <- plotly::renderPlotly({
```

Through:
```r
    })
```

Keep only the static plot output:
```r
    output$explanation_plot <- renderPlot({
      explanation_plot_obj()
    }, res = 96)
```

**Step 3: Test the changes**

1. Run the Shiny app
2. Navigate to "Metric Explanations" tab
3. Select a metric to explain (e.g., "Calcium Entry Rate")
4. Verify the plot shows with all annotations, labels, and arrows visible
5. Verify there is no Static/Interactive toggle above the plot
6. Expected: Clean plot with all explanatory annotations visible

**Step 4: Verify other modules still have interactive toggles**

1. Check "Heatmap" tab - should have Static/Interactive toggle
2. Check "Metrics" tab - should have Static/Interactive toggle
3. Check "Time Course" tab - should have Static/Interactive toggle
4. Expected: All other visualization modules retain their toggles

**Step 5: Commit**

```bash
git add R/mod_metrics_explained.R
git commit -m "fix: remove interactive plot toggle from Metric Explanations

- Remove Static/Interactive radioGroupButtons from UI
- Remove plotly output handler from server
- Keep only static plot output
- ggplot annotations (labels, arrows, math) don't convert to plotly
- Educational plots require annotations to be meaningful"
```

---

### Task 3: Final Verification and Testing

**Step 1: Test all accordion dropdowns across modules**

Test each module with accordions:
1. **Heatmap** - Display Options, Colors & Style, Labels, etc.
2. **Metrics** - Display Options, Colors & Style, Labels, etc.
3. **Metric Explanations** - Cell Selection accordion
4. **Time Course** - All accordions in left sidebar

For each:
- Expand accordion
- Use any dropdown/select inputs
- Verify dropdown menu is fully visible
- Verify you can select items

**Step 2: Test all plot toggles**

1. **Heatmap** - Toggle between Static and Interactive
2. **Metrics** - Toggle between Static and Interactive
3. **Time Course** - Toggle between Static and Interactive
4. **Metric Explanations** - Should have no toggle, only static plot

Verify:
- Static plots render correctly
- Interactive plots render correctly (except Metric Explanations)
- Toggles work smoothly
- No console errors

**Step 3: Test layout consistency**

Verify all visualization modules use consistent two-column layout:
1. Left sidebar (width=4) with Controls/options
2. Right plot area (width=8)
3. Accordions in left sidebar are always visible
4. Plot toggles (where applicable) in top-right of plot box

**Step 4: Final commit if any fixes needed**

If any issues were found and fixed during testing:

```bash
git add [modified files]
git commit -m "fix: address issues found during final verification

- [list specific fixes made]"
```

**Step 5: Push changes to remote**

```bash
git push origin feature/ui-ux-redesign
```

---

## Summary

This plan addresses two critical UX issues:

1. **Accordion Dropdown Clipping**: Fixed by changing CSS overflow properties to allow dropdowns to render outside accordion boundaries while maintaining visual styling
2. **Metric Explanations Interactive Plot**: Removed because ggplot annotations (essential for educational content) don't convert to plotly, resulting in confusing, incomplete visualizations

After completion, all modules will have:
- ✅ Consistent two-column layouts
- ✅ Working dropdown menus in accordions
- ✅ Interactive plot toggles where appropriate (Heatmap, Metrics, Time Course)
- ✅ Static-only plots where annotations are essential (Metric Explanations)
