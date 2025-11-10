# SimpleCa²⁺ UI/UX Redesign - Design Document

**Date:** 2025-11-02
**Status:** Approved for Implementation
**Approach:** Progressive Enhancement

## Executive Summary

Comprehensive UI/UX overhaul of SimpleCa²⁺ to achieve:
- **Visual Consistency**: Unified theme system with modern scientific aesthetic
- **Enhanced Interactivity**: Rich plotly features with zoom, pan, hover, and dynamic filtering
- **Better Organization**: Accordion-based settings for improved discoverability
- **Professional Polish**: Consistent components, smooth animations, accessibility

**Implementation Strategy:** Progressive Enhancement - establish foundation (theme) that immediately improves entire app, then layer enhancements module-by-module.

**Timeline:** 3-4 weeks

---

## 1. Theme System & Design Tokens

### 1.1 Color Palette (Modern Scientific)

**Primary Colors:**
```r
primary_blue      <- "#0072B2"  # Brand accent, buttons, active states
primary_light     <- "#E8F4F8"  # Subtle backgrounds, hover states
primary_dark      <- "#004D7A"  # Headers, emphasis
```

**Neutral Grays:**
```r
white             <- "#FFFFFF"  # Main background
gray_50           <- "#F8F9FA"  # Subtle backgrounds, alternating rows
gray_100          <- "#E9ECEF"  # Borders, dividers
gray_600          <- "#6C757D"  # Secondary text
gray_900          <- "#212529"  # Primary text
```

**Semantic Colors:**
```r
success           <- "#28A745"  # Validation, positive feedback
warning           <- "#FFC107"  # Cautions, processing states
danger            <- "#DC3545"  # Errors, critical warnings
info              <- "#17A2B8"  # Helpful hints, tooltips
```

**Data Visualization:**
- Retain existing palettes (plasma, viridis, magma, inferno, cividis) for heatmaps
- Add curated 2-6 color palettes for multi-group comparisons
- Ensure WCAG AA contrast compliance (4.5:1 for text, 3:1 for UI components)

### 1.2 Typography System

**Font Stack:**
```css
font_primary: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
              "Helvetica Neue", Arial, sans-serif
font_mono:    "SF Mono", Monaco, "Cascadia Code", "Courier New", monospace
```

**Type Scale:**
- **Hero (24px)**: Page titles, main headers
- **H1 (20px)**: Section headers
- **H2 (16px)**: Subsection headers
- **Body (14px)**: Standard text, form controls
- **Small (12px)**: Help text, captions
- **Tiny (11px)**: Axis labels, dense tables

**Font Weights:**
- **Regular (400)**: Body text, standard controls
- **Medium (500)**: Emphasis, active states
- **Semibold (600)**: Headers, buttons

### 1.3 Spacing System

**4px base unit:**
```r
spacing_xs  <- "4px"
spacing_sm  <- "8px"
spacing_md  <- "16px"
spacing_lg  <- "24px"
spacing_xl  <- "32px"
spacing_2xl <- "48px"
```

**Application:**
- Form label to input: 4px
- Between form controls: 16px
- Section spacing within box: 16-24px
- Between boxes: 16px
- Page margins: 24-32px

### 1.4 Elevation & Shadows

```css
shadow_level_0: none                                    /* Flush elements */
shadow_level_1: 0 1px 3px rgba(0,0,0,0.12)             /* Cards, boxes */
shadow_level_2: 0 4px 6px rgba(0,0,0,0.1)              /* Dropdowns, popovers */
shadow_level_3: 0 10px 20px rgba(0,0,0,0.15)           /* Modals, overlays */
```

### 1.5 Border Radius

```r
radius_sm  <- "3px"   # Buttons, inputs
radius_md  <- "6px"   # Cards, boxes
radius_lg  <- "8px"   # Modals
```

---

## 2. Consistent Component Library

### 2.1 Boxes & Containers

**Standard Box:**
```r
theme_box <- function(title, ..., status = "primary") {
  box(
    title = title,
    status = status,
    solidHeader = TRUE,
    width = 12,
    style = "
      background: white;
      border: 1px solid #E9ECEF;
      border-radius: 6px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.12);
      padding: 16px;
    ",
    ...
  )
}
```

**Collapsible Box:**
- Same styling as standard box
- Header clickable with chevron icon (▸/▾)
- 200ms smooth collapse animation
- State persists within session

**Accordion Panel:**
```r
accordion_section <- function(id, title, content, expanded = FALSE) {
  # Gray header (gray_50) with chevron
  # White content area with smooth height transition
  # Can have multiple sections open simultaneously
  # 3px left border in primary_light when expanded
}
```

### 2.2 Buttons

**Primary Button:**
```css
background: #0072B2
color: white
font-weight: 500
padding: 8px 16px
border-radius: 3px
hover: #004D7A
active: slightly darker + subtle inset shadow
disabled: #E9ECEF background, #6C757D text
```

**Secondary Button:**
```css
background: white
border: 1px solid #0072B2
color: #0072B2
font-weight: 500
hover: #E8F4F8 background
```

**Icon Button:**
```css
size: 32px × 32px
icon: centered
hover: #F8F9FA background
active: clear visual feedback
```

### 2.3 Form Controls

**Text Input:**
```css
border: 1px solid #E9ECEF
border-radius: 3px
padding: 6px 12px
focus: #0072B2 border + subtle blue glow
error: #DC3545 border + error text below
```

**Select Dropdown:**
```css
same styling as text input
chevron icon on right
dropdown has level 2 shadow
hover states on options
```

**Slider:**
```css
track: #E9ECEF, 4px height
filled_track: #0072B2
thumb: 16px circle, #0072B2, level 1 shadow
hover/active: slightly larger, level 2 shadow
```

**Checkbox/Radio:**
```css
size: 18px square/circle
border: #E9ECEF
checked: #0072B2 background, white checkmark
focus: blue outline
```

### 2.4 Interactive Elements

**Tabs:**
```css
horizontal bar
active: #004D7A text, 3px bottom border in #0072B2
inactive: #6C757D text, no border
hover: #F8F9FA background
```

**Tooltips:**
```css
background: rgba(33, 37, 41, 0.9)
color: white
font-size: 12px
border-radius: 3px
appears after 300ms hover delay
shadow: level 2
```

**Loading Spinners:**
```css
color: #0072B2
sizes: small (20px), medium (32px), large (48px)
smooth animation
optional "Loading..." text below
```

### 2.5 Spacing Guidelines

**Within boxes:**
- Title to content: 16px
- Between sections: 24px
- Form label to input: 4px
- Between form rows: 16px

**Between boxes:**
- Vertical: 16px
- Grid layouts: 16px gap

---

## 3. Enhanced Graph Interactivity

### 3.1 Core Interactive Features

**Zoom & Pan (All Plot Types):**
- Box zoom: Click-drag to select region → zooms to that area
- Double-click reset: Returns to original view
- Scroll wheel zoom: Zoom in/out centered on cursor
- Pan mode toggle: Drag to pan when enabled
- Reset button: Always visible
- Zoom history: Back/forward buttons

**Hover Details:**

*Time Course:*
```
Shows:
  - Time point (s)
  - ΔF/F₀ value
  - Group name
  - n = X cells
Vertical crosshair follows cursor
Tooltip positioned to avoid data obscuring
```

*Heatmaps:*
```
Shows:
  - Cell ID
  - Time point
  - ΔF/F₀ value
Highlights entire row (cell across time)
```

*Metrics:*
```
Shows:
  - Cell ID
  - Metric name & value
  - Group name
Highlight point with glow effect
```

**Dynamic Filtering:**

*Legend Interactions:*
```
Click group name → Toggle visibility
Double-click → Show only that group
Groups fade to 30% opacity (don't disappear)
Re-click to restore
"Show All" button to reset
```

*Cell Selection:*
```
Click cells/points to select
Shift-click for multiple selection
Selected items highlighted in all linked plots
"Clear selection" button
Export selected subset option
```

**Live Parameter Updates (Smart Hybrid):**

*Real-time (instant):*
- Color changes (palette, individual colors)
- Text edits (titles, labels)
- Font size adjustments
- Line width/point size
- Opacity/transparency
- Show/hide elements (grid, legend)

*Debounced (0.5s delay):*
- Sorting algorithms (heatmap)
- Baseline window changes
- Smoothing/filtering parameters
- Binning/aggregation
- Scale transformations

*Visual feedback during debounce:*
- Subtle loading indicator on plot
- "Updating..." badge in settings panel
- Previous plot remains visible until ready

### 3.2 Plot-Specific Enhancements

**Time Course:**
- Region selection: Click-drag on x-axis to highlight time window
- Toggle individual cells: Click legend to hide/show specific cells
- Baseline region indicator: Visual overlay showing F₀ window
- SEM ribbon opacity slider: Adjust without re-rendering

**Heatmap:**
- Row reordering: Drag-and-drop cells to custom order
- Color scale adjustment: Interactive histogram, adjust min/max by dragging
- Quick sort buttons: One-click sort without opening settings
- Cell grouping: Draw boxes around groups for annotation

**Metrics:**
- Metric comparison: Select 2 metrics → automatic scatter plot with correlation
- Outlier highlighting: Auto-detect (>2 SD), click to identify
- Distribution overlays: Toggle histogram/violin behind points
- Quick stats: Click group → shows mean ± SEM

### 3.3 Export & Accessibility

**Export with Current View State:**
- "Export Current View" button
- Saves exactly as displayed (zoom, hidden groups, selections)
- Separate from "Export Full Dataset"
- View state metadata in filename

**Accessibility:**
- Keyboard navigation (arrow keys pan, +/- zoom)
- Screen reader announcements
- High contrast mode toggle
- Colorblind-safe palette options
- Focus indicators on all interactive elements

---

## 4. Settings Organization (Accordion System)

### 4.1 Structure Pattern (All Modules)

```
┌─────────────────────────────────────────┐
│  📊 Plot Type Toggle    [Static|Interactive]  │
├─────────────────────────────────────────┤
│                                         │
│  ▸ Data & Filtering                    │
│  ▾ Style & Appearance      ⭐          │
│    ├─ Colors                           │
│    ├─ Line/Point Styles                │
│    └─ Fonts & Sizes                    │
│  ▸ Labels & Annotations                │
│  ▸ Export Options                      │
│                                         │
│  [Plot - 70% of vertical space]        │
│                                         │
│  Quick Export: [PNG ▾] [Download]      │
└─────────────────────────────────────────┘
```

### 4.2 Accordion Sections (Standardized)

**1. Data & Filtering** (Collapsed by default)
- Group selection (multi-select)
- Time window selection
- Cell filtering options
- Baseline parameters
- Calculation method toggles

**2. Style & Appearance** ⭐ (Expanded by default)
- Color palettes & pickers
- Line width / point size
- Opacity controls
- Grid & axes styling
- Theme presets

**3. Labels & Annotations** (Collapsed by default)
- Plot title & subtitle
- Axis labels
- Legend position & format
- Font family
- Title alignment
- Statistical annotations

**4. Advanced** (Collapsed by default)
- Custom transformations
- Smoothing/filtering
- Statistical overlays
- Debug/diagnostic options

**5. Export Options** (Collapsed by default)
- Format (PNG/PDF/TIFF/SVG)
- Dimensions & DPI
- Size presets
- Current view vs full data
- Batch export settings

### 4.3 Visual Design

**Accordion Header:**
```css
background: #F8F9FA
border: 1px solid #E9ECEF
padding: 12px 16px
font-size: 14px
font-weight: 500
chevron: #0072B2, rotates 90° on expand
hover: slight darken, pointer cursor
```

**Accordion Content:**
```css
background: white
border-left: 3px solid #E8F4F8
padding: 16px
animation: 200ms ease-out height transition
```

### 4.4 Smart Defaults & State

**Persistence:**
- Accordion states persist in session
- Last-used settings saved per module
- "Reset to Defaults" in each section
- "Save as Preset" for styles

**Contextual Visibility:**
- No groups loaded → Hide "Group Selection"
- Only 1 time point → Hide "Time Window"
- Interactive selected → Show "Interactivity Settings"
- Static selected → Hide interactive-specific options

**Keyboard Navigation:**
- Tab → Navigate sections
- Space/Enter → Expand/collapse
- Arrow Up/Down → Move between sections
- Esc → Collapse all
- Ctrl+E → Jump to Export

---

## 5. Implementation Roadmap

### Phase 1: Foundation (Week 1)

**Goal:** Establish unified theme system

**Days 1-2: Create Theme System**
- Create `R/theme.R` with all design tokens
- CSS custom properties
- Helper functions for consistent styling
- Test theme loads without breaking existing UI

**Days 2-3: Apply Global Styles**
- Update `app.R` with unified CSS
- Standardize all boxes with theme colors
- Apply consistent spacing/padding
- Update button styles across all modules
- Fix typography inconsistencies

**Day 3: Verify No Regressions**
- Manual testing all modules
- Verify plots render
- Check export functionality
- Confirm no broken layouts

**Deliverable:** App looks cleaner, more consistent - all modules benefit immediately

---

### Phase 2: Component Library (Week 2)

**Goal:** Build reusable components, retrofit one module as proof-of-concept

**Days 4-5: Build Core Components**
- Create `R/components.R`
- Standard box/accordion components
- Consistent button variants
- Form control wrappers
- Export controls component

**Days 6-7: Retrofit Time Course Module**
- Replace hardcoded UI with components
- Implement accordion sections
- Add plot type toggle
- Test thoroughly

**Day 7: Document Component Usage**
- Code examples for each component
- Style guide for developers
- Screenshots of components

**Deliverable:** Time Course module fully redesigned, components ready for reuse

---

### Phase 3: Enhanced Interactivity (Week 2-3)

**Goal:** Add interactive features progressively

**Days 8-10: Plotly Integration Enhancement**

*Time Course:*
- Zoom/pan with box select
- Hover tooltips with full details
- Legend click to toggle groups
- Reset view button

*Heatmap:*
- Hover to highlight cell + row
- Click to select cells
- Interactive color scale adjustment

*Metrics:*
- Hover for cell details
- Click for selection
- Outlier highlighting

**Days 10-11: Smart Reactivity System**
- Implement hybrid update system
- Real-time: colors, labels, fonts
- Debounced: data transforms, sorting
- Visual feedback during updates
- Performance testing with large datasets

**Days 11-12: Cross-Plot Linking**
- Select cell in heatmap → highlights in metrics
- Click group in time course → filters metrics
- Shared selection state across tabs

**Deliverable:** All plots highly interactive, smooth performance

---

### Phase 4: Remaining Modules (Week 3)

**Goal:** Apply component library + accordion system to all modules

**Days 13-14: Heatmap Module**
- Retrofit with components
- Implement accordion settings
- Add enhanced interactivity
- Test with various datasets

**Day 15: Metrics Module**
- Apply component library
- Organize settings in accordions
- Integrate interactivity features

**Days 16-17: Supporting Modules**
- Load Data: Cleaner file upload UI
- Processed Data: Better table styling
- Tables: Enhanced DT styling
- Export: Unified export component
- Help: Better formatting with new typography

**Deliverable:** All modules consistent, polished, interactive

---

### Phase 5: Polish & Testing (Week 3-4)

**Goal:** Final refinements, accessibility, documentation

**Day 18: Accessibility Audit**
- Keyboard navigation testing
- Screen reader compatibility
- Color contrast verification (WCAG AA)
- Focus indicators
- ARIA labels

**Day 19: Performance Optimization**
- Profile with large datasets
- Optimize reactive expressions
- Lazy load heavy computations
- Test memory usage

**Day 20: User Documentation**
- Update PROTOCOL.md with new features
- Add interactive feature guide
- Quick reference for settings
- Screenshot gallery of new UI

**Day 21: Final Testing**
- Cross-browser (Chrome, Firefox, Safari)
- Various screen sizes
- End-to-end workflow testing
- Export quality verification

**Deliverable:** Production-ready, polished application

---

## 6. Risk Mitigation

### Technical Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Plotly performance with large datasets | High | Test early, implement progressive loading, fallback to static |
| Reactivity complexity | Medium | Keep reactive graph simple, use `isolate()` strategically |
| CSS conflicts | Low | Use specific selectors, test after each phase |
| Browser compatibility | Medium | Test in major browsers each phase, use standard CSS |

### User Impact

| Concern | Mitigation |
|---------|------------|
| Users familiar with old UI | Keep keyboard shortcuts consistent, "What's New" guide |
| Existing workflows | Maintain backward compatibility for saved settings |
| Learning curve | Progressive disclosure (simple default, advanced hidden) |

---

## 7. Success Metrics

### Before/After Comparisons

| Metric | Before | Target |
|--------|--------|--------|
| Time to create publication plot | ~5 min | ~2 min |
| Settings discoverability | Ad-hoc | Organized in logical groups |
| Visual consistency | Varies by module | 100% consistent |
| Interactive features | Basic plotly | Rich interactivity |

### User Feedback Goals

- "Easier to find settings"
- "Looks professional"
- "Interactive features save time"
- "Exports look better"

---

## 8. Future Enhancements (Post-MVP)

**Not included in initial implementation:**
- Dark mode theme
- Custom theme builder
- Saved analysis templates
- Advanced statistical overlays
- Multi-panel figure composition
- Real-time collaborative editing

---

## Appendix A: File Structure

```
SimpleCa²⁺/
├── app.R                     # Updated with theme CSS
├── R/
│   ├── theme.R               # NEW: Design tokens & theme system
│   ├── components.R          # NEW: Reusable UI components
│   ├── mod_time_course.R     # UPDATED: Component-based UI
│   ├── mod_heatmap.R         # UPDATED: Component-based UI
│   ├── mod_metrics.R         # UPDATED: Component-based UI
│   ├── mod_load_data.R       # UPDATED: Refined styling
│   ├── mod_preproc.R         # UPDATED: Refined styling
│   ├── mod_tables.R          # UPDATED: Refined styling
│   ├── mod_export.R          # UPDATED: Unified export component
│   ├── mod_help.R            # UPDATED: Better typography
│   └── utils.R               # UPDATED: Helper functions
├── docs/
│   └── plans/
│       └── 2025-11-02-ui-ux-redesign-design.md  # This document
└── www/
    └── custom.css            # Theme CSS (if separate from app.R)
```

---

## Appendix B: Key R Packages

**Existing (retain):**
- `shiny`, `shinydashboard`: Core framework
- `plotly`: Interactive plots
- `ggplot2`: Static plots
- `DT`: Interactive tables
- `shinyWidgets`: Enhanced inputs
- `colourpicker`: Color selection

**Potentially Add:**
- `bslib`: Advanced theming (already in dependencies)
- `shinyjs`: Enhanced JavaScript interactions (already in dependencies)
- None required - existing packages sufficient

---

**End of Design Document**
