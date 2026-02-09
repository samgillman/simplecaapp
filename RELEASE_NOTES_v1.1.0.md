# SimpleCa v1.1.0 Release Notes

## Major Enhancements

### Interactive Baseline Adjustment
You can now adjust baseline frames directly in the Time Course and Heatmap tabs without reloading your data! Simply:
1. Open the Graph Settings (Time Course) or Controls (Heatmap)
2. Adjust the baseline frame range slider
3. Click "Apply Baseline Change" to instantly recalculate all metrics

### Smarter Peak Detection
The algorithm now intelligently excludes peaks that occur during the baseline period, preventing false positives from baseline fluctuations being interpreted as calcium responses.

### Visual Metric Explanations
New "Metrics Explained" tab provides interactive visualizations showing exactly how each metric is calculated using your actual data, making it easier to understand and validate your results.

## Bug Fixes

### Fixed: Incomplete Metrics Calculation
- **Problem**: When analyzing data with peaks in the baseline period, only 1 out of multiple cells would have complete metrics (FWHM, Rise Time, etc.)
- **Solution**: Implemented proper peak detection that excludes baseline frames and correctly handles edge cases

### Fixed: Double Normalization Issue
- **Problem**: Already-processed ΔF/F₀ data was being normalized again, leading to incorrect values
- **Solution**: Added proper `data_is_dFF0` flag propagation to prevent double normalization

## Technical Improvements

- Enhanced robustness of `calculate_cell_metrics()` function
- Improved handling of edge cases (very small baselines, negative values)
- Better threshold crossing detection for kinetic metrics
- Synchronized baseline controls across all visualization tabs

## Documentation Updates

- Added CHANGELOG.md for version tracking
- Updated README with new features
- Enhanced inline documentation for metric calculations

## Installation

### For New Users:
```r
# Clone the repository
git clone https://github.com/yourusername/simpleca.git
cd simpleca

# Install dependencies
install.packages("renv")
renv::restore()

# Run the app
shiny::runApp()
```

### For Existing Users:
```bash
# Pull latest changes
git pull origin main

# Restore any new dependencies
renv::restore()
```

## Usage Tips

### Working with Inverted Responses
If your calcium indicator shows decreasing fluorescence:
- The current version treats these as "no response"
- Consider inverting your data before analysis
- Future versions will add native support for bidirectional responses

### Optimizing Baseline Selection
- Ensure baseline frames capture true resting state
- Avoid including stimulus application in baseline
- Use the new baseline adjustment feature to fine-tune without reprocessing

## Future Development

Planned features for future releases:
- Support for bidirectional calcium responses
- Automated spike detection and counting
- Statistical comparisons between groups
- Custom metric definitions
- Batch export functionality

## Acknowledgments

Thank you to all users who provided feedback and bug reports. Special thanks to those who identified the metrics calculation issue that led to these improvements.

## Support

For issues or questions:
- Open an issue on GitHub
- Check the built-in Help tab
- See PROTOCOL.md for detailed usage instructions

---

**Download:** [SimpleCa-v1.1.0.zip](https://github.com/yourusername/simpleca/releases/download/v1.1.0/SimpleCa-v1.1.0.zip)