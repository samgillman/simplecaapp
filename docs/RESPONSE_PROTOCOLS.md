# Response protocols and reproducibility

## Choosing and applying a rule

Load one recording per field and identify target cells independently of whether
they respond. Declare the input units, normalization baseline and time source.
Open Percent Responding, confirm the actual experiment times, and apply the
analysis settings. A completed run is a snapshot: edits create pending changes,
while its displayed counts, plots and downloads retain the applied settings.
Reprocessing input invalidates that snapshot.

The original sample-based protocol remains available. It requires a local
fractional increase at least as large as both a 10% floor and three baseline SDs
for two consecutive observations by default. Its coverage and bounded endpoint
clipping behavior are preserved. Importing a legacy protocol does not migrate
its scientific semantics automatically.

The slow evoked candidate uses the same initial amplitude/noise settings with
at least one second between the first and last qualifying observations. At 2 Hz,
this requires three observations; at 10 Hz, eleven. It is a provisional starting
rule for empirical validation, not an established optimal cutoff for an
indicator, cell line, or stimulus. Brief real events can fail this duration.

## Local normalization and threshold

The scorer consumes a fixed-reference fractional trace y = (S - S0) / S0,
where S is corrected fluorescence or a declared fluorescence ratio. For a
stimulus-specific prestimulus interval it calculates:

```
b       = mean(y in local baseline)
rise(t) = (y(t) - b) / (1 + b)
noise   = sample_SD(y in local baseline) / (1 + b)
T       = max(amplitude_floor, SD_multiplier * noise)
```

A qualifying observation has rise >= T. A nonpositive or nonfinite local
reference cannot be repaired by taking its absolute value, adding a constant,
or dividing by a replacement epsilon. The comparison is inclusive.

The signal is not smoothed for classification. Display choices do not change
the scorer. Three SD is a detection heuristic, not a p-value or a guaranteed
whole-window false-positive rate. Baseline SD includes biological fluctuation
and drift as well as measurement noise.

Raw and already-corrected fluorescence use the selected initial frame mean.
Already-normalized fixed-reference fractions are preserved; percentages are
divided by 100 once. Raw baseline quality is unavailable for normalized uploads.
Ratio inputs are labeled ΔR/R0; the app does not perform channel background
correction on an already formed ratio. Arbitrary z-scores, moving-baseline
normalizations and F/F0 values are not accepted as ΔF/F0.

## Time and coverage

Version 2 records baseline start/end, stimulus onset and response start/end
independently. Baseline intervals include the left endpoint and exclude the
right endpoint. Response intervals include both endpoints. Baseline end must
not follow stimulus onset. Delaying response start does not move the baseline.
The onset reference should represent application or arrival at the cells as
appropriate to the experiment; record that choice in the experiment metadata.

Seconds mode uses the difference between qualifying timestamps, with at least
two observed samples. It does not infer uninterrupted fluorescence between
widely spaced observations. Missing values and excessive gaps cannot be joined
into a qualifying run. Version 2 requires complete predefined windows before
classification, independently of whether an apparent event is present.

The automatic cadence tolerance is 1.5 times the expected interval when one is
declared, otherwise 1.5 times the median positive observed interval. Its resolved
value is exported. Real elapsed timestamps are preferred; a frame index plus
one confirmed acquisition rate only establishes a reconstructed regular clock.

Portable protocols can contain timing overrides keyed to recording IDs. Import
never assumes that another recording has the same stimulus times. Review the
windows before applying the imported protocol. Version 1 keeps its original
baseline placement and endpoint-clipping policy.

## Quality, eligibility and denominators

- **Responded:** eligible and passed the applied rule.
- **Did not respond:** eligible and did not pass that rule.
- **Excluded:** explicit normalization/measurement exclusion with a reason.
- **Unscorable:** required observations or a valid local reference are unavailable.

The primary percentage is 100 × Responded / (Responded + Did not respond).
Excluded and unscorable cells are separately counted, and a zero denominator is
reported as unavailable. A valid flat trace remains in the denominator. Each
cell is counted once per stimulus, regardless of the number of peaks.

Quality warnings never exclude cells automatically. A small reference relative
to baseline SD, zero baseline variance, or an optional peak-outlier flag deserves
review, but may reflect real biology. The historical 1e-6 initial denominator
guard is retained as a documented numerical validity rule. Normalization with
an artificial positive fluorescence shift is not supported.

With a positive amplitude floor, zero baseline SD can still yield a defined
threshold. Noise-only mode with zero estimated noise is unscorable unless a
nonzero measurement-noise floor is explicitly supplied. Both criteria cannot
be disabled.

The optional control-positive population is a separately labeled result. Its
denominator contains control responders that are also evaluable for the primary
stimulus. Control-negative cells are not automatically labeled dead or removed
from the all-evaluable result.

## Continuous measurements

Existing general metrics retain their historical definitions: a post-loading-
baseline peak, recording-clock peak time, and whole-recording signed AUC.
The additional stimulus-window measurements use local fractional signal and
the applied window. They include local peak, signed AUC, onset, confirmation
time and latency relative to the recorded stimulus onset.

The peak is the observed window maximum, with the first maximum used for ties.
A detected onset is the first qualifying sample; confirmation is when the run
first satisfies the persistence requirement. A nonresponder can have a measured
peak and AUC without having a detected-response latency.

AUC uses actual timestamps and valid adjacent observations. It does not bridge
gaps or extrapolate past the observed endpoints. Integration bounds, observed
duration and coverage are exported; an integral over a shorter observed span is
not silently described as the entire requested interval. Units are fraction·s.
Changing only the classification threshold does not alter continuous metrics.

## Experimental units and export

Record condition and experimental-unit IDs in Load Data. The unit definition
belongs to the experiment: an animal, independently treated well/preparation,
or another justified unit. Fields assigned to a unit are pooled before computing
its responder fraction. The mean of unit fractions is distinct from the pooled
cell fraction. Unassigned fields do not become invented biological replicates.
Repeated views of the same cells should not be counted as new independent cells.

The analysis bundle saves the applied protocol JSON, content hash, run identity,
software/scorer versions, processing manifest, cell calls, population counts,
window measurements, quality diagnostics, normalized traces and unit summaries.
Requested/effective timing and original input units remain explicit. Schema
versions with unsupported semantics are rejected instead of silently receiving
the current defaults.

Both SimpleCa and the Fiji plugin use the same response engine. The Fiji release
pins an upstream revision and verifies its embedded source hashes. Golden
fixtures in tests/fixtures/response-protocols provide independent expected
thresholds, calls, counts and window measurements for cross-application checks.

## Validation before experimental comparisons

Evaluate a candidate protocol on matched vehicle/perfusion controls and
representative stimulus recordings with the same preprocessing, acquisition
cadence and window lengths. Inspect accepted and rejected events against the
images. Select settings on pilot data, then apply the saved rule consistently
across compared groups. Inspect baseline noise, effective thresholds and
exclusion rates alongside responder percentages. Changing settings is a new
analysis configuration and remains visible in the exported record.
