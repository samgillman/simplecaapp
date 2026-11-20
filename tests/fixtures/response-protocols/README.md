# Shared scientific response fixtures

These synthetic fixtures are intended for the SimpleCa application and the Fiji
adapter to run through the same R response engine. They are analytical software
checks, not experimental validation of a biological response threshold.

## Contents

- `golden-protocol.json`: portable schema 2, engine 2.0.0, with a primary
  capsaicin window and a generically named ionomycin control.
- `golden-traces.csv`: fixed-reference fractional traces sampled every 0.5 s.
  A normalized baseline of zero represents valid fluorescence relative to its
  reference; it is not a raw fluorescence baseline of zero.
- `golden-expected-calls.csv`: independently calculated per-cell classifications,
  thresholds, run times and signed window integrals.
- `golden-expected-counts.csv`: independently calculated numerator/denominator
  results, including the conditional control-positive population.

The registered cell `InvalidF0` is intentionally absent from the trace matrix.
The test supplies an explicit quality row with `Status = "Excluded"`,
`F0 = 0` and reason `Invalid raw fluorescence baseline`. An absent trace without
that quality information is instead unscorable under schema 2.

## Exact definitions

Primary baseline: `2 <= time < 5`; primary response window: `5 <= time <= 8`.
Control baseline: `9 <= time < 12`; control response window: `12 <= time <= 15`.

The provisional response rule uses a local fractional rise of at least
`max(0.10, 3 * baseline sample SD)`. A qualifying observed run must span at
least one second from its first to its last sample. At 2 Hz, that takes three
consecutive observations. Actual timestamps govern the decision. Missing
values, unacceptable gaps and incomplete requested windows do not become
negative responses; they make the applicable cell/window unscorable.

The noisy cell has six baseline values alternating between -0.05 and +0.05.
Its sample SD is `sqrt(0.003)`, so its primary threshold is approximately
0.1643167673. Its 0.12 primary rise therefore fails the noise criterion.

The sustained primary trace is 0.2 at 5, 5.5 and 6 s, and zero thereafter.
Its observed signed trapezoidal integral over the response window is 0.25
fraction-seconds. The brief trace has two elevated samples and an integral
of 0.15 fraction-seconds; it does not satisfy a one-second run. Its continuous
measurements remain in the exported results.

The primary result is 1 responder among 4 eligible cells: 25%. One additional
cell is explicitly excluded and one is unscorable. The control has 4 responders
among 5 eligible cells: 80%. Of the four control-positive cells, three are
eligible for the primary measurement; one responds, yielding 33.333...%.

## Additional behavioral tests

`tests/testthat/test-response-protocols.R` also verifies legacy two-sample
reproduction and endpoint clipping; optional legacy annotations; independent
baseline, application and response timing; per-recording overrides; timestamp
gaps and sparse sampling; noise-only zero-variance handling; warning-only
baseline variability flags; signed integrals; actual integration coverage;
immutable sensitivity reruns; explicitly selected negative-control windows;
and biological-replicate versus cell-pooled summaries.

Schema 2 uses `fixed_reference_fractional_change` so the same mathematics can
describe fluorescence or an intensity ratio. Acquisition metadata must identify
the signal kind. Portable measurement column names are
`Window_peak_local_fraction` and `Window_AUC_fraction_s`; the R API retains
temporary `dFF` aliases for front-end compatibility, which should not be exported
as ratio-specific measurement labels.

The engine records sample SD, inclusive threshold comparison (`>=`), no
smoothing, and complete-window eligibility. The three-baseline-sample default
is a computational guard, not a recommendation that three observations suffice
to calibrate a biological assay. The 10% / 3 SD / 1 s preset remains provisional
until assessed using suitable control recordings for the actual preparation.

Protocol MD5 values are deterministic content fingerprints, not signatures.
Import rejects unsupported schema/engine versions and inconsistent duplicated
metadata rather than silently applying current semantics to a future protocol.
