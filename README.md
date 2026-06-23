# LSTA-2026-0415 reproducibility package

This repository contains the reproducibility materials for the revised manuscript:

**Lag-Window and Positive-Semidefinite Sandwich Estimation of the Long-Run Covariance Kernel Associated with the Empirical Process under Association**

## Contents

- code/: R scripts and Deucalion SLURM workflows used to run the validated Monte Carlo study and the real-data illustration.
- data/: public IPMA workbook used in the Lisbon temperature illustration.
- esults/monte_carlo/: validated final Monte Carlo outputs, manuscript tables, and validation files.
- esults/real_application/: real-data application outputs, diagnostics, tables, figures, and LaTeX blocks.
- provenance/: Git state, freeze manifests, job identifiers, and SHA-256 checksums.

## Final Monte Carlo validation

The final Monte Carlo run used 4500 atomic tasks:

- 3 dependence scenarios;
- 3 sample sizes;
- 500 replications per cell;
- 4 estimators: Bartlett lag-window, Bartlett sandwich, exact flat-top, and hard truncation.

The validated run satisfies:

- TASKS_OBSERVED=4500;
- METRICS_SELECTED_ROWS=18000;
- GAP_SELECTED_ROWS=4500;
- BS_NEGATIVE_EIGEN_COUNT=0.

## Real-data illustration

The real-data illustration uses daily minimum and maximum temperature observations for Lisbon Geofisico from IPMA. Daily mean temperature is defined as (tmin + tmax) / 2, and the long-run covariance kernel is estimated for centered threshold indicators over the probability grid 0.05, 0.10, ..., 0.95.

## Main empirical finding

The Bartlett sandwich estimator produced no indefinite covariance estimate in
the final Monte Carlo run. In the Lisbon temperature-anomaly application, all
four estimators are positive definite on the selected threshold grid. The
Bartlett sandwich and Bartlett lag-window estimates are numerically very close,
with Frobenius and sup-norm gaps of 0.04639 and 0.00688, respectively, while
the Bartlett sandwich estimator remains positive semidefinite by construction.
The evidence that BL, FT, and HT can be indefinite is provided by the Monte
Carlo study rather than by the adjusted real-data application.

## Reproducibility notes

The final validated code commit on Deucalion was:

33713f03a7a6e0b13995c18905361f667ef58cc1

The final validation tag was:

unified-final4500-validated-20260622

SHA-256 checksums are provided in provenance/.


