
# Ne Yonezawa Pipeline — Changelog

## v0.3 (2026-03-03)
- Added `R/Ne_Yonezawa_pipeline_v0.3_paper.R` and `scripts/run_Yonezawa_v0.3_paper.R`.
- Paper‑mode (sexual + clonal) **Ne** implementation scaffold following Yonezawa et al. (2000).
- Saves λ, w, v, stage-wise reproduction placeholders (`fec_mean`, `fec_var`), normalized weights (`phi`), and Ne summaries:
  - `output/*_v0.3_eigen_vectors.csv`
  - `output/*_v0.3_stage_params.csv`
  - `output/*_v0.3_scalars.csv`
- Defaults assume Poisson variance (= mean) for stage‑wise reproduction; user‑supplied variance can override.
- Next: refine variance terms from empirical counts and add `mode = "sexual_only"` option.

## v0.2 (2026-03-03)
- Added `R/Ne_Yonezawa_pipeline_v0.2_paper.R` and `scripts/run_Yonezawa_v0.2_paper.R`.
- **Paper‑mode replication (sexual + clonal)** for Yonezawa et al. (2000) matrices (Mizuyajiri, Nanryu).
- Computes and **saves λ, stable stage distribution (w), and reproductive value (v)**:
  - `output/*_v0.2_eigen_vectors.csv`
  - `output/*_v0.2_scalars.csv`
- Verified that λ matches COMPADRE curated values for both sites.

## v0.1 (2026-03-03)
- Project initialized as `Ne_Yonezawa_Project`.
- Created folder structure: `data_raw/`, `data_processed/`, `R/`, `scripts/`, `output/`, `logs/`.
- Added initial scaffold files:
  - `R/Ne_Yonezawa_pipeline_v0.1.R`
  - `scripts/run_Yonezawa_v0.1.R`
- `compute_ne_yonezawa()` placeholder pending matrix & variance inputs.






## v0.3.1 (2026-03-03)
 - Ran paper-mode analysis on Mizuyajiri and Nanryu matrices.
- Saved λ, w, v, reproduction placeholders, and Ne summaries with dynamic version tag.
- Automated CHANGELOG + VERSION update via utils_versioning.R. 

## v0.3.2 (2026-03-03)
 - Ran paper-mode analysis on Mizuyajiri and Nanryu matrices.
- Saved λ, w, v, reproduction placeholders, and Ne summaries with dynamic version tag.
- Automated CHANGELOG + VERSION update via utils_versioning.R. 

## v0.3.3 (2026-03-03)
 - Implemented EXACT Yonezawa (2000) formulas for Ny/N and Ne/N with equation numbers.
- Used Table 2 (U, F, D_obs, D_exp) and Table 4 (L). Saved full audit files. 

## v0.4.0 (2026-03-03)
 - Added generic paper-exact function for any U,F,D,L (A).
- Added generation-time L(U,F) from the paper's age-by-stage definition (B).
- Added mode='sexual_only' with eigen-based D by default (C). 
