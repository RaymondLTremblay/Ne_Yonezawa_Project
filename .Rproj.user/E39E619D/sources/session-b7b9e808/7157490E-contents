# -------------------------------------------------------------
# Stable run script: Yonezawa paper-mode (auto version + changelog)
# Date: 2026-03-03
# -------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
})

source(file.path("R", "utils_versioning.R"))
source(file.path("R", "Ne_Yonezawa_pipeline_paper.R"))

# ---- User-configurable section -------------------------------------------
STAGES <- c("one_leaf","multileaf_nonfl","multileaf_flower")
MATRICES <- c("yonezawa_mizuyajiri.csv", "yonezawa_nanryu.csv")

# Version bump level for THIS run (set to NULL if no bump): 'patch'|'minor'|'major'|NULL
VERSION_BUMP <- "patch"

# Items to record in CHANGELOG for THIS run
CHANGELOG_ITEMS <- c(
  "Ran paper-mode analysis on Mizuyajiri and Nanryu matrices.",
  "Saved λ, w, v, reproduction placeholders, and Ne summaries with dynamic version tag.",
  "Automated CHANGELOG + VERSION update via utils_versioning.R."
)

# Optional: supply census N or stage-wise variance if available
N_CENSUS <- NA_real_
FEC_MEAN <- NULL
FEC_VAR  <- NULL

# Optional Git auto-commit/tag (set TRUE if your project is a Git repo)
AUTO_GIT <- FALSE

# ---- Start: get version tag and write CHANGELOG entry ---------------------
VERSION_TAG <- start_run(version_bump = VERSION_BUMP, changelog_items = CHANGELOG_ITEMS)
message("Using version tag: ", VERSION_TAG)

# ---- Run analyses ---------------------------------------------------------
for (MATRIX_FILE in MATRICES) {
  message("\n--- Processing (", VERSION_TAG, "): ", MATRIX_FILE, " ---")
  res <- analyze_matrix_yonezawa_paper(
    matrix_file = MATRIX_FILE,
    stages      = STAGES,
    version_tag = VERSION_TAG,
    out_prefix  = tools::file_path_sans_ext(MATRIX_FILE),
    fec_mean    = FEC_MEAN,
    fec_var     = FEC_VAR,
    N_census    = N_CENSUS
  )
  
  cat(sprintf("\nLambda (λ): %.6f\n", res$eig$lambda))
  cat(sprintf("sum(w): %.6f | t(v)%%*%%w: %.6f\n", res$diag$sum_w, res$diag$v_dot_w))
  
  cat("\nStage-wise w, v, fec_mean (paper-mode, sexual+clonal):\n")
  print(as_tibble(read.csv(res$files$stage)))
  
  cat(sprintf("\nMbar (mean reproduction): %.6f | Vbar (variance): %.6f\n", res$ne$Mbar, res$ne$Vbar))
  cat(sprintf("Ne_unit: %.6f\n", res$ne$Ne_unit))
}

# ---- Optional Git commit/tag ---------------------------------------------
if (isTRUE(AUTO_GIT)) {
  msg <- paste0("Run ", VERSION_TAG, ": paper-mode analysis updated outputs.")
  try_git_commit(message = msg, tag = VERSION_TAG)
}