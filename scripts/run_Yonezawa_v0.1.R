# -------------------------------------------------------------
# Run script: Yonezawa Ne (v0.1 scaffold)
# Date: 2026-03-03
# -------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
})

source(file.path("R", "Ne_Yonezawa_pipeline_v0.1.R"))

MATRIX_FILE <- "matrix_input.rds"
STAGES <- NULL
FEC_VAR <- NULL

A <- read_matrix(MATRIX_FILE)
A <- coerce_stage_universe(A, stages = STAGES)

res <- tryCatch({
  compute_ne_yonezawa(A, fec_var = FEC_VAR, stages = STAGES)
}, error = function(e) {
  message("[Info] v0.1 scaffold: core function not yet implemented.\n", e$message)
  NULL
})

if (!is.null(res)) {
  save_results(res, file_basename = "Yonezawa_results", version = "v0.1")
} else {
  message("No results to save in v0.1 (waiting for matrix & variance inputs).")
}
