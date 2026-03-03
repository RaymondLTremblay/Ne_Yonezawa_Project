# -------------------------------------------------------------
# Run script: Yonezawa paper-mode replication (v0.2)
# Date: 2026-03-03
# -------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
})

source(file.path("R", "Ne_Yonezawa_pipeline_v0.2_paper.R"))

# ---- User inputs ----------------------------------------------------------
# Use the CSVs you created with Option #3:
#   yonezawa_mizuyajiri.csv
#   yonezawa_nanryu.csv
STAGES <- c("one_leaf","multileaf_nonfl","multileaf_flower")

# Run both matrices in sequence, saving outputs for each
for (MATRIX_FILE in c("yonezawa_mizuyajiri.csv", "yonezawa_nanryu.csv")) {
  
  message("\n--- Processing: ", MATRIX_FILE, " ---")
  res <- analyze_matrix_yonezawa_paper(
    matrix_file = MATRIX_FILE,
    stages      = STAGES,
    version_tag = "v0.2",
    out_prefix  = tools::file_path_sans_ext(MATRIX_FILE)
  )
  
  # Print a concise console summary
  cat(sprintf("\nLambda (λ): %.6f\n", res$eig$lambda))
  cat(sprintf("sum(w): %.6f | t(v)%%*%%w: %.6f\n", res$diag$sum_w, res$diag$v_dot_w))
  print(as_tibble(res$diag$vectors))
}