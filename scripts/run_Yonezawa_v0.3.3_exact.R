# -------------------------------------------------------------
# Run: Yonezawa (2000) EXACT replication (v0.3.3_exact)
# Outputs: stage tables, Ny/N and Ne/N for Observed & Expected D,
#          plus a method-audit text documenting equation mappings.
# -------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))

source("R/utils_versioning.R")                             # auto versioning
source("R/Ne_Yonezawa_pipeline_v0.3.3_exact.R")           # exact calculator

MATRICES <- c("Miz","Nan")

VERSION_TAG <- start_run(
  version_bump = "patch",
  changelog_items = c(
    "Implemented EXACT Yonezawa (2000) formulas for Ny/N and Ne/N with equation numbers.",
    "Used Table 2 (U, F, D_obs, D_exp) and Table 4 (L). Saved full audit files."
  )
)
message("Using version tag: ", VERSION_TAG)

for (POP in MATRICES) {
  inp <- get_table2_inputs(POP)
  
  # Observed
  obs <- compute_effective_sizes_exact(inp$U, inp$F, inp$D_obs, inp$L, inp$stages)
  # Expected
  exp <- compute_effective_sizes_exact(inp$U, inp$F, inp$D_exp, inp$L, inp$stages)
  
  # ----- Save stage tables (Observed & Expected)
  obs_path <- file.path("output", sprintf("%s_%s_exact_stage_obs.csv", tolower(POP), VERSION_TAG))
  exp_path <- file.path("output", sprintf("%s_%s_exact_stage_exp.csv", tolower(POP), VERSION_TAG))
  write.csv(obs$stage_table, obs_path, row.names = FALSE)
  write.csv(exp$stage_table, exp_path, row.names = FALSE)
  
  # ----- Save effective sizes (Observed & Expected)
  eff <- tibble::tibble(
    population = POP,
    L = inp$L,
    # Observed
    Ny_over_N_obs = obs$Ny_over_N,
    Ne_over_N_obs = obs$Ne_over_N,
    over_u2_obs = obs$over_u2,
    ubar_obs = obs$ubar,
    Rsum_obs = obs$Rsum,
    Ny_over_N_text_obs = obs$Ny_over_N_text,
    Ne_over_N_text_obs = obs$Ne_over_N_text,
    # Expected
    Ny_over_N_exp = exp$Ny_over_N,
    Ne_over_N_exp = exp$Ne_over_N,
    over_u2_exp = exp$over_u2,
    ubar_exp = exp$ubar,
    Rsum_exp = exp$Rsum,
    Ny_over_N_text_exp = exp$Ny_over_N_text,
    Ne_over_N_text_exp = exp$Ne_over_N_text
  )
  eff_path <- file.path("output", sprintf("%s_%s_exact_effective_sizes.csv", tolower(POP), VERSION_TAG))
  write.csv(eff, eff_path, row.names = FALSE)
  
  # ----- Write a short method-audit file (equation references)
  audit <- c(
    sprintf("Yonezawa (2000) EXACT replication for population: %s", POP),
    "Inputs: Table 2 U, F, D (Observed & Expected); Table 4 L.",
    "",
    "Equation mapping (see PDF):",
    "  (1)-(2): variance decomposition -> defines survival vs recruitment components.",
    "  (3)-(4): S_i, A_i; here a=0, Poisson clonal => S_i=1, A_i=1.",
    "  (5): variance V uses u_bar^2 - overline(u^2) and (1 - u_bar) terms.",
    "       The empirical Ny/N and Ne/N reported in Table 4 are matched by using overline(u^2):",
    "         Ny/N = 1 / (1 - overline(u^2))",
    "         Ne/N = 1 / ( L * (1 - overline(u^2)) )",
    "       (We also compute the textual Eq. (10)-(11) forms using u_bar for comparison.)",
    "",
    sprintf("Observed D:  %s", paste(inp$D_obs, collapse=", ")),
    sprintf("Expected D:  %s", paste(inp$D_exp, collapse=", ")),
    sprintf("u (colSums U): %s", paste(round(colSums(inp$U), 3), collapse=", ")),
    sprintf("L (Table 4): %g", inp$L),
    "",
    "Observed results:",
    sprintf("  overline(u^2) = %.6f | u_bar = %.6f | sum r = %.6f",
            obs$over_u2, obs$ubar, obs$Rsum),
    sprintf("  Ny/N (Table-4 form) = %.6f | Ne/N (Table-4 form) = %.6f",
            obs$Ny_over_N, obs$Ne_over_N),
    sprintf("  Ny/N (Eq. 11 form)   = %.6f | Ne/N (Eq. 10 form)   = %.6f",
            obs$Ny_over_N_text, obs$Ne_over_N_text),
    "",
    "Expected results:",
    sprintf("  overline(u^2) = %.6f | u_bar = %.6f | sum r = %.6f",
            exp$over_u2, exp$ubar, exp$Rsum),
    sprintf("  Ny/N (Table-4 form) = %.6f | Ne/N (Table-4 form) = %.6f",
            exp$Ny_over_N, exp$Ne_over_N),
    sprintf("  Ny/N (Eq. 11 form)   = %.6f | Ne/N (Eq. 10 form)   = %.6f",
            exp$Ny_over_N_text, exp$Ne_over_N_text)
  )
  audit_path <- file.path("output", sprintf("%s_%s_method_audit.txt", tolower(POP), VERSION_TAG))
  writeLines(audit, audit_path)
  
  # ----- Console summary (matches Table 4 layout)
  cat("\n============================\n",
      POP, " (", VERSION_TAG, ")\n", sep = "")
  cat("Observed:  Ny/N =", sprintf("%.3f", obs$Ny_over_N),
      "| Ne/N =", sprintf("%.3f", obs$Ne_over_N), "\n")
  cat("Expected:  Ny/N =", sprintf("%.3f", exp$Ny_over_N),
      "(", ")", "| Ne/N =", sprintf("%.3f", exp$Ne_over_N), "\n")
}