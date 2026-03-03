# -------------------------------------------------------------
# Run v0.4 — Generic + L-from-definition + Sexual-only mode
# -------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))

source("R/utils_versioning.R")
source("R/Ne_Yonezawa_pipeline_v0.4.R")

VERSION_TAG <- start_run(
  version_bump = "minor",
  changelog_items = c(
    "Added generic paper-exact function for any U,F,D,L (A).",
    "Added generation-time L(U,F) from the paper's age-by-stage definition (B).",
    "Added mode='sexual_only' with eigen-based D by default (C)."
  )
)
message("Using version tag: ", VERSION_TAG)

for (POP in c("Miz","Nan")) {
  inp <- get_table2_inputs(POP)
  
  # --- A) Paper-exact (Observed & Expected), using Table 4 L ---
  obs_pe <- compute_effective_sizes_paper_exact(inp$U, inp$F, inp$D_obs, inp$L, stages = inp$stages)
  exp_pe <- compute_effective_sizes_paper_exact(inp$U, inp$F, inp$D_exp, inp$L, stages = inp$stages)
  
  # Save
  write.csv(obs_pe$stage_table, file.path("output", sprintf("%s_%s_v0.4_paper_obs_stage.csv", tolower(POP), VERSION_TAG)), row.names = FALSE)
  write.csv(exp_pe$stage_table, file.path("output", sprintf("%s_%s_v0.4_paper_exp_stage.csv", tolower(POP), VERSION_TAG)), row.names = FALSE)
  write.csv(tibble::tibble(pop=POP, L=inp$L,
                           Ny_over_N_obs=obs_pe$Ny_over_N, Ne_over_N_obs=obs_pe$Ne_over_N,
                           Ny_over_N_exp=exp_pe$Ny_over_N, Ne_over_N_exp=exp_pe$Ne_over_N),
            file.path("output", sprintf("%s_%s_v0.4_paper_effective_sizes.csv", tolower(POP), VERSION_TAG)),
            row.names = FALSE)
  
  cat("\n===", POP, "(paper-exact; Table-4 L)", "===\n")
  cat("Observed:  Ny/N =", round(obs_pe$Ny_over_N,3), "| Ne/N =", round(obs_pe$Ne_over_N,3), "\n")
  cat("Expected:  Ny/N =", round(exp_pe$Ny_over_N,3), "| Ne/N =", round(exp_pe$Ne_over_N,3), "\n")
  
  # --- B) Compute L from definition (U, F, x=1..500) ---
  L_def <- compute_generation_time_yonezawa(inp$U, inp$F, x_max = 500L)
  # Recompute paper-exact with this L
  obs_pe_defL <- compute_effective_sizes_paper_exact(inp$U, inp$F, inp$D_obs, L_def, stages = inp$stages)
  write.csv(tibble::tibble(pop=POP, L_from_def=L_def,
                           Ny_over_N_obs_defL=obs_pe_defL$Ny_over_N,
                           Ne_over_N_obs_defL=obs_pe_defL$Ne_over_N),
            file.path("output", sprintf("%s_%s_v0.4_paper_obs_Ldef.csv", tolower(POP), VERSION_TAG)),
            row.names = FALSE)
  cat("L (definition) =", round(L_def,3), "→ Observed Ny/N, Ne/N =", round(obs_pe_defL$Ny_over_N,3), ",", round(obs_pe_defL$Ne_over_N,3), "\n")
  
  # --- C) Sexual-only demo (set 10% sexual for stage 3; 0 for stages 1–2) ---
  sex_frac <- c(0, 0, 0.10)  # change as you wish; this is illustrative
  sex_only <- compute_effective_sizes_generic(
    U = inp$U, F = inp$F,
    D_source = "eigen",     # recompute stage fractions under A_sexual
    L = NULL,               # compute L from definition using F_sexual
    mode = "sexual_only",
    sexual_fraction = sex_frac,
    stages = inp$stages
  )
  write.csv(sex_only$stage_table,
            file.path("output", sprintf("%s_%s_v0.4_sexonly_stage.csv", tolower(POP), VERSION_TAG)),
            row.names = FALSE)
  write.csv(tibble::tibble(pop=POP,
                           Ny_over_N_sexonly=sex_only$Ny_over_N,
                           Ne_over_N_sexonly=sex_only$Ne_over_N),
            file.path("output", sprintf("%s_%s_v0.4_sexonly_effective_sizes.csv", tolower(POP), VERSION_TAG)),
            row.names = FALSE)
  cat("Sex-only (10% in stage 3): Ny/N =", round(sex_only$Ny_over_N,3),
      "| Ne/N =", round(sex_only$Ne_over_N,3), "\n")
}