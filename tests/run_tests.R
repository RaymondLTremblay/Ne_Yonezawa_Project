# Minimal runner for non-package projects
suppressPackageStartupMessages({
  library(testthat)
})

source(file.path("R","Ne_Yonezawa_pipeline_v0.3.3_exact.R"))

# Helper to capture outputs (Observed & Expected) for a population
get_exact <- function(pop) {
  inp <- get_table2_inputs(pop)
  obs <- compute_effective_sizes_exact(inp$U, inp$F, inp$D_obs, inp$L, inp$stages)
  exp <- compute_effective_sizes_exact(inp$U, inp$F, inp$D_exp, inp$L, inp$stages)
  list(obs=obs, exp=exp, L=inp$L)
}

test_that("Yonezawa Table 4 is reproduced for Miz", {
  ez <- get_exact("Miz")
  expect_equal(round(ez$obs$Ny_over_N, 3), 2.932, tolerance = 1e-3)
  expect_equal(round(ez$obs$Ne_over_N, 3), 0.219, tolerance = 1e-3)
  expect_equal(round(ez$exp$Ny_over_N, 3), 2.976, tolerance = 1e-3)
  expect_equal(round(ez$exp$Ne_over_N, 3), 0.222, tolerance = 1e-3)
})

test_that("Yonezawa Table 4 is reproduced for Nan", {
  ez <- get_exact("Nan")
  expect_equal(round(ez$obs$Ny_over_N, 3), 2.428, tolerance = 1e-3)
  expect_equal(round(ez$obs$Ne_over_N, 3), 0.291, tolerance = 1e-3)
  expect_equal(round(ez$exp$Ny_over_N, 3), 2.446, tolerance = 1e-3)
  expect_equal(round(ez$exp$Ne_over_N, 3), 0.293, tolerance = 1e-3)
})

cat("\nAll paper‑exact tests passed.\n")