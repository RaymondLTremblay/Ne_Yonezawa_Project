# -------------------------------------------------------------
# Ne Pipeline (Yonezawa-only)
# Version: v0.1
# Date: 2026-03-03
# Author: Raymond L. Tremblay Lalande
#
# Changelog (summary for this file):
# v0.1 - Initial scaffold: project structure, placeholders, I/O helpers.
# -------------------------------------------------------------

quiet_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed.", pkg))
  }
}

quiet_require("Matrix")

eigen_dominant <- function(A) {
  stopifnot(is.matrix(A) || inherits(A, "Matrix"))
  ev <- eigen(A, only.values = FALSE)
  idx <- which.max(Re(ev$values))
  lambda <- Re(ev$values[idx])
  w <- Re(ev$vectors[, idx])
  
  evt <- eigen(t(A), only.values = FALSE)
  idxt <- which.max(Re(evt$values))
  v <- Re(evt$vectors[, idxt])
  
  w <- w / sum(w)
  v <- v / as.numeric(crossprod(v, w))
  
  list(lambda = lambda, w = w, v = v)
}

compute_ne_yonezawa <- function(A, fec_var = NULL, stages = NULL) {
  stop("compute_ne_yonezawa() not yet implemented: waiting for matrix & variance inputs (v0.2).")
}

coerce_stage_universe <- function(A, stages = NULL) {
  if (!is.matrix(A)) A <- as.matrix(A)
  if (!is.null(stages)) {
    if (length(stages) != nrow(A)) stop("stages length must equal nrow(A)")
    dimnames(A) <- list(stages, stages)
  }
  A
}

save_results <- function(x, file_basename, version = "v0.1") {
  if (!dir.exists("output")) dir.create("output")
  path <- file.path("output", sprintf("%s_%s.rds", file_basename, version))
  saveRDS(x, path)
  message("Saved: ", normalizePath(path))
  invisible(path)
}

read_matrix <- function(filename) {
  path <- file.path("data_raw", filename)
  ext <- tolower(tools::file_ext(path))
  
  if (ext == "rds") {
    A <- readRDS(path)
  } else if (ext == "csv") {
    A <- as.matrix(read.csv(path, check.names = FALSE, row.names = 1))
  } else {
    stop("Unsupported file extension: ", ext)
  }
  
  A
}