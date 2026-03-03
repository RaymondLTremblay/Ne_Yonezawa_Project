# -------------------------------------------------------------
# Ne Pipeline (Yonezawa paper-mode: sexual + clonal)
# Version: v0.2
# Date: 2026-03-03
# Author: Raymond L. Tremblay Lalande
#
# Purpose:
#   Step-by-step replication scaffold for Yonezawa et al. (2000)
#   using curated Fritillaria camtschatcensis matrices (Mizuyajiri, Nanryu).
#   v0.2 computes and saves: lambda (λ), stable stage (w), reproductive value (v),
#   plus generation-time placeholder. Ne() remains TODO for v0.3.
#
# References:
#   - Yonezawa et al. 2000, Evolution (stage-structured Ne; clonal + sexual). 
#   - COMPADRE entries for Mizuyajiri and Nanryu mean matrices (1993–1995).
# -------------------------------------------------------------

# ---- Minimal dependency check --------------------------------------------
quiet_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed.", pkg))
  }
}
quiet_require("Matrix")   # for robust matrix handling

# ---- I/O helpers ----------------------------------------------------------
read_matrix <- function(filename) {
  # Reads from project-root/data_raw
  path <- file.path("data_raw", filename)
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") {
    A <- readRDS(path)
  } else if (ext == "csv") {
    # First column is row names; header contains column names
    A <- as.matrix(read.csv(path, check.names = FALSE, row.names = 1))
  } else {
    stop("Unsupported file extension: ", ext)
  }
  A
}

coerce_stage_universe <- function(A, stages = NULL) {
  if (!is.matrix(A)) A <- as.matrix(A)
  if (!is.null(stages)) {
    if (length(stages) != nrow(A)) stop("stages length must equal nrow(A)")
    dimnames(A) <- list(stages, stages)
  }
  A
}

ensure_dir <- function(dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

save_csv <- function(df, path) {
  ensure_dir(dirname(path))
  utils::write.csv(df, path, row.names = FALSE)
  message("Saved CSV: ", normalizePath(path))
}

save_rds <- function(obj, path) {
  ensure_dir(dirname(path))
  saveRDS(obj, path)
  message("Saved RDS: ", normalizePath(path))
}

# ---- Linear algebra core --------------------------------------------------
# Returns: list(lambda, w, v)
# Scaling: sum(w) = 1; and t(v) %*% w = 1
eigen_dominant <- function(A) {
  stopifnot(is.matrix(A) || inherits(A, "Matrix"))
  ev  <- eigen(A, only.values = FALSE)
  idx <- which.max(Re(ev$values))
  lambda <- Re(ev$values[idx])
  w <- Re(ev$vectors[, idx])
  
  evt <- eigen(t(A), only.values = FALSE)
  idxt <- which.max(Re(evt$values))
  v <- Re(evt$vectors[, idxt])
  
  # Scale w, v
  w <- w / sum(w)
  v <- v / as.numeric(crossprod(v, w))
  
  list(lambda = lambda, w = w, v = v)
}

# ---- Diagnostics / sanity checks -----------------------------------------
# Returns list with scalar checks and data.frame for w and v
summarize_eigen <- function(eig, stages) {
  s1 <- sum(eig$w)
  s2 <- as.numeric(t(eig$v) %*% eig$w)
  if (any(eig$w <= 0)) warning("Some entries in w are non-positive.")
  if (any(is.na(eig$w)) || any(is.na(eig$v))) stop("NA in eigenvectors.")
  data.frame(stage = stages, w = eig$w, v = eig$v) -> vecs
  list(sum_w = s1, v_dot_w = s2, vectors = vecs)
}

# ---- Generation time (placeholder) ---------------------------------------
# We add a placeholder for generation time T; implement exact form next.
compute_generation_time <- function(A, eig) {
  # TODO (v0.3): add appropriate generation-time calculation for the
  # stage-structured model under paper-mode assumptions.
  # For now, return NA and document.
  NA_real_
}

# ---- Yonezawa Ne (paper mode) --------------------------------------------
# Placeholder: will be implemented in v0.3 after we lock eigen outputs.
compute_ne_yonezawa_paper <- function(A, eig, stages) {
  stop("Ne computation not yet implemented (v0.3). v0.2 focuses on λ, w, v replication.")
}

# ---- Orchestrator ---------------------------------------------------------
analyze_matrix_yonezawa_paper <- function(
    matrix_file,
    stages,
    version_tag = "v0.2",
    out_prefix = NULL
) {
  if (is.null(out_prefix)) {
    out_prefix <- tools::file_path_sans_ext(matrix_file)
  }
  # Load and set stage universe
  A <- read_matrix(matrix_file)
  A <- coerce_stage_universe(A, stages = stages)
  
  # Eigen analysis
  eig <- eigen_dominant(A)
  diag <- summarize_eigen(eig, stages = stages)
  Tgen <- compute_generation_time(A, eig)
  
  # Save summaries
  # 1) vectors table
  vectors_path <- file.path("output", sprintf("%s_%s_eigen_vectors.csv", out_prefix, version_tag))
  save_csv(diag$vectors, vectors_path)
  
  # 2) scalars (lambda, checks, generation time)
  scalars <- data.frame(
    matrix_file = matrix_file,
    lambda = eig$lambda,
    sum_w = diag$sum_w,
    v_dot_w = diag$v_dot_w,
    generation_time = Tgen,
    version = version_tag,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  scalars_path <- file.path("output", sprintf("%s_%s_scalars.csv", out_prefix, version_tag))
  save_csv(scalars, scalars_path)
  
  # Also return an in-memory object for immediate inspection
  list(A = A, eig = eig, diag = diag, Tgen = Tgen,
       files = list(vectors = vectors_path, scalars = scalars_path))
}