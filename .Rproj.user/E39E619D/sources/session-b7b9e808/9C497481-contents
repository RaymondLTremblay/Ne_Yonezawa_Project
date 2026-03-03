# -------------------------------------------------------------
# Ne Pipeline (Yonezawa paper-mode: sexual + clonal)
# Version-agnostic file — versioning handled via utils_versioning.R
# Date: 2026-03-03
# -------------------------------------------------------------

quiet_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed.", pkg))
  }
}
quiet_require("Matrix")

# ---- I/O helpers ----------------------------------------------------------
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

coerce_stage_universe <- function(A, stages = NULL) {
  if (!is.matrix(A)) A <- as.matrix(A)
  if (!is.null(stages)) {
    if (length(stages) != nrow(A)) stop("stages length must equal nrow(A)")
    dimnames(A) <- list(stages, stages)
  }
  A
}

ensure_dir <- function(dir) if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

save_csv <- function(df, path) {
  ensure_dir(dirname(path))
  utils::write.csv(df, path, row.names = FALSE)
  message("Saved CSV: ", normalizePath(path))
}

# ---- Eigen core -----------------------------------------------------------
# Scaling: sum(w)=1 and t(v)%*%w = 1
eigen_dominant <- function(A) {
  stopifnot(is.matrix(A) || inherits(A, "Matrix"))
  ev  <- eigen(A, only.values = FALSE)
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

summarize_eigen <- function(eig, stages) {
  s1 <- sum(eig$w)
  s2 <- as.numeric(t(eig$v) %*% eig$w)
  if (any(eig$w <= 0)) warning("Some entries in w are non-positive.")
  if (any(is.na(eig$w)) || any(is.na(eig$v))) stop("NA in eigenvectors.")
  data.frame(stage = stages, w = eig$w, v = eig$v) -> vecs
  list(sum_w = s1, v_dot_w = s2, vectors = vecs)
}

# ---- Generation time (placeholder) ---------------------------------------
compute_generation_time <- function(A, eig) {
  NA_real_
}

# ---- Stage-wise reproduction (paper-mode: sexual + clonal) ---------------
extract_reproduction_by_stage <- function(A, stages, fec_mean = NULL, fec_var = NULL) {
  repro <- as.numeric(A[1, ])
  names(repro) <- stages
  if (is.null(fec_mean)) fec_mean <- repro
  if (is.null(fec_var))  fec_var  <- fec_mean
  list(mean = fec_mean, var = fec_var)
}

# ---- Yonezawa Ne (paper-mode) --------------------------------------------
compute_ne_yonezawa_paper <- function(A, eig, stages, fec_mean = NULL, fec_var = NULL, N_census = NA_real_) {
  fr <- extract_reproduction_by_stage(A, stages, fec_mean = fec_mean, fec_var = fec_var)
  m  <- fr$mean
  v2 <- fr$var
  w <- eig$w; v <- eig$v
  contrib <- v * m
  phi <- contrib / sum(contrib)
  Mbar <- sum(w * m)
  Vbar <- sum(phi * v2)
  Ne_unit <- if (Vbar > 0) (Mbar^2) / Vbar else NA_real_
  Ne <- if (!is.na(N_census)) Ne_unit * N_census else Ne_unit
  list(stages = stages, w = w, v = v, fec_mean = m, fec_var = v2, phi = phi,
       Mbar = Mbar, Vbar = Vbar, Ne_unit = Ne_unit, Ne = Ne, N_census = N_census)
}

# ---- Orchestrator ---------------------------------------------------------
analyze_matrix_yonezawa_paper <- function(
    matrix_file,
    stages,
    version_tag,
    out_prefix = NULL,
    fec_mean = NULL,
    fec_var = NULL,
    N_census = NA_real_
) {
  if (missing(version_tag) || is.null(version_tag)) stop("version_tag is required — use utils_versioning::start_run() to get one.")
  if (is.null(out_prefix)) out_prefix <- tools::file_path_sans_ext(matrix_file)
  A <- read_matrix(matrix_file)
  A <- coerce_stage_universe(A, stages = stages)
  eig <- eigen_dominant(A)
  diag <- summarize_eigen(eig, stages = stages)
  Tgen <- compute_generation_time(A, eig)
  ne_out <- compute_ne_yonezawa_paper(A, eig, stages, fec_mean = fec_mean, fec_var = fec_var, N_census = N_census)
  
  # Save outputs with dynamic version tag
  vectors_path <- file.path("output", sprintf("%s_%s_eigen_vectors.csv", out_prefix, version_tag))
  save_csv(diag$vectors, vectors_path)
  
  scalars <- data.frame(
    matrix_file = matrix_file,
    lambda = eig$lambda,
    sum_w = diag$sum_w,
    v_dot_w = diag$v_dot_w,
    generation_time = Tgen,
    Mbar = ne_out$Mbar,
    Vbar = ne_out$Vbar,
    Ne_unit = ne_out$Ne_unit,
    Ne = ne_out$Ne,
    N_census = ne_out$N_census,
    version = version_tag,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  scalars_path <- file.path("output", sprintf("%s_%s_scalars.csv", out_prefix, version_tag))
  save_csv(scalars, scalars_path)
  
  stage_tbl <- data.frame(
    stage = stages,
    w = ne_out$w,
    v = ne_out$v,
    fec_mean = ne_out$fec_mean,
    fec_var = ne_out$fec_var,
    phi = ne_out$phi
  )
  stage_path <- file.path("output", sprintf("%s_%s_stage_params.csv", out_prefix, version_tag))
  save_csv(stage_tbl, stage_path)
  
  list(A = A, eig = eig, diag = diag, Tgen = Tgen, ne = ne_out,
       files = list(vectors = vectors_path, scalars = scalars_path, stage = stage_path))
}