# -------------------------------------------------------------
# Ne Pipeline v0.4 — Paper-exact + Generic + Sexual-only mode
# (A) Generic paper-exact calc for any U, F, D, L
# (B) Generation time L(U,F) from Yonezawa's definition (x=1..500)
# (C) mode = "paper_exact" (default) or "sexual_only"
# -------------------------------------------------------------

quiet_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed.", pkg))
  }
}
quiet_require("Matrix")

# ---------- Table 2 helpers (from v0.3.3_exact) ----------
get_table2_inputs <- function(pop = c("Miz","Nan")) {
  pop <- match.arg(pop)
  if (pop == "Miz") {
    U <- matrix(c(
      0.789,0.121,0.054,
      0.007,0.621,0.335,
      0.001,0.258,0.611
    ), nrow=3, byrow=TRUE,
    dimnames=list(c("one_leaf","multileaf_nonfl","multileaf_flower"),
                  c("one_leaf","multileaf_nonfl","multileaf_flower")))
    F <- c(0.055,1.328,2.398)
    D_obs <- c(0.935,0.038,0.027)
    D_exp <- c(0.921,0.046,0.033)
    L <- 13.399
  } else {
    U <- matrix(c(
      0.748,0.137,0.138,
      0.006,0.669,0.374,
      0.001,0.194,0.488
    ), nrow=3, byrow=TRUE,
    dimnames=list(c("one_leaf","multileaf_nonfl","multileaf_flower"),
                  c("one_leaf","multileaf_nonfl","multileaf_flower")))
    F <- c(0.138,2.773,5.016)
    D_obs <- c(0.958,0.027,0.015)
    D_exp <- c(0.951,0.034,0.015)
    L <- 8.353
  }
  list(U=U,F=F,D_obs=D_obs,D_exp=D_exp,L=L,
       stages=c("one_leaf","multileaf_nonfl","multileaf_flower"))
}

# ---------- (B) Generation time L from U, F (Yonezawa text) ----------
# Paper definition (x = 1..500):
# u_{j|x} = element (row j, col 1) of U^x
# l_x     = sum_j u_{j|x}
# m_x     = sum_j F_j * u_{j|x} / l_x
# L       = (sum_x x * m_x * l_x) / (sum_x m_x * l_x)
compute_generation_time_yonezawa <- function(U, F, x_max = 500L) {
  stopifnot(nrow(U) == ncol(U), length(F) == nrow(U))
  # Assume stage 1 is the recruiting stage (as in Table 2)
  # Compute U^x progressively for stability
  Upow <- diag(nrow(U))
  num <- 0
  den <- 0
  for (x in 1:x_max) {
    Upow <- Upow %*% U
    u_j_given_x <- Upow[,1]                # column 1 (from newborns to stage j at age x)
    l_x <- sum(u_j_given_x)
    if (l_x <= 0) next
    m_x <- sum(F * u_j_given_x) / l_x
    num <- num + (x * m_x * l_x)
    den <- den + (m_x * l_x)
  }
  if (den == 0) return(NA_real_)
  num / den
}

# ---------- (A) Paper-exact effective sizes for any U, F, D, L ----------
# Reproduces Table 4 mapping:
# Ny/N = 1 / (1 - overline(u^2)),  Ne/N = 1 / ( L * (1 - overline(u^2)) )
compute_effective_sizes_paper_exact <- function(U, F, D, L, stages = NULL) {
  if (!is.null(stages)) {
    if (length(stages) != nrow(U)) stop("stages length mismatch")
    dimnames(U) <- list(stages, stages)
  }
  u <- colSums(U)
  r <- F * D
  over_u2 <- sum(D * (u^2))
  ubar    <- sum(D *  u  )  # reported for audit
  
  Ny_over_N_tab  <- 1 / (1 - over_u2)
  Ne_over_N_tab  <- 1 / (L * (1 - over_u2))
  Ny_over_N_text <- 1 / (1 - ubar)            # Eq. (11) textual form (for audit only)
  Ne_over_N_text <- 1 / (2 * L * (1 - ubar))  # Eq. (10) textual form (for audit only)
  
  list(
    stage_table = data.frame(stage = rownames(U), D = D, u = u, F = F, r = r),
    Rsum = sum(r), over_u2 = over_u2, ubar = ubar,
    Ny_over_N = Ny_over_N_tab, Ne_over_N = Ne_over_N_tab,
    Ny_over_N_text = Ny_over_N_text, Ne_over_N_text = Ne_over_N_text
  )
}

# ---------- (C) Generic wrapper with mode and D source ----------
# D_source = "observed" | "expected" | "eigen" | numeric vector
# mode     = "paper_exact" (use F as given) | "sexual_only" (use F_sexual or sexual_fraction)
compute_effective_sizes_generic <- function(
    U, F,
    D_source = c("eigen","observed","expected"),
    D_obs = NULL, D_exp = NULL,
    L = NULL, x_max = 500L,
    mode = c("paper_exact","sexual_only"),
    F_sexual = NULL,
    sexual_fraction = NULL,    # if supplied, F_use = sexual_fraction * F
    stages = NULL
){
  D_source <- match.arg(D_source)
  mode     <- match.arg(mode)
  if (!is.null(stages)) dimnames(U) <- list(stages, stages)
  
  # 1) Choose the fecundity used by this mode
  if (mode == "paper_exact") {
    F_use <- F
  } else {
    if (!is.null(F_sexual)) {
      F_use <- F_sexual
    } else if (!is.null(sexual_fraction)) {
      if (length(sexual_fraction) != length(F)) stop("sexual_fraction length mismatch")
      F_use <- sexual_fraction * F
    } else {
      stop("sexual_only mode requires F_sexual or sexual_fraction")
    }
  }
  
  # 2) Choose the stage distribution D
  if (is.numeric(D_source)) {
    D_use <- D_source
  } else if (D_source == "observed") {
    if (is.null(D_obs)) stop("D_obs required for D_source='observed'")
    D_use <- D_obs
  } else if (D_source == "expected") {
    if (is.null(D_exp)) stop("D_exp required for D_source='expected'")
    D_use <- D_exp
  } else { # "eigen" (recommended for exploratory modes)
    A <- U
    A[1, ] <- F_use
    ev <- eigen(A)
    i  <- which.max(Re(ev$values))
    w  <- Re(ev$vectors[, i]); w <- w / sum(w)
    D_use <- w
  }
  
  # 3) Generation time L
  if (is.null(L)) {
    # Compute from definition (B)
    L_use <- compute_generation_time_yonezawa(U, F_use, x_max = x_max)
  } else {
    L_use <- L
  }
  
  # 4) Paper-exact effective sizes using chosen D and L
  compute_effective_sizes_paper_exact(U, F_use, D_use, L_use, stages = rownames(U))
}