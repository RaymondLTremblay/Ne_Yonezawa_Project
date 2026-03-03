# --- scripts/one_time_make_table2_matrices.R ---

# Mizuyajiri (Table 2)
U_miz <- matrix(c(
  0.789, 0.121, 0.054,
  0.007, 0.621, 0.335,
  0.001, 0.258, 0.611
), nrow = 3, byrow = TRUE)
F_miz <- c(0.055, 1.328, 2.398)

A_miz <- U_miz
A_miz[1, ] <- F_miz
dimnames(A_miz) <- list(c("one_leaf","multileaf_nonfl","multileaf_flower"),
                        c("one_leaf","multileaf_nonfl","multileaf_flower"))
write.csv(A_miz, "data_raw/yonezawa_miz_table2.csv")

# Nanryu (Table 2)
U_nan <- matrix(c(
  0.748, 0.137, 0.138,
  0.006, 0.669, 0.374,
  0.001, 0.194, 0.488
), nrow = 3, byrow = TRUE)
F_nan <- c(0.138, 2.773, 5.016)

A_nan <- U_nan
A_nan[1, ] <- F_nan
dimnames(A_nan) <- list(c("one_leaf","multileaf_nonfl","multileaf_flower"),
                        c("one_leaf","multileaf_nonfl","multileaf_flower"))
write.csv(A_nan, "data_raw/yonezawa_nan_table2.csv")