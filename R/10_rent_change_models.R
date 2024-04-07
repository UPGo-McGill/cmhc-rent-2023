#### RENT CHANGE MODELS ########################################################

source("R/06_data_for_models.R")

fc <- list()
mc <- list()
ec <- list()


# Legend ------------------------------------------------------------------

# FREH: FREH_change only
# rev: rev_change only
# both: FREH_change and rev_change
# outliers: Alternative specification with outliers removed
# impute: Alternative specification with imputed rent values
# alt: Alternative specification with FREH_3_change
# count: Alternative specification with FREH_count_change and rev_count_change
# housing: Alternative specification with housing only


# Formulas ----------------------------------------------------------------

fc$FREH <- rent_change ~ FREH_change + universe_change + universe_log + tenant + 
  tourism_log + CMA:year

fc$rev <- rent_change ~ rev_change + universe_change + universe_log + tenant + 
  tourism_log + CMA:year

fc$both <- rent_change ~ FREH_change + rev_change + universe_change + 
  universe_log + tenant + tourism_log + CMA:year

fc$s_FREH <- c("FREH_change", "universe_change", "universe_log", "tenant", 
               "tourism_log")

fc$s_rev <- c("rev_change", "universe_change", "universe_log", "tenant", 
              "tourism_log")
  
fc$s_both <- c("FREH_change", "rev_change", "universe_change", "universe_log", 
               "tenant", "tourism_log")


# Linear models -----------------------------------------------------------

mc$l_FREH <- lm(fc$FREH, data = dc$main)
mc$l_rev <- lm(fc$rev, data = dc$main)
mc$l_both <- lm(fc$both, data = dc$main)
mc$l_outliers <- lm(fc$both, data = dc$outliers)
mc$l_impute <- lm(fc$both, data = dc$impute)
mc$l_alt <- lm(fc$both, data = dc$alt)
mc$l_count <- lm(fc$both, data = dc$count)
mc$l_housing <- lm(fc$both, data = dc$housing)
mc$l_lag <- lm(fc$both, data = dc$lag)

# Models by province
mc$l_p <- map(list(
  "British Columbia", c("Alberta", "Saskatchewan", "Manitoba"),
  "Ontario", "Quebec", c("New Brunswick", "Nova Scotia", "Prince Edward Island", 
                         "Newfoundland and Labrador")), 
  \(x) lm(fc$both, data = filter(dc$main, province %in% x)))


# Prepare eigenvectors for spatial regressions ----------------------------

ec <- map(dc, \(x) {
  x |> 
    st_transform(4326) |> 
    st_set_agr("constant") |> 
    st_centroid() |> 
    st_coordinates() |> 
    meigen(s_id = x$id)
})


# RE-ESF ------------------------------------------------------------------

mc$sf_FREH <- resf(
  y = dc$main$rent_change,
  x = st_drop_geometry(dc$main[fc$s_FREH]),
  meig = ec$main,
  xgroup = st_drop_geometry(dc$main[c("CMA", "year")]))

mc$sf_rev <- resf(
  y = dc$main$rent_change,
  x = st_drop_geometry(dc$main[fc$s_rev]),
  meig = ec$main,
  xgroup = st_drop_geometry(dc$main[c("CMA", "year")]))

mc$sf_both <- resf(
  y = dc$main$rent_change,
  x = st_drop_geometry(dc$main[fc$s_both]),
  meig = ec$main,
  xgroup = st_drop_geometry(dc$main[c("CMA", "year")]))

mc$sf_outliers <- resf(
  y = dc$outliers$rent_change,
  x = st_drop_geometry(dc$outliers[fc$s_both]),
  meig = ec$outliers,
  xgroup = st_drop_geometry(dc$outliers[c("CMA", "year")]))

mc$sf_impute <- resf(
  y = dc$impute$rent_change,
  x = st_drop_geometry(dc$impute[fc$s_both]),
  meig = ec$impute,
  xgroup = st_drop_geometry(dc$impute[c("CMA", "year")]))

mc$sf_alt <- resf(
  y = dc$alt$rent_change,
  x = st_drop_geometry(dc$alt[fc$s_both]),
  meig = ec$alt,
  xgroup = st_drop_geometry(dc$alt[c("CMA", "year")]))

mc$sf_count <- resf(
  y = dc$count$rent_change,
  x = st_drop_geometry(dc$count[fc$s_both]),
  meig = ec$count,
  xgroup = st_drop_geometry(dc$count[c("CMA", "year")]))

mc$sf_housing <- resf(
  y = dc$housing$rent_change,
  x = st_drop_geometry(dc$housing[fc$s_both]),
  meig = ec$housing,
  xgroup = st_drop_geometry(dc$housing[c("CMA", "year")]))

mc$sf_lag <- resf(
  y = dc$lag$rent_change,
  x = st_drop_geometry(dc$lag[fc$s_both]),
  meig = ec$lag,
  xgroup = st_drop_geometry(dc$lag[c("CMA", "year")]))


# S&NVC -------------------------------------------------------------------

mc$sn_FREH <- resf_vc(
  y = dc$main$rent_change,
  x = st_drop_geometry(dc$main[fc$s_FREH]),
  meig = ec$main,
  xgroup = st_drop_geometry(dc$main[c("CMA", "year")]),
  x_nvc = TRUE)

mc$sn_rev <- resf_vc(
  y = dc$main$rent_change,
  x = st_drop_geometry(dc$main[fc$s_rev]),
  meig = ec$main,
  xgroup = st_drop_geometry(dc$main[c("CMA", "year")]),
  x_nvc = TRUE)

mc$sn_both <- resf_vc(
  y = dc$main$rent_change,
  x = st_drop_geometry(dc$main[fc$s_both]),
  meig = ec$main,
  xgroup = st_drop_geometry(dc$main[c("CMA", "year")]),
  x_nvc = TRUE)

mc$sn_outliers <- resf_vc(
  y = dc$outliers$rent_change,
  x = st_drop_geometry(dc$outliers[fc$s_both]),
  meig = ec$outliers,
  xgroup = st_drop_geometry(dc$outliers[c("CMA", "year")]),
  x_nvc = TRUE)

mc$sn_impute <- resf_vc(
  y = dc$impute$rent_change,
  x = st_drop_geometry(dc$impute[fc$s_both]),
  meig = ec$impute,
  xgroup = st_drop_geometry(dc$impute[c("CMA", "year")]),
  x_nvc = TRUE)

mc$sn_alt <- resf_vc(
  y = dc$alt$rent_change,
  x = st_drop_geometry(dc$alt[fc$s_both]),
  meig = ec$alt,
  xgroup = st_drop_geometry(dc$alt[c("CMA", "year")]),
  x_nvc = TRUE)

mc$sn_count <- resf_vc(
  y = dc$count$rent_change,
  x = st_drop_geometry(dc$count[fc$s_both]),
  meig = ec$count,
  xgroup = st_drop_geometry(dc$count[c("CMA", "year")]),
  x_nvc = TRUE)

mc$sn_housing <- resf_vc(
  y = dc$housing$rent_change,
  x = st_drop_geometry(dc$housing[fc$s_both]),
  meig = ec$housing,
  xgroup = st_drop_geometry(dc$housing[c("CMA", "year")]),
  x_nvc = TRUE)

mc$sn_lag <- resf_vc(
  y = dc$lag$rent_change,
  x = st_drop_geometry(dc$lag[fc$s_both]),
  meig = ec$lag,
  xgroup = st_drop_geometry(dc$lag[c("CMA", "year")]),
  x_nvc = TRUE)
