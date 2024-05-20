#### RENT MODELS ###############################################################

source("R/06_data_for_models.R")

fr <- list()
mr <- list()


# Prepare eigenvectors for spatial regressions ----------------------------

er <- map(dr, \(x) {
  x |> 
    st_transform(4326) |> 
    st_set_agr("constant") |> 
    st_centroid() |> 
    st_coordinates() |> 
    meigen(s_id = x$id)
})


# Legend ------------------------------------------------------------------

# FREH: FREH only
# rev: rev only
# both: FREH and rev
# vacancy: FREH and rev, vacancy included
# outliers: Alternative specification with outliers removed
# no_zero: Alternative specification with zeroes removed
# impute: Alternative specification with imputed rent values
# impute_vacancy: Alternative specification with imputed rent/vacancy values
# alt: Alternative specification with FREH_3
# housing: Alternative specification with housing only


# Formulas ----------------------------------------------------------------

fr$FREH <- rent_log ~ FREH_log + FREH_dummy + universe_log + tenant + 
  tourism_log + CMA:year

fr$rev <- rent_log ~ rev_log + rev_dummy + universe_log + tenant + 
  tourism_log + CMA:year

fr$both <- rent_log ~ FREH_log + FREH_dummy + rev_log + rev_dummy + 
  universe_log + tenant + tourism_log + CMA:year

fr$vacancy <- rent_log ~ FREH_log + FREH_dummy + rev_log + rev_dummy + 
  universe_log + vacancy + tenant + tourism_log + CMA:year

fr$outliers <- rent_log ~ FREH_log + FREH_dummy + rev_log + universe_log + 
  tenant + tourism_log + CMA:year

fr$no_zero <- rent_log ~ FREH_log + rev_log + universe_log + tenant + 
  tourism_log + CMA:year

fr$year_FREH <- rent_log ~ FREH_log + FREH_dummy + universe_log + tenant + 
  tourism_log + CMA

fr$year_rev <- rent_log ~ rev_log + rev_dummy + universe_log + tenant + 
  tourism_log + CMA

fr$year_both <- rent_log ~ FREH_log + FREH_dummy + rev_log + rev_dummy + 
  universe_log + tenant + tourism_log + CMA

fr$year_vacancy <- rent_log ~ FREH_log + FREH_dummy + rev_log + rev_dummy + 
  universe_log + vacancy + tenant + tourism_log + CMA

fr$s_FREH <- c("FREH_log", "FREH_dummy", "universe_log", "tenant", 
               "tourism_log")

fr$s_rev <- c("rev_log", "rev_dummy", "universe_log", "tenant", "tourism_log")

fr$s_both <- c("FREH_log", "FREH_dummy", "rev_log", "rev_dummy", "universe_log", 
               "tenant", "tourism_log")

fr$s_vacancy <- c("FREH_log", "FREH_dummy", "rev_log", "rev_dummy", 
                  "universe_log", "tenant", "tourism_log", "vacancy")

fr$s_outliers <- c("FREH_log", "FREH_dummy", "rev_log", "universe_log", 
                   "tenant", "tourism_log")

fr$s_no_zero <- c("FREH_log", "rev_log", "universe_log", "tenant", 
                  "tourism_log")


# Linear models -----------------------------------------------------------

mr$l_FREH <- lm(fr$FREH, data = dr$main)
mr$l_rev <- lm(fr$rev, data = dr$main)
mr$l_both <- lm(fr$both, data = dr$main)
mr$l_vacancy <- lm(fr$vacancy, data = dr$vacancy)
mr$l_outliers <- lm(fr$outliers, data = dr$outliers)
mr$l_no_zero <- lm(fr$no_zero, data = dr$no_zero)
mr$l_impute <- lm(fr$both, data = dr$impute)
mr$l_impute_vacancy <- lm(fr$vacancy, data = dr$impute)
mr$l_alt <- lm(fr$both, data = dr$alt)
mr$l_housing <- lm(fr$both, data = dr$housing)

# Models by province
mr$l_p <- map(list(
  "British Columbia", c("Alberta", "Saskatchewan", "Manitoba"),
  "Ontario", "Quebec", c("New Brunswick", "Nova Scotia", "Prince Edward Island", 
                         "Newfoundland and Labrador")), 
  \(x) lm(fr$both, data = filter(dr$main, province %in% x)))

# Models by year
mr$l_year_FREH <- 
  map(2016:2022, \(x) lm(fr$year_FREH, data = filter(dr$main, year == x)))

mr$l_year_rev <- 
  map(2016:2022, \(x) lm(fr$year_rev, data = filter(dr$main, year == x)))

mr$l_year_both <- 
  map(2016:2022, \(x) lm(fr$year_both, data = filter(dr$main, year == x)))

mr$l_year_vacancy <- 
  map(2016:2022, \(x) lm(fr$year_vacancy, data = filter(dr$vacancy, year == x)))







er$year <- map(2016:2022, \(x) {
  dr$main |> 
    filter(year == x) |> 
    st_transform(4326) |> 
    st_set_agr("constant") |> 
    st_centroid() |> 
    st_coordinates() |> 
    meigen()}) |> 
  set_names(2016:2022)

er$year_vacancy <- map(2016:2022, \(x) {
  dr$vacancy |> 
    filter(year == x) |> 
    st_transform(4326) |> 
    st_set_agr("constant") |> 
    st_centroid() |> 
    st_coordinates() |> 
    meigen()}) |> 
  set_names(2016:2022)


# RE-ESF ------------------------------------------------------------------

mr$sf_FREH <- resf(
  y = dr$main$rent_log,
  x = st_drop_geometry(dr$main[fr$s_FREH]),
  meig = er$main,
  xgroup = st_drop_geometry(dr$main[c("CMA", "year")]))

mr$sf_rev <- resf(
  y = dr$main$rent_log,
  x = st_drop_geometry(dr$main[fr$s_rev]),
  meig = er$main,
  xgroup = st_drop_geometry(dr$main[c("CMA", "year")]))

mr$sf_both <- resf(
  y = dr$main$rent_log,
  x = st_drop_geometry(dr$main[fr$s_both]),
  meig = er$main,
  xgroup = st_drop_geometry(dr$main[c("CMA", "year")]))

mr$sf_vacancy <- resf(
  y = dr$vacancy$rent_log,
  x = st_drop_geometry(dr$vacancy[fr$s_vacancy]),
  meig = er$vacancy,
  xgroup = st_drop_geometry(dr$vacancy[c("CMA", "year")]))

mr$sf_outliers <- resf(
  y = dr$outliers$rent_log,
  x = st_drop_geometry(dr$outliers[fr$s_outliers]),
  meig = er$outliers,
  xgroup = st_drop_geometry(dr$outliers[c("CMA", "year")]))

mr$sf_no_zero <- resf(
  y = dr$no_zero$rent_log,
  x = st_drop_geometry(dr$no_zero[fr$s_no_zero]),
  meig = er$no_zero,
  xgroup = st_drop_geometry(dr$no_zero[c("CMA", "year")]))

mr$sf_impute <- resf(
  y = dr$impute$rent_log,
  x = st_drop_geometry(dr$impute[fr$s_both]),
  meig = er$impute,
  xgroup = st_drop_geometry(dr$impute[c("CMA", "year")]))

mr$sf_impute_vacancy <- resf(
  y = dr$impute$rent_log,
  x = st_drop_geometry(dr$impute[fr$s_vacancy]),
  meig = er$impute,
  xgroup = st_drop_geometry(dr$impute[c("CMA", "year")]))

mr$sf_alt <- resf(
  y = dr$alt$rent_log,
  x = st_drop_geometry(dr$alt[fr$s_both]),
  meig = er$alt,
  xgroup = st_drop_geometry(dr$alt[c("CMA", "year")]))

mr$sf_housing <- resf(
  y = dr$housing$rent_log,
  x = st_drop_geometry(dr$housing[fr$s_both]),
  meig = er$housing,
  xgroup = st_drop_geometry(dr$housing[c("CMA", "year")]))

mr$sf_year <- map(as.character(2016:2022), \(x) resf(
  y = dr$main$rent_log[dr$main$year == x],
  x = st_drop_geometry(dr$main[dr$main$year == x, fr$s_both]),
  meig = er$year[[x]],
  xgroup = st_drop_geometry(dr$main[dr$main$year == x, "CMA"])))

mr$sf_year_vacancy <- map(as.character(2016:2022), \(x) resf(
  y = dr$vacancy$rent_log[dr$vacancy$year == x],
  x = st_drop_geometry(dr$vacancy[dr$vacancy$year == x, fr$s_vacancy]),
  meig = er$year_vacancy[[x]],
  xgroup = st_drop_geometry(dr$vacancy[dr$vacancy$year == x, "CMA"])))


# S&NVC -------------------------------------------------------------------

mr$sn_FREH <- resf_vc(
  y = dr$main$rent_log,
  x = st_drop_geometry(dr$main[fr$s_FREH]),
  meig = er$main,
  xgroup = st_drop_geometry(dr$main[c("CMA", "year")]),
  x_nvc = TRUE)

mr$sn_rev <- resf_vc(
  y = dr$main$rent_log,
  x = st_drop_geometry(dr$main[fr$s_rev]),
  meig = er$main,
  xgroup = st_drop_geometry(dr$main[c("CMA", "year")]),
  x_nvc = TRUE)

mr$sn_both <- resf_vc(
  y = dr$main$rent_log,
  x = st_drop_geometry(dr$main[fr$s_both]),
  meig = er$main,
  xgroup = st_drop_geometry(dr$main[c("CMA", "year")]),
  x_nvc = TRUE)

mr$sn_vacancy <- resf_vc(
  y = dr$vacancy$rent_log,
  x = st_drop_geometry(dr$vacancy[fr$s_vacancy]),
  meig = er$vacancy,
  xgroup = st_drop_geometry(dr$vacancy[c("CMA", "year")]),
  x_nvc = TRUE)

mr$sn_outliers <- resf_vc(
  y = dr$outliers$rent_log,
  x = st_drop_geometry(dr$outliers[fr$s_outliers]),
  meig = er$outliers,
  xgroup = st_drop_geometry(dr$outliers[c("CMA", "year")]),
  x_nvc = TRUE)

mr$sn_no_zero <- resf_vc(
  y = dr$no_zero$rent_log,
  x = st_drop_geometry(dr$no_zero[fr$s_no_zero]),
  meig = er$no_zero,
  xgroup = st_drop_geometry(dr$no_zero[c("CMA", "year")]),
  x_nvc = TRUE)

mr$sn_impute <- resf_vc(
  y = dr$impute$rent_log,
  x = st_drop_geometry(dr$impute[fr$s_both]),
  meig = er$impute,
  xgroup = st_drop_geometry(dr$impute[c("CMA", "year")]),
  x_nvc = TRUE)

mr$sn_impute_vacancy <- resf_vc(
  y = dr$impute$rent_log,
  x = st_drop_geometry(dr$impute[fr$s_vacancy]),
  meig = er$impute,
  xgroup = st_drop_geometry(dr$impute[c("CMA", "year")]),
  x_nvc = TRUE)

mr$sn_alt <- resf_vc(
  y = dr$alt$rent_log,
  x = st_drop_geometry(dr$alt[fr$s_both]),
  meig = er$alt,
  xgroup = st_drop_geometry(dr$alt[c("CMA", "year")]),
  x_nvc = TRUE)

mr$sn_housing <- resf_vc(
  y = dr$housing$rent_log,
  x = st_drop_geometry(dr$housing[fr$s_both]),
  meig = er$housing,
  xgroup = st_drop_geometry(dr$housing[c("CMA", "year")]),
  x_nvc = TRUE)
