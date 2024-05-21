#### 11 RENT MODELS ############################################################

source("R/09_data_for_models.R")

mr <- list()


# Prepare eigenvectors/adjacency matrices for spatial regressions ---------

er <- map(dr, \(x) {
  x |> 
    st_transform(4326) |> 
    st_set_agr("constant") |> 
    st_centroid() |> 
    st_coordinates() |> 
    meigen(s_id = x$id)
})

ar <- map(dr, \(x) {
  
  adj_mat <- 
    x |> 
    slice(1, .by = id) |> 
    select(id) |> 
    poly2nb() |> 
    nb2mat(style = "B", zero.policy = TRUE)
  
  rownames(adj_mat) <- 
    x |> 
    slice(1, .by = id) |> 
    pull(id)
  
  colnames(adj_mat) <- 
    x |> 
    slice(1, .by = id) |> 
    pull(id)
  
  adj_mat
  
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

fr <- list()

fr$lm_FREH <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  non_FREH_lag_log + non_FREH_lag_dummy + price_lag_log + rev_lag_dummy + 
  universe_change + tourism_log + income_log + apart + CMA:year + id

fr$lm_FREH_min <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  non_FREH_lag_log + non_FREH_lag_dummy + CMA:year + id

fr$lm_FREH_vac <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  non_FREH_lag_log + non_FREH_lag_dummy + price_lag_log + rev_lag_dummy + 
  universe_change + tourism_log + income_log + apart + vacancy + CMA:year + id

fr$resf_FREH <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                  "non_FREH_lag_log", "non_FREH_lag_dummy", "price_lag_log",
                  "rev_lag_dummy", "universe_change", "tourism_log", 
                  "income_log", "apart")

fr$resf_FREH_min <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                      "non_FREH_lag_log", "non_FREH_lag_dummy")

fr$resf_FREH_vac <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                      "non_FREH_lag_log", "non_FREH_lag_dummy", "price_lag_log",
                      "rev_lag_dummy", "universe_change", "tourism_log", 
                      "income_log", "apart", "vacancy")

fr$brm_FREH <- rent_log ~ ar(gr = id) + car(adj_mat, gr = id, type = "icar") + 
  FREH_lag_log + FREH_lag_dummy + non_FREH_lag_log + non_FREH_lag_dummy + 
  price_lag_log + rev_lag_dummy + universe_change + tourism_log + income_log + 
  apart + (1 | id) + (1 | CMA:year)

fr$brm_FREH_min <- rent_log ~ ar(gr = id) + 
  car(adj_mat, gr = id, type = "icar") + FREH_lag_log + FREH_lag_dummy + 
  non_FREH_lag_log + non_FREH_lag_dummy + (1 | id) + (1 | CMA:year)

fr$brm_FREH_vac <- rent_log ~ ar(gr = id) + 
  car(adj_mat, gr = id, type = "icar") + FREH_lag_log + FREH_lag_dummy + 
  non_FREH_lag_log + non_FREH_lag_dummy + price_lag_log + rev_lag_dummy + 
  universe_change + tourism_log + income_log + apart + vacancy + (1 | id) + 
  (1 | CMA:year)


# Linear models -----------------------------------------------------------

mr$l_FREH <- lm(fr$lm_FREH, data = dr$main)
mr$l_FREH_min <- lm(fr$lm_FREH_min, data = dr$main)
mr$l_FREH_vac <- lm(fr$lm_FREH_vac, data = dr$vacancy)
mr$l_FREH_imp <- lm(fr$lm_FREH, data = dr$impute)
mr$l_FREH_min_imp <- lm(fr$lm_FREH_min, data = dr$impute)
mr$l_FREH_vac_imp <- lm(fr$lm_FREH_vac, data = dr$impute)

# # Models by province
# mr$l_p <- map(list(
#   "British Columbia", c("Alberta", "Saskatchewan", "Manitoba"),
#   "Ontario", "Quebec", c("New Brunswick", "Nova Scotia", "Prince Edward Island", 
#                          "Newfoundland and Labrador")), 
#   \(x) lm(fr$both, data = filter(dr$main, province %in% x)))
# 
# # Models by year
# mr$l_year_FREH <- 
#   map(2016:2022, \(x) lm(fr$year_FREH, data = filter(dr$main, year == x)))
# 
# mr$l_year_rev <- 
#   map(2016:2022, \(x) lm(fr$year_rev, data = filter(dr$main, year == x)))
# 
# mr$l_year_both <- 
#   map(2016:2022, \(x) lm(fr$year_both, data = filter(dr$main, year == x)))
# 
# mr$l_year_vacancy <- 
#   map(2016:2022, \(x) lm(fr$year_vacancy, data = filter(dr$vacancy, year == x)))


# RE-ESF ------------------------------------------------------------------

mr$sf_FREH <- resf(
  y = dr$main$rent_log,
  x = st_drop_geometry(dr$main[fr$resf_FREH]),
  meig = er$main,
  xgroup = bind_cols(id = dr$main$id, 
                     CMA_year = paste0(dr$main$CMA, dr$main$year)))

mr$sf_FREH_min <- resf(
  y = dr$main$rent_log,
  x = st_drop_geometry(dr$main[fr$resf_FREH_min]),
  meig = er$main,
  xgroup = bind_cols(id = dr$main$id, 
                     CMA_year = paste0(dr$main$CMA, dr$main$year)))

mr$sf_FREH_vac <- resf(
  y = dr$vacancy$rent_log,
  x = st_drop_geometry(dr$vacancy[fr$resf_FREH_vac]),
  meig = er$vacancy,
  xgroup = bind_cols(id = dr$vacancy$id, 
                     CMA_year = paste0(dr$vacancy$CMA, dr$vacancy$year)))

mr$sf_FREH_imp <- resf(
  y = dr$impute$rent_log,
  x = st_drop_geometry(dr$impute[fr$resf_FREH]),
  meig = er$impute,
  xgroup = bind_cols(id = dr$impute$id, 
                     CMA_year = paste0(dr$impute$CMA, dr$impute$year)))

mr$sf_FREH_min_imp <- resf(
  y = dr$impute$rent_log,
  x = st_drop_geometry(dr$impute[fr$resf_FREH_min]),
  meig = er$impute,
  xgroup = bind_cols(id = dr$impute$id, 
                     CMA_year = paste0(dr$impute$CMA, dr$impute$year)))

mr$sf_FREH_vac_imp <- resf(
  y = dr$impute$rent_log,
  x = st_drop_geometry(dr$impute[fr$resf_FREH_vac]),
  meig = er$impute,
  xgroup = bind_cols(id = dr$impute$id, 
                     CMA_year = paste0(dr$impute$CMA, dr$impute$year)))


# S&NVC -------------------------------------------------------------------

mr$sn_FREH <- resf_vc(
  y = dr$main$rent_log,
  x = st_drop_geometry(dr$main[fr$resf_FREH]),
  meig = er$main,
  xgroup = bind_cols(id = dr$main$id, 
                     CMA_year = paste0(dr$main$CMA, dr$main$year)),
  x_nvc = TRUE)

mr$sn_FREH_min <- resf_vc(
  y = dr$main$rent_log,
  x = st_drop_geometry(dr$main[fr$resf_FREH_min]),
  meig = er$main,
  xgroup = bind_cols(id = dr$main$id, 
                     CMA_year = paste0(dr$main$CMA, dr$main$year)),
  x_nvc = TRUE)

mr$sn_FREH_vac <- resf_vc(
  y = dr$vacancy$rent_log,
  x = st_drop_geometry(dr$vacancy[fr$resf_FREH_vac]),
  meig = er$vacancy,
  xgroup = bind_cols(id = dr$vacancy$id, 
                     CMA_year = paste0(dr$vacancy$CMA, dr$vacancy$year)),
  x_nvc = TRUE)

mr$sn_FREH_imp <- resf_vc(
  y = dr$impute$rent_log,
  x = st_drop_geometry(dr$impute[fr$resf_FREH]),
  meig = er$impute,
  xgroup = bind_cols(id = dr$impute$id, 
                     CMA_year = paste0(dr$impute$CMA, dr$impute$year)),
  x_nvc = TRUE)

mr$sn_FREH_min_imp <- resf_vc(
  y = dr$impute$rent_log,
  x = st_drop_geometry(dr$impute[fr$resf_FREH_min]),
  meig = er$impute,
  xgroup = bind_cols(id = dr$impute$id, 
                     CMA_year = paste0(dr$impute$CMA, dr$impute$year)),
  x_nvc = TRUE)

mr$sn_FREH_vac_imp <- resf_vc(
  y = dr$impute$rent_log,
  x = st_drop_geometry(dr$impute[fr$resf_FREH_vac]),
  meig = er$impute,
  xgroup = bind_cols(id = dr$impute$id, 
                     CMA_year = paste0(dr$impute$CMA, dr$impute$year)),
  x_nvc = TRUE)


# Bayesian models ---------------------------------------------------------

pr <- list()

pr$FREH <- get_prior(fr$brm_FREH, data = dr$main, 
                     data2 = list(adj_mat = ar$main))
pr$FREH$prior[c(5, 6, 8, 9, 11, 12)] <- "normal(0, 1)"

pr$FREH_min <- get_prior(fr$brm_FREH_min, data = dr$main, 
                         data2 = list(adj_mat = ar$main))
pr$FREH_min$prior[c(4, 6)] <- "normal(0, 1)"

pr$FREH_vac <- get_prior(fr$brm_FREH_vac, data = dr$vacancy, 
                     data2 = list(adj_mat = ar$vacancy))
pr$FREH_vac$prior[c(5, 6, 8, 9, 11, 12, 13)] <- "normal(0, 1)"

chains <- 10
cores <- 10
iter <- 10000

mr$b_FREH <- brm(fr$lm_FREH, data = dr$main, prior = pr$FREH, 
                 chains = chains, cores = cores, iter = iter)
mr$b_FREH_min <- brm(fr$lm_FREH_min, data = dr$main, prior = pr$FREH_min, 
                     chains = chains, cores = cores, iter = iter)
mr$b_FREH_vac <- brm(fr$lm_FREH_vac, data = dr$vacancy, prior = pr$FREH_vac, 
                     chains = chains, cores = cores, iter = iter)
mr$b_FREH_imp <- brm(fr$lm_FREH, data = dr$impute, prior = pr$FREH, 
                     chains = chains, cores = cores, iter = iter)
mr$b_FREH_min_imp <- brm(fr$lm_FREH_min, data = dr$impute, prior = pr$FREH_min, 
                         chains = chains, cores = cores, iter = iter)
mr$b_FREH_vac_imp <- brm(fr$lm_FREH_vac, data = dr$impute, prior = pr$FREH_vac, 
                         chains = chains, cores = cores, iter = iter)
