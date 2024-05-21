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
    poly2nb() |> 
    nb2mat(style = "B", zero.policy = TRUE)
  
  rownames(adj_mat) <- 
    x |> 
    slice(1, .by = id) |> 
    pull(id)
  
  colnames(adj_mat) <- rownames(adj_mat)
  
  adj_mat
  
})


# Prepare group effect tables ---------------------------------------------

gr <- map(dr, \(x) bind_cols(id = x$id, CMA_year = paste0(x$CMA, x$year)))


# Formulas ----------------------------------------------------------------

fr <- list()

# FREH LM
fr$lm_FREH <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  non_FREH_lag_log + non_FREH_lag_dummy + price_lag_log + rev_lag_dummy + 
  universe_change + tourism_log + income_log + apart + CMA:year + id

fr$lm_FREH_min <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  non_FREH_lag_log + non_FREH_lag_dummy + CMA:year + id

fr$lm_FREH_vac <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  non_FREH_lag_log + non_FREH_lag_dummy + price_lag_log + rev_lag_dummy + 
  universe_change + tourism_log + income_log + apart + vacancy + CMA:year + id

# FREH RESF
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

# FREH BRM
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

# Rev LM
fr$lm_rev <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_log + rev_lag_dummy + price_lag_log + universe_change + 
  tourism_log + income_log + apart + CMA:year + id

fr$lm_rev_min <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_log + rev_lag_dummy + price_lag_log + CMA:year + id

fr$lm_rev_vac <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_log + rev_lag_dummy + price_lag_log + universe_change + 
  tourism_log + income_log + apart + vacancy + CMA:year + id

# Rev RESF
fr$resf_rev <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                 "rev_lag_log", "rev_lag_dummy", "price_lag_log", 
                 "universe_change", "tourism_log", "income_log", "apart")

fr$resf_rev_min <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                     "rev_lag_log", "rev_lag_dummy", "price_lag_log")

fr$resf_rev_vac <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                     "rev_lag_log", "rev_lag_dummy", "price_lag_log",
                     "universe_change", "tourism_log", "income_log", "apart",
                     "vacancy")

# Rev BRM
fr$brm_rev <- rent_log ~ ar(gr = id) + car(adj_mat, gr = id, type = "icar") + 
  FREH_lag_log + FREH_lag_dummy + rev_lag_log + rev_lag_dummy + price_lag_log + 
  universe_change + tourism_log + income_log + apart + (1 | id) + 
  (1 | CMA:year)

fr$brm_rev_min <- rent_log ~ ar(gr = id) + 
  car(adj_mat, gr = id, type = "icar") + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_log + rev_lag_dummy + price_lag_log + (1 | id) + (1 | CMA:year)

fr$brm_rev_vac <- rent_log ~ ar(gr = id) + 
  car(adj_mat, gr = id, type = "icar") + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_log + rev_lag_dummy + price_lag_log + universe_change + tourism_log + 
  income_log + apart + vacancy + (1 | id) + (1 | CMA:year)

# Price LM
fr$lm_price <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_dummy + price_lag_log + universe_change + income_log + apart + 
  CMA:year + id

fr$lm_price_min <- rent_log ~ rent_lag_log + rev_lag_dummy + price_lag_log + 
  CMA:year + id

fr$lm_price_vac <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_dummy + price_lag_log + universe_change + income_log + apart + 
  vacancy + CMA:year + id

# Price RESF
fr$resf_price <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                   "rev_lag_dummy", "price_lag_log", "universe_change", 
                   "income_log", "apart")

fr$resf_price_min <- c("rent_lag_log", "rev_lag_dummy", "price_lag_log")

fr$resf_price_vac <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                       "rev_lag_dummy", "price_lag_log", "universe_change", 
                       "income_log", "apart", "vacancy")

# Price BRM
fr$brm_price <- rent_log ~ ar(gr = id) + car(adj_mat, gr = id, type = "icar") + 
  FREH_lag_log + FREH_lag_dummy + rev_lag_dummy + price_lag_log + 
  universe_change + income_log + apart + (1 | id) + (1 | CMA:year)

fr$brm_price_min <- rent_log ~ ar(gr = id) + 
  car(adj_mat, gr = id, type = "icar") + rev_lag_dummy + price_lag_log + 
  (1 | id) + (1 | CMA:year)

fr$brm_price_vac <- rent_log ~ ar(gr = id) + 
  car(adj_mat, gr = id, type = "icar") + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_dummy + price_lag_log + universe_change + income_log + apart + 
  vacancy + (1 | id) + (1 | CMA:year)


# Linear models -----------------------------------------------------------

mr$l_FREH <- lm(fr$lm_FREH, data = dr$main)
mr$l_FREH_min <- lm(fr$lm_FREH_min, data = dr$main)
mr$l_FREH_vac <- lm(fr$lm_FREH_vac, data = dr$vacancy)
mr$l_FREH_imp <- lm(fr$lm_FREH, data = dr$impute)
mr$l_FREH_min_imp <- lm(fr$lm_FREH_min, data = dr$impute)
mr$l_FREH_vac_imp <- lm(fr$lm_FREH_vac, data = dr$impute)

mr$l_rev <- lm(fr$lm_rev, data = dr$main)
mr$l_rev_min <- lm(fr$lm_rev_min, data = dr$main)
mr$l_rev_vac <- lm(fr$lm_rev_vac, data = dr$vacancy)
mr$l_rev_imp <- lm(fr$lm_rev, data = dr$impute)
mr$l_rev_min_imp <- lm(fr$lm_rev_min, data = dr$impute)
mr$l_rev_vac_imp <- lm(fr$lm_rev_vac, data = dr$impute)

mr$l_price <- lm(fr$lm_price, data = dr$main)
mr$l_price_min <- lm(fr$lm_price_min, data = dr$main)
mr$l_price_vac <- lm(fr$lm_price_vac, data = dr$vacancy)
mr$l_price_imp <- lm(fr$lm_price, data = dr$impute)
mr$l_price_min_imp <- lm(fr$lm_price_min, data = dr$impute)
mr$l_price_vac_imp <- lm(fr$lm_price_vac, data = dr$impute)


# # Models by province
# mr$l_p <- map(list(
#   "British Columbia", c("Alberta", "Saskatchewan", "Manitoba"),
#   "Ontario", "Quebec", c("New Brunswick", "Nova Scotia", 
#                          "Prince Edward Island", 
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
#   map(2016:2022, \(x) lm(fr$year_vacancy, 
#                          data = filter(dr$vacancy, year == x)))


# RE-ESF ------------------------------------------------------------------

# FREH
mr$sf_FREH <- resf(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_FREH]),
  meig = er$main, xgroup = gr$main)

mr$sf_FREH_min <- resf(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_FREH_min]),
  meig = er$main, xgroup = gr$main)

mr$sf_FREH_vac <- resf(
  y = dr$vacancy$rent_log, x = st_drop_geometry(dr$vacancy[fr$resf_FREH_vac]),
  meig = er$vacancy, xgroup = gr$vacancy)

mr$sf_FREH_imp <- resf(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_FREH]),
  meig = er$impute, xgroup = gr$impute)

mr$sf_FREH_min_imp <- resf(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_FREH_min]),
  meig = er$impute, xgroup = gr$impute)

mr$sf_FREH_vac_imp <- resf(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_FREH_vac]),
  meig = er$impute, xgroup = gr$impute)

# Rev
mr$sf_rev <- resf(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_rev]),
  meig = er$main, xgroup = gr$main)

mr$sf_rev_min <- resf(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_rev_min]),
  meig = er$main, xgroup = gr$main)

mr$sf_rev_vac <- resf(
  y = dr$vacancy$rent_log, x = st_drop_geometry(dr$vacancy[fr$resf_rev_vac]),
  meig = er$vacancy, xgroup = gr$vacancy)

mr$sf_rev_imp <- resf(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_rev]),
  meig = er$impute, xgroup = gr$impute)

mr$sf_rev_min_imp <- resf(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_rev_min]),
  meig = er$impute, xgroup = gr$impute)

mr$sf_rev_vac_imp <- resf(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_rev_vac]),
  meig = er$impute, xgroup = gr$impute)

# Price
mr$sf_price <- resf(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_price]),
  meig = er$main, xgroup = gr$main)

mr$sf_price_min <- resf(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_price_min]),
  meig = er$main, xgroup = gr$main)

mr$sf_price_vac <- resf(
  y = dr$vacancy$rent_log, x = st_drop_geometry(dr$vacancy[fr$resf_price_vac]),
  meig = er$vacancy, xgroup = gr$vacancy)

mr$sf_price_imp <- resf(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_price]),
  meig = er$impute, xgroup = gr$impute)

mr$sf_price_min_imp <- resf(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_price_min]),
  meig = er$impute, xgroup = gr$impute)

mr$sf_price_vac_imp <- resf(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_price_vac]),
  meig = er$impute, xgroup = gr$impute)


# S&NVC -------------------------------------------------------------------

# FREH
mr$sn_FREH <- resf_vc(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_FREH]),
  meig = er$main, xgroup = gr$main, x_nvc = TRUE)

mr$sn_FREH_min <- resf_vc(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_FREH_min]),
  meig = er$main, xgroup = gr$main, x_nvc = TRUE)

mr$sn_FREH_vac <- resf_vc(
  y = dr$vacancy$rent_log, x = st_drop_geometry(dr$vacancy[fr$resf_FREH_vac]),
  meig = er$vacancy, xgroup = gr$vacancy, x_nvc = TRUE)

mr$sn_FREH_imp <- resf_vc(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_FREH]),
  meig = er$impute, xgroup = gr$impute, x_nvc = TRUE)

mr$sn_FREH_min_imp <- resf_vc(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_FREH_min]),
  meig = er$impute, xgroup = gr$impute, x_nvc = TRUE)

mr$sn_FREH_vac_imp <- resf_vc(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_FREH_vac]),
  meig = er$impute, xgroup = gr$impute, x_nvc = TRUE)

# Rev
mr$sn_rev <- resf_vc(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_rev]),
  meig = er$main, xgroup = gr$main, x_nvc = TRUE)

mr$sn_rev_min <- resf_vc(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_rev_min]),
  meig = er$main, xgroup = gr$main, x_nvc = TRUE)

mr$sn_rev_vac <- resf_vc(
  y = dr$vacancy$rent_log, x = st_drop_geometry(dr$vacancy[fr$resf_rev_vac]),
  meig = er$vacancy, xgroup = gr$vacancy, x_nvc = TRUE)

mr$sn_rev_imp <- resf_vc(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_rev]),
  meig = er$impute, xgroup = gr$impute, x_nvc = TRUE)

mr$sn_rev_min_imp <- resf_vc(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_rev_min]),
  meig = er$impute, xgroup = gr$impute, x_nvc = TRUE)

mr$sn_rev_vac_imp <- resf_vc(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_rev_vac]),
  meig = er$impute, xgroup = gr$impute, x_nvc = TRUE)

# Price
mr$sn_price <- resf_vc(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_price]),
  meig = er$main, xgroup = gr$main, x_nvc = TRUE)

mr$sn_price_min <- resf_vc(
  y = dr$main$rent_log, x = st_drop_geometry(dr$main[fr$resf_price_min]),
  meig = er$main, xgroup = gr$main, x_nvc = TRUE)

mr$sn_price_vac <- resf_vc(
  y = dr$vacancy$rent_log, x = st_drop_geometry(dr$vacancy[fr$resf_price_vac]),
  meig = er$vacancy, xgroup = gr$vacancy, x_nvc = TRUE)

mr$sn_price_imp <- resf_vc(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_price]),
  meig = er$impute, xgroup = gr$impute, x_nvc = TRUE)

mr$sn_price_min_imp <- resf_vc(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_price_min]),
  meig = er$impute, xgroup = gr$impute, x_nvc = TRUE)

mr$sn_price_vac_imp <- resf_vc(
  y = dr$impute$rent_log, x = st_drop_geometry(dr$impute[fr$resf_price_vac]),
  meig = er$impute, xgroup = gr$impute, x_nvc = TRUE)


# Bayesian models ---------------------------------------------------------

pr <- list()

# FREH
pr$FREH <- get_prior(fr$brm_FREH, data = dr$main, 
                     data2 = list(adj_mat = ar$main))
pr$FREH$prior[c(5, 6, 8, 9, 11, 12)] <- "normal(0, 1)"

pr$FREH_min <- get_prior(fr$brm_FREH_min, data = dr$main, 
                         data2 = list(adj_mat = ar$main))
pr$FREH_min$prior[c(4, 6)] <- "normal(0, 1)"

pr$FREH_vac <- get_prior(fr$brm_FREH_vac, data = dr$vacancy, 
                     data2 = list(adj_mat = ar$vacancy))
pr$FREH_vac$prior[c(5, 6, 8, 9, 11, 12, 13)] <- "normal(0, 1)"

pr$FREH_imp <- get_prior(fr$brm_FREH, data = dr$impute, 
                     data2 = list(adj_mat = ar$impute))
pr$FREH_imp$prior[c(5, 6, 8, 9, 11, 12)] <- "normal(0, 1)"

pr$FREH_min_imp <- get_prior(fr$brm_FREH_min, data = dr$impute, 
                         data2 = list(adj_mat = ar$impute))
pr$FREH_min_imp$prior[c(4, 6)] <- "normal(0, 1)"

pr$FREH_vac_imp <- get_prior(fr$brm_FREH_vac, data = dr$impute, 
                         data2 = list(adj_mat = ar$impute))
pr$FREH_vac_imp$prior[c(5, 6, 8, 9, 11, 12, 13)] <- "normal(0, 1)"

# Rev
pr$rev <- get_prior(fr$brm_rev, data = dr$main, 
                     data2 = list(adj_mat = ar$main))
pr$rev$prior[c(5, 6, 7, 8, 9, 10, 11)] <- "normal(0, 1)"

pr$rev_min <- get_prior(fr$brm_rev_min, data = dr$main, 
                         data2 = list(adj_mat = ar$main))
pr$rev_min$prior[c(4, 5, 7)] <- "normal(0, 1)"

pr$rev_vac <- get_prior(fr$brm_rev_vac, data = dr$vacancy, 
                         data2 = list(adj_mat = ar$vacancy))
pr$rev_vac$prior[c(5, 6, 7, 9, 10, 11, 12)] <- "normal(0, 1)"

pr$rev_imp <- get_prior(fr$brm_rev, data = dr$impute, 
                         data2 = list(adj_mat = ar$impute))
pr$rev_imp$prior[c(5, 6, 7, 8, 9, 10, 11)] <- "normal(0, 1)"

pr$rev_min_imp <- get_prior(fr$brm_rev_min, data = dr$impute, 
                             data2 = list(adj_mat = ar$impute))
pr$rev_min_imp$prior[c(4, 5, 7)] <- "normal(0, 1)"

pr$rev_vac_imp <- get_prior(fr$brm_rev_vac, data = dr$impute, 
                             data2 = list(adj_mat = ar$impute))
pr$rev_vac_imp$prior[c(5, 6, 7, 8, 9, 10, 11)] <- "normal(0, 1)"

# Price


chains <- 10
cores <- 10
iter <- 10000

# FREH
mr$b_FREH <- brm(fr$brm_FREH, data = dr$main, data2 = list(adj_mat = ar$main),
                 prior = pr$FREH, chains = chains, cores = cores, iter = iter)
mr$b_FREH_min <- brm(fr$brm_FREH_min, data = dr$main, 
                     data2 = list(adj_mat = ar$main), prior = pr$FREH_min, 
                     chains = chains, cores = cores, iter = iter)
mr$b_FREH_vac <- brm(fr$brm_FREH_vac, data = dr$vacancy, 
                     data2 = list(adj_mat = ar$main), prior = pr$FREH_vac, 
                     chains = chains, cores = cores, iter = iter)
mr$b_FREH_imp <- brm(fr$brm_FREH, data = dr$impute, 
                     data2 = list(adj_mat = ar$main), prior = pr$FREH, 
                     chains = chains, cores = cores, iter = iter)
mr$b_FREH_min_imp <- brm(fr$brm_FREH_min, data = dr$impute, 
                         data2 = list(adj_mat = ar$main), prior = pr$FREH_min, 
                         chains = chains, cores = cores, iter = iter)
mr$b_FREH_vac_imp <- brm(fr$brm_FREH_vac, data = dr$impute, 
                         data2 = list(adj_mat = ar$main), prior = pr$FREH_vac, 
                         chains = chains, cores = cores, iter = iter)

# Rev
mr$b_rev <- brm(fr$brm_rev, data = dr$main, data2 = list(adj_mat = ar$main),
                prior = pr$rev, chains = chains, cores = cores, iter = iter)
mr$b_rev_min <- brm(fr$brm_rev_min, data = dr$main, 
                    data2 = list(adj_mat = ar$main), prior = pr$rev_min, 
                    chains = chains, cores = cores, iter = iter)
mr$b_rev_vac <- brm(fr$brm_rev_vac, data = dr$vacancy, 
                    data2 = list(adj_mat = ar$main), prior = pr$rev_vac, 
                    chains = chains, cores = cores, iter = iter)
mr$b_rev_imp <- brm(fr$brm_rev, data = dr$impute, 
                    data2 = list(adj_mat = ar$main), prior = pr$rev, 
                    chains = chains, cores = cores, iter = iter)
mr$b_rev_min_imp <- brm(fr$brm_rev_min, data = dr$impute, 
                        data2 = list(adj_mat = ar$main), prior = pr$rev_min, 
                        chains = chains, cores = cores, iter = iter)
mr$b_rev_vac_imp <- brm(fr$brm_rev_vac, data = dr$impute, 
                        data2 = list(adj_mat = ar$main), prior = pr$rev_vac, 
                        chains = chains, cores = cores, iter = iter)

# Price
mr$b_price <- brm(fr$brm_price, data = dr$main, 
                  data2 = list(adj_mat = ar$main), prior = pr$price, 
                  chains = chains, cores = cores, iter = iter)
mr$b_price_min <- brm(fr$brm_price_min, data = dr$main, 
                      data2 = list(adj_mat = ar$main), prior = pr$price_min, 
                      chains = chains, cores = cores, iter = iter)
mr$b_price_vac <- brm(fr$brm_price_vac, data = dr$vacancy, 
                      data2 = list(adj_mat = ar$main), prior = pr$price_vac, 
                      chains = chains, cores = cores, iter = iter)
mr$b_price_imp <- brm(fr$brm_price, data = dr$impute, 
                      data2 = list(adj_mat = ar$main), prior = pr$price, 
                      chains = chains, cores = cores, iter = iter)
mr$b_price_min_imp <- brm(fr$brm_price_min, data = dr$impute, 
                          data2 = list(adj_mat = ar$main), prior = pr$price_min, 
                          chains = chains, cores = cores, iter = iter)
mr$b_price_vac_imp <- brm(fr$brm_price_vac, data = dr$impute, 
                          data2 = list(adj_mat = ar$main), prior = pr$price_vac, 
                          chains = chains, cores = cores, iter = iter)
