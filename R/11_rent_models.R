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
  non_FREH_lag_log + non_FREH_lag_dummy + price_lag_log + rev_price_lag_dummy + 
  universe_change + tourism_log + income_log + apart + CMA:year + id
fr$lm_FREH_m <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  non_FREH_lag_log + non_FREH_lag_dummy + CMA:year + id
fr$lm_FREH_v <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  non_FREH_lag_log + non_FREH_lag_dummy + price_lag_log + rev_price_lag_dummy + 
  universe_change + tourism_log + income_log + apart + vacancy + CMA:year + id

# FREH RESF
fr$resf_FREH <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                  "non_FREH_lag_log", "non_FREH_lag_dummy", "price_lag_log",
                  "rev_price_lag_dummy", "universe_change", "tourism_log", 
                  "income_log", "apart")
fr$resf_FREH_m <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                    "non_FREH_lag_log", "non_FREH_lag_dummy")
fr$resf_FREH_v <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                    "non_FREH_lag_log", "non_FREH_lag_dummy", "price_lag_log",
                    "rev_price_lag_dummy", "universe_change", "tourism_log", 
                    "income_log", "apart", "vacancy")

# FREH BRM
fr$brm_FREH <- rent_log ~ car(adj_mat, gr = id, type = "icar") + rent_lag_log +
  FREH_lag_log + FREH_lag_dummy + non_FREH_lag_log + non_FREH_lag_dummy + 
  price_lag_log + rev_price_lag_dummy + universe_change + tourism_log + income_log + 
  apart + (1 | id) + (1 | CMA:year)
fr$brm_FREH_m <- rent_log ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_lag_log + FREH_lag_dummy + non_FREH_lag_log + 
  non_FREH_lag_dummy + (1 | id) + (1 | CMA:year)
fr$brm_FREH_v <- rent_log ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_lag_log + FREH_lag_dummy + non_FREH_lag_log + 
  non_FREH_lag_dummy + price_lag_log + rev_price_lag_dummy + 
  universe_change + tourism_log + income_log + apart + vacancy + (1 | id) + 
  (1 | CMA:year)

# Rev LM
fr$lm_rev <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_log + rev_price_lag_dummy + price_lag_log + universe_change + 
  tourism_log + income_log + apart + CMA:year + id
fr$lm_rev_m <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_log + rev_price_lag_dummy + price_lag_log + CMA:year + id
fr$lm_rev_v <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  rev_lag_log + rev_price_lag_dummy + price_lag_log + universe_change + 
  tourism_log + income_log + apart + vacancy + CMA:year + id

# Rev RESF
fr$resf_rev <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                 "rev_lag_log", "rev_price_lag_dummy", "price_lag_log", 
                 "universe_change", "tourism_log", "income_log", "apart")
fr$resf_rev_m <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                   "rev_lag_log", "rev_price_lag_dummy", "price_lag_log")
fr$resf_rev_v <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                   "rev_lag_log", "rev_price_lag_dummy", "price_lag_log",
                   "universe_change", "tourism_log", "income_log", "apart",
                   "vacancy")

# Rev BRM
fr$brm_rev <- rent_log ~ car(adj_mat, gr = id, type = "icar") + rent_lag_log +
  FREH_lag_log + FREH_lag_dummy + rev_lag_log + rev_price_lag_dummy + price_lag_log + 
  universe_change + tourism_log + income_log + apart + (1 | id) + 
  (1 | CMA:year)
fr$brm_rev_m <- rent_log ~ car(adj_mat, gr = id, type = "icar") + rent_lag_log + 
  FREH_lag_log + FREH_lag_dummy + rev_lag_log + rev_price_lag_dummy + price_lag_log + 
  (1 | id) + (1 | CMA:year)
fr$brm_rev_v <- rent_log ~ car(adj_mat, gr = id, type = "icar") + rent_lag_log + 
  FREH_lag_log + FREH_lag_dummy + rev_lag_log + rev_price_lag_dummy + price_lag_log + 
  universe_change + tourism_log + income_log + apart + vacancy + (1 | id) + 
  (1 | CMA:year)

# Price LM
fr$lm_price <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  rev_price_lag_dummy + price_lag_log + universe_change + income_log + apart + 
  CMA:year + id
fr$lm_price_m <- rent_log ~ rent_lag_log + rev_price_lag_dummy + price_lag_log + 
  CMA:year + id
fr$lm_price_v <- rent_log ~ rent_lag_log + FREH_lag_log + FREH_lag_dummy + 
  rev_price_lag_dummy + price_lag_log + universe_change + income_log + apart + 
  vacancy + CMA:year + id

# Price RESF
fr$resf_price <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                   "rev_price_lag_dummy", "price_lag_log", "universe_change", 
                   "income_log", "apart")
fr$resf_price_m <- c("rent_lag_log", "rev_price_lag_dummy", "price_lag_log")
fr$resf_price_v <- c("rent_lag_log", "FREH_lag_log", "FREH_lag_dummy", 
                     "rev_price_lag_dummy", "price_lag_log", "universe_change", 
                     "income_log", "apart", "vacancy")

# Price BRM
fr$brm_price <- rent_log ~ car(adj_mat, gr = id, type = "icar") + rent_lag_log +
  FREH_lag_log + FREH_lag_dummy + rev_price_lag_dummy + price_lag_log + 
  universe_change + income_log + apart + (1 | id) + (1 | CMA:year)
fr$brm_price_m <- rent_log ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + rev_price_lag_dummy + price_lag_log + (1 | id) + (1 | CMA:year)
fr$brm_price_v <- rent_log ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_lag_log + FREH_lag_dummy + rev_price_lag_dummy + price_lag_log + 
  universe_change + income_log + apart + vacancy + (1 | id) + (1 | CMA:year)


# Linear models -----------------------------------------------------------

mr$l_FREH <- lm(fr$lm_FREH, data = dr$main)
mr$l_FREH_m <- lm(fr$lm_FREH_m, data = dr$main)
mr$l_FREH_v <- lm(fr$lm_FREH_v, data = dr$vac)
mr$l_FREH_i <- lm(fr$lm_FREH, data = dr$imp)
mr$l_FREH_mi <- lm(fr$lm_FREH_m, data = dr$imp)
mr$l_FREH_vi <- lm(fr$lm_FREH_v, data = dr$imp)

mr$l_rev <- lm(fr$lm_rev, data = dr$main)
mr$l_rev_m <- lm(fr$lm_rev_m, data = dr$main)
mr$l_rev_v <- lm(fr$lm_rev_v, data = dr$vac)
mr$l_rev_i <- lm(fr$lm_rev, data = dr$imp)
mr$l_rev_mi <- lm(fr$lm_rev_m, data = dr$imp)
mr$l_rev_vi <- lm(fr$lm_rev_v, data = dr$imp)

mr$l_price <- lm(fr$lm_price, data = dr$main)
mr$l_price_m <- lm(fr$lm_price_m, data = dr$main)
mr$l_price_v <- lm(fr$lm_price_v, data = dr$vac)
mr$l_price_i <- lm(fr$lm_price, data = dr$imp)
mr$l_price_mi <- lm(fr$lm_price_m, data = dr$imp)
mr$l_price_vi <- lm(fr$lm_price_v, data = dr$imp)


# RE-ESF ------------------------------------------------------------------

# FREH
mr$sf_FREH <- resf(dr$main$rent_log, st_drop_geometry(dr$main[fr$resf_FREH]),
                   gr$main, meig = er$main)
mr$sf_FREH_m <- resf(dr$main$rent_log, 
                     st_drop_geometry(dr$main[fr$resf_FREH_m]), gr$main,
                     meig = er$main)
mr$sf_FREH_v <- resf(dr$vac$rent_log, st_drop_geometry(dr$vac[fr$resf_FREH_v]), 
                     gr$vac, meig = er$vac)
mr$sf_FREH_i <- resf(dr$imp$rent_log, st_drop_geometry(dr$imp[fr$resf_FREH]), 
                     gr$imp, meig = er$imp)
mr$sf_FREH_mi <- resf(dr$imp$rent_log, st_drop_geometry(dr$imp[fr$resf_FREH_m]),
                      gr$imp, meig = er$imp)
mr$sf_FREH_vi <- resf(dr$imp$rent_log, st_drop_geometry(dr$imp[fr$resf_FREH_v]),
                      gr$imp, meig = er$imp)

# Rev
mr$sf_rev <- resf(dr$main$rent_log, st_drop_geometry(dr$main[fr$resf_rev]),
                  gr$main, meig = er$main)
mr$sf_rev_m <- resf(dr$main$rent_log, st_drop_geometry(dr$main[fr$resf_rev_m]),
                    gr$main, meig = er$main)
mr$sf_rev_v <- resf(dr$vac$rent_log, st_drop_geometry(dr$vac[fr$resf_rev_v]),
                    gr$vac, meig = er$vac)
mr$sf_rev_i <- resf(dr$imp$rent_log, st_drop_geometry(dr$imp[fr$resf_rev]),
                    gr$imp, meig = er$imp)
mr$sf_rev_mi <- resf(dr$imp$rent_log, st_drop_geometry(dr$imp[fr$resf_rev_m]),
                     gr$imp, meig = er$imp)
mr$sf_rev_vi <- resf(dr$imp$rent_log, st_drop_geometry(dr$imp[fr$resf_rev_v]),
                     gr$imp, meig = er$imp)

# Price
mr$sf_price <- resf(dr$main$rent_log, st_drop_geometry(dr$main[fr$resf_price]),
                    gr$main, meig = er$main)
mr$sf_price_m <- resf(dr$main$rent_log, 
                      st_drop_geometry(dr$main[fr$resf_price_m]), gr$main,
                      meig = er$main)
mr$sf_price_v <- resf(dr$vac$rent_log, 
                      st_drop_geometry(dr$vac[fr$resf_price_v]), gr$vac,
                      meig = er$vac)
mr$sf_price_i <- resf(dr$imp$rent_log, st_drop_geometry(dr$imp[fr$resf_price]), 
                      gr$imp, meig = er$imp)
mr$sf_price_mi <- resf(dr$imp$rent_log, 
                       st_drop_geometry(dr$imp[fr$resf_price_m]), gr$imp,
                       meig = er$imp)
mr$sf_price_vi <- resf(dr$imp$rent_log, 
                       st_drop_geometry(dr$imp[fr$resf_price_v]), gr$imp,
                       meig = er$imp)


# S&NVC -------------------------------------------------------------------

# FREH
mr$sn_FREH <- resf_vc(dr$main$rent_log, st_drop_geometry(dr$main[fr$resf_FREH]),
                      x_nvc = TRUE, xgroup = gr$main, meig = er$main)
mr$sn_FREH_m <- resf_vc(dr$main$rent_log, 
                     st_drop_geometry(dr$main[fr$resf_FREH_m]), 
                     x_nvc = TRUE, xgroup = gr$main, meig = er$main)
mr$sn_FREH_v <- resf_vc(dr$vac$rent_log, 
                        st_drop_geometry(dr$vac[fr$resf_FREH_v]), 
                        x_nvc = TRUE, xgroup = gr$vac, meig = er$vac)
mr$sn_FREH_i <- resf_vc(dr$imp$rent_log, st_drop_geometry(dr$imp[fr$resf_FREH]), 
                        x_nvc = TRUE, xgroup = gr$imp, meig = er$imp)
mr$sn_FREH_mi <- resf_vc(dr$imp$rent_log, 
                         st_drop_geometry(dr$imp[fr$resf_FREH_m]),
                         x_nvc = TRUE, xgroup = gr$imp, meig = er$imp)
mr$sn_FREH_vi <- resf_vc(dr$imp$rent_log, 
                         st_drop_geometry(dr$imp[fr$resf_FREH_v]),
                         x_nvc = TRUE, xgroup = gr$imp, meig = er$imp)

# Rev
mr$sn_rev <- resf_vc(dr$main$rent_log, st_drop_geometry(dr$main[fr$resf_rev]),
                     x_nvc = TRUE, xgroup = gr$main, meig = er$main, 
                     # Set maxiter to avoid error in iteration 18
                     maxiter = 17)
mr$sn_rev_m <- resf_vc(dr$main$rent_log, 
                       st_drop_geometry(dr$main[fr$resf_rev_m]),
                       x_nvc = TRUE, xgroup = gr$main, meig = er$main)
mr$sn_rev_v <- resf_vc(dr$vac$rent_log, st_drop_geometry(dr$vac[fr$resf_rev_v]),
                       x_nvc = TRUE, xgroup = gr$vac, meig = er$vac)
mr$sn_rev_i <- resf_vc(dr$imp$rent_log, st_drop_geometry(dr$imp[fr$resf_rev]),
                       x_nvc = TRUE, xgroup = gr$imp, meig = er$imp)
mr$sn_rev_mi <- resf_vc(dr$imp$rent_log, 
                        st_drop_geometry(dr$imp[fr$resf_rev_m]),
                        x_nvc = TRUE, xgroup = gr$imp, meig = er$imp)
mr$sn_rev_vi <- resf_vc(dr$imp$rent_log, 
                        st_drop_geometry(dr$imp[fr$resf_rev_v]),
                        x_nvc = TRUE, xgroup = gr$imp, meig = er$imp)

# Price
mr$sn_price <- resf_vc(dr$main$rent_log, 
                       st_drop_geometry(dr$main[fr$resf_price]),
                       x_nvc = TRUE, xgroup = gr$main, meig = er$main)
mr$sn_price_m <- resf_vc(dr$main$rent_log, 
                      st_drop_geometry(dr$main[fr$resf_price_m]), 
                      x_nvc = TRUE, xgroup = gr$main, meig = er$main)
mr$sn_price_v <- resf_vc(dr$vac$rent_log, 
                      st_drop_geometry(dr$vac[fr$resf_price_v]), 
                      x_nvc = TRUE, xgroup = gr$vac, meig = er$vac)
mr$sn_price_i <- resf_vc(dr$imp$rent_log, 
                         st_drop_geometry(dr$imp[fr$resf_price]), 
                         x_nvc = TRUE, xgroup = gr$imp, meig = er$imp)
mr$sn_price_mi <- resf_vc(dr$imp$rent_log, 
                       st_drop_geometry(dr$imp[fr$resf_price_m]), 
                       x_nvc = TRUE, xgroup = gr$imp, meig = er$imp)
mr$sn_price_vi <- resf_vc(dr$imp$rent_log, 
                       st_drop_geometry(dr$imp[fr$resf_price_v]), 
                       x_nvc = TRUE, xgroup = gr$imp, meig = er$imp)


# Save partial output -----------------------------------------------------

qsave(mr, file = "output/mr.qs", nthreads = availableCores())


# Bayesian models ---------------------------------------------------------

pr <- list()

# FREH
pr$FREH <- get_prior(fr$brm_FREH, data = dr$main, 
                     data2 = list(adj_mat = ar$main))
pr$FREH$prior[c(4, 5, 7, 8, 9, 11, 12)] <- "normal(0, 1)"

pr$FREH_m <- get_prior(fr$brm_FREH_m, data = dr$main, 
                         data2 = list(adj_mat = ar$main))
pr$FREH_m$prior[c(3, 5, 6)] <- "normal(0, 1)"

pr$FREH_v <- get_prior(fr$brm_FREH_v, data = dr$vac, 
                       data2 = list(adj_mat = ar$vac))
pr$FREH_v$prior[c(4, 5, 7, 8, 9, 11, 12, 13)] <- "normal(0, 1)"

pr$FREH_i <- get_prior(fr$brm_FREH, data = dr$imp, 
                       data2 = list(adj_mat = ar$imp))
pr$FREH_i$prior[c(4, 5, 7, 8, 9, 11, 12)] <- "normal(0, 1)"

pr$FREH_mi <- get_prior(fr$brm_FREH_m, data = dr$imp, 
                         data2 = list(adj_mat = ar$imp))
pr$FREH_mi$prior[c(3, 5, 6)] <- "normal(0, 1)"

pr$FREH_vi <- get_prior(fr$brm_FREH_v, data = dr$imp, 
                        data2 = list(adj_mat = ar$imp))
pr$FREH_vac_imp$prior[c(4, 5, 7, 8, 9, 11, 12, 13)] <- "normal(0, 1)"

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
pr$FREH <- get_prior(fr$brm_FREH, data = dr$main, 
                     data2 = list(adj_mat = ar$main))
pr$FREH$prior[c(4, 5, 7, 8, 9, 11, 12)] <- "normal(0, 1)"

pr$FREH_m <- get_prior(fr$brm_FREH_m, data = dr$main, 
                       data2 = list(adj_mat = ar$main))
pr$FREH_m$prior[c(3, 5, 6)] <- "normal(0, 1)"

pr$FREH_v <- get_prior(fr$brm_FREH_v, data = dr$vac, 
                       data2 = list(adj_mat = ar$vac))
pr$FREH_v$prior[c(4, 5, 7, 8, 9, 11, 12, 13)] <- "normal(0, 1)"

pr$FREH_i <- get_prior(fr$brm_FREH, data = dr$imp, 
                       data2 = list(adj_mat = ar$imp))
pr$FREH_i$prior[c(4, 5, 7, 8, 9, 11, 12)] <- "normal(0, 1)"

pr$FREH_mi <- get_prior(fr$brm_FREH_m, data = dr$imp, 
                        data2 = list(adj_mat = ar$imp))
pr$FREH_mi$prior[c(3, 5, 6)] <- "normal(0, 1)"

pr$FREH_vi <- get_prior(fr$brm_FREH_v, data = dr$imp, 
                        data2 = list(adj_mat = ar$imp))
pr$FREH_vac_imp$prior[c(4, 5, 7, 8, 9, 11, 12, 13)] <- "normal(0, 1)"

# Parameters
chains <- 10
cores <- 10
iter <- 10000

# FREH
mr$b_FREH <- brm(fr$brm_FREH, data = dr$main, data2 = list(adj_mat = ar$main),
                 prior = pr$FREH, chains = chains, cores = cores, iter = iter)
mr$b_FREH_m <- brm(fr$brm_FREH_m, data = dr$main, 
                     data2 = list(adj_mat = ar$main), prior = pr$FREH_m, 
                     chains = chains, cores = cores, iter = iter)
mr$b_FREH_v <- brm(fr$brm_FREH_v, data = dr$vac, 
                     data2 = list(adj_mat = ar$vac), prior = pr$FREH_v, 
                     chains = chains, cores = cores, iter = iter)
mr$b_FREH_i <- brm(fr$brm_FREH, data = dr$imp, 
                     data2 = list(adj_mat = ar$imp), prior = pr$FREH, 
                     chains = chains, cores = cores, iter = iter)
mr$b_FREH_mi <- brm(fr$brm_FREH_m, data = dr$imp, 
                    data2 = list(adj_mat = ar$imp), prior = pr$FREH_m, 
                    chains = chains, cores = cores, iter = iter)
mr$b_FREH_vi <- brm(fr$brm_FREH_v, data = dr$imp, 
                    data2 = list(adj_mat = ar$imp), prior = pr$FREH_v, 
                    chains = chains, cores = cores, iter = iter)

# Rev
mr$b_rev <- brm(fr$brm_rev, data = dr$main, data2 = list(adj_mat = ar$main),
                prior = pr$rev, chains = chains, cores = cores, iter = iter)
mr$b_rev_m <- brm(fr$brm_rev_m, data = dr$main, 
                  data2 = list(adj_mat = ar$main), prior = pr$rev_m, 
                  chains = chains, cores = cores, iter = iter)
mr$b_rev_v <- brm(fr$brm_rev_v, data = dr$vac, 
                  data2 = list(adj_mat = ar$vac), prior = pr$rev_v, 
                  chains = chains, cores = cores, iter = iter)
mr$b_rev_i <- brm(fr$brm_rev, data = dr$imp, data2 = list(adj_mat = ar$imp), 
                  prior = pr$rev, chains = chains, cores = cores, iter = iter)
mr$b_rev_mi <- brm(fr$brm_rev_m, data = dr$imp, data2 = list(adj_mat = ar$main),
                   prior = pr$rev_m, chains = chains, cores = cores, 
                   iter = iter)
mr$b_rev_vi <- brm(fr$brm_rev_v, data = dr$imp, data2 = list(adj_mat = ar$imp), 
                   prior = pr$rev_v, chains = chains, cores = cores, 
                   iter = iter)

# Price
mr$b_price <- brm(fr$brm_price, data = dr$main, data2 = list(adj_mat = ar$main), 
                  prior = pr$price, chains = chains, cores = cores, iter = iter)
mr$b_price_m <- brm(fr$brm_price_m, data = dr$main, 
                    data2 = list(adj_mat = ar$main), prior = pr$price_m, 
                    chains = chains, cores = cores, iter = iter)
mr$b_price_v <- brm(fr$brm_price_v, data = dr$vac, 
                    data2 = list(adj_mat = ar$vac), prior = pr$price_v, 
                    chains = chains, cores = cores, iter = iter)
mr$b_price_i <- brm(fr$brm_price, data = dr$imp, data2 = list(adj_mat = ar$imp), 
                    prior = pr$price, chains = chains, cores = cores, 
                    iter = iter)
mr$b_price_mi <- brm(fr$brm_price_m, data = dr$imp, 
                     data2 = list(adj_mat = ar$imp), prior = pr$price_m, 
                     chains = chains, cores = cores, iter = iter)
mr$b_price_vi <- brm(fr$brm_price_v, data = dr$imp, 
                     data2 = list(adj_mat = ar$imp), prior = pr$price_v, 
                     chains = chains, cores = cores, iter = iter)
