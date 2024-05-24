#### RENT CHANGE MODELS ########################################################

source("R/09_data_for_models.R")

mc <- list()


# Prepare eigenvectors/adjacency matrices for spatial regressions ---------

ec <- map(dc, \(x) {
  x |> 
    st_transform(4326) |> 
    st_set_agr("constant") |> 
    st_centroid() |> 
    st_coordinates() |> 
    meigen(s_id = x$id)
})

ac <- map(dc, \(x) {
  
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

gc <- map(dc, \(x) bind_cols(id = x$id, CMA_year = paste0(x$CMA, x$year)))



# Formulas ----------------------------------------------------------------

fc <- list()

# FREH_change LM
fc$lm_FREH <- rent_change ~ rent_lag_log + FREH_change + non_FREH_change + 
  price_change + universe_change + tourism_log + income_log + apart + CMA:year + 
  id
fc$lm_FREH_m <- rent_change ~ rent_lag_log + FREH_change + non_FREH_change + 
  price_change + CMA:year + id
fc$lm_FREH_v <- rent_change ~ rent_lag_log + FREH_change + non_FREH_change + 
  price_change + universe_change + tourism_log + income_log + apart + vacancy + 
  CMA:year + id

# FREH_change RESF
fc$resf_FREH <- c("rent_lag_log", "FREH_change", "non_FREH_change", 
                  "price_change", "universe_change", "tourism_log", 
                  "income_log", "apart")
fc$resf_FREH_m <- c("rent_lag_log", "FREH_change", "non_FREH_change", 
                    "price_change")
fc$resf_FREH_v <- c("rent_lag_log", "FREH_change", "non_FREH_change", 
                    "price_change", "universe_change", "tourism_log", 
                    "income_log", "apart", "vacancy")

# FREH_change BRM
fc$brm_FREH <- rent_change ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_change + non_FREH_change + price_change + 
  universe_change + tourism_log + income_log + apart + (1 | id) + (1 | CMA:year)
fc$brm_FREH_m <- rent_change ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_change + non_FREH_change + price_change + (1 | id) + 
  (1 | CMA:year)
fc$brm_FREH_v <- rent_change ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_change + non_FREH_change + price_change + 
  universe_change + tourism_log + income_log + apart + vacancy + (1 | id) + 
  (1 | CMA:year)

# Rev_change LM
fc$lm_rev <- rent_change ~ rent_lag_log + FREH_change + rev_change + 
  price_change + universe_change + tourism_log + income_log + apart + CMA:year + 
  id
fc$lm_rev_m <- rent_change ~ rent_lag_log + FREH_change + rev_change + 
  price_change + CMA:year + id
fc$lm_rev_v <- rent_change ~ rent_lag_log + FREH_change + rev_change + 
  price_change + universe_change + tourism_log + income_log + apart + vacancy + 
  CMA:year + id

# Rev_change RESF
fc$resf_rev <- c("rent_lag_log", "FREH_change", "rev_change", "price_change",
                 "universe_change", "tourism_log", "income_log", "apart")
fc$resf_rev_m <- c("rent_lag_log", "FREH_change", "rev_change", "price_change")
fc$resf_rev_v <- c("rent_lag_log", "FREH_change", "rev_change", "price_change",
                   "universe_change", "tourism_log", "income_log", "apart",
                   "vacancy")

# Rev_change BRM
fc$brm_rev <- rent_change ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_change + rev_change + price_change + 
  universe_change + tourism_log + income_log + apart + (1 | id) + (1 | CMA:year)
fc$brm_rev_m <- rent_change ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_change + rev_change + price_change + (1 | id) + 
  (1 | CMA:year)
fc$brm_rev_v <- rent_change ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_change + rev_change + price_change + 
  universe_change + tourism_log + income_log + apart + vacancy + 
  (1 | id) + (1 | CMA:year)

# Price_change LM
fc$lm_price <- rent_change ~ rent_lag_log + FREH_change + price_change + 
  universe_change + income_log + apart + CMA:year + id
fc$lm_price_m <- rent_change ~ FREH_change + price_change + CMA:year + id
fc$lm_price_v <- rent_change ~ rent_lag_log + FREH_change + price_change + 
  universe_change + income_log + apart + vacancy + CMA:year + id

# Price_change RESF
fc$resf_price <- c("rent_lag_log", "FREH_change", "price_change", 
                   "universe_change", "income_log", "apart")
fc$resf_price_m <- c("FREH_change", "price_change")
fc$resf_price_v <- c("rent_lag_log", "FREH_change", "price_change", 
                     "universe_change", "income_log", "apart", "vacancy")

# Price_change BRM
fc$brm_price <- rent_change ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_change + price_change + universe_change + income_log + 
  apart + (1 | id) + (1 | CMA:year)
fc$brm_price_m <- rent_change ~ car(adj_mat, gr = id, type = "icar") + 
  FREH_change + price_change + (1 | id) + (1 | CMA:year)
fc$brm_price_v <- rent_change ~ car(adj_mat, gr = id, type = "icar") + 
  rent_lag_log + FREH_change + price_change + universe_change + income_log + 
  apart + vacancy (1 | id) + (1 | CMA:year)


# Linear models -----------------------------------------------------------

mc$l_FREH <- lm(fc$lm_FREH, data = dc$main)
mc$l_FREH_m <- lm(fc$lm_FREH_m, data = dc$main)
mc$l_FREH_v <- lm(fc$lm_FREH_v, data = dc$vac)
mc$l_FREH_i <- lm(fc$lm_FREH, data = dc$imp)
mc$l_FREH_mi <- lm(fc$lm_FREH_m, data = dc$imp)
mc$l_FREH_vi <- lm(fc$lm_FREH_v, data = dc$imp)

mc$l_rev <- lm(fc$lm_rev, data = dc$main)
mc$l_rev_m <- lm(fc$lm_rev_m, data = dc$main)
mc$l_rev_v <- lm(fc$lm_rev_v, data = dc$vac)
mc$l_rev_i <- lm(fc$lm_rev, data = dc$imp)
mc$l_rev_mi <- lm(fc$lm_rev_m, data = dc$imp)
mc$l_rev_vi <- lm(fc$lm_rev_v, data = dc$imp)

mc$l_price <- lm(fc$lm_price, data = dc$main)
mc$l_price_m <- lm(fc$lm_price_m, data = dc$main)
mc$l_price_v <- lm(fc$lm_price_v, data = dc$vac)
mc$l_price_i <- lm(fc$lm_price, data = dc$imp)
mc$l_price_mi <- lm(fc$lm_price_m, data = dc$imp)
mc$l_price_vi <- lm(fc$lm_price_v, data = dc$imp)


# RE-ESF ------------------------------------------------------------------

# FREH
mc$sf_FREH <- resf(dc$main$rent_change, st_drop_geometry(dc$main[fc$resf_FREH]),
                   gc$main, meig = ec$main)
mc$sf_FREH_m <- resf(dc$main$rent_change, 
                     st_drop_geometry(dc$main[fc$resf_FREH_m]), gc$main,
                     meig = ec$main)
mc$sf_FREH_v <- resf(dc$vac$rent_change, 
                     st_drop_geometry(dc$vac[fc$resf_FREH_v]), gc$vac, 
                     meig = ec$vac)
mc$sf_FREH_i <- resf(dc$imp$rent_change, st_drop_geometry(dc$imp[fc$resf_FREH]), 
                     gc$imp, meig = ec$imp)
mc$sf_FREH_mi <- resf(dc$imp$rent_change, 
                      st_drop_geometry(dc$imp[fc$resf_FREH_m]), gc$imp, 
                      meig = ec$imp)
mc$sf_FREH_vi <- resf(dc$imp$rent_change, 
                      st_drop_geometry(dc$imp[fc$resf_FREH_v]), gc$imp, 
                      meig = ec$imp)

# Rev
mc$sf_rev <- resf(dc$main$rent_change, st_drop_geometry(dc$main[fc$resf_rev]),
                  gc$main, meig = ec$main)
mc$sf_rev_m <- resf(dc$main$rent_change, 
                    st_drop_geometry(dc$main[fc$resf_rev_m]), gc$main, 
                    meig = ec$main)
mc$sf_rev_v <- resf(dc$vac$rent_change, st_drop_geometry(dc$vac[fc$resf_rev_v]),
                    gc$vac, meig = ec$vac)
mc$sf_rev_i <- resf(dc$imp$rent_change, st_drop_geometry(dc$imp[fc$resf_rev]),
                    gc$imp, meig = ec$imp)
mc$sf_rev_mi <- resf(dc$imp$rent_change, 
                     st_drop_geometry(dc$imp[fc$resf_rev_m]), gc$imp, 
                     meig = ec$imp)
mc$sf_rev_vi <- resf(dc$imp$rent_change, 
                     st_drop_geometry(dc$imp[fc$resf_rev_v]), gc$imp, 
                     meig = ec$imp)

# Price
mc$sf_price <- resf(dc$main$rent_change, 
                    st_drop_geometry(dc$main[fc$resf_price]), gc$main, 
                    meig = ec$main)
mc$sf_price_m <- resf(dc$main$rent_change, 
                      st_drop_geometry(dc$main[fc$resf_price_m]), gc$main,
                      meig = ec$main)
mc$sf_price_v <- resf(dc$vac$rent_change, 
                      st_drop_geometry(dc$vac[fc$resf_price_v]), gc$vac,
                      meig = ec$vac)
mc$sf_price_i <- resf(dc$imp$rent_change, 
                      st_drop_geometry(dc$imp[fc$resf_price]), gc$imp, 
                      meig = ec$imp)
mc$sf_price_mi <- resf(dc$imp$rent_change, 
                       st_drop_geometry(dc$imp[fc$resf_price_m]), gc$imp,
                       meig = ec$imp)
mc$sf_price_vi <- resf(dc$imp$rent_change, 
                       st_drop_geometry(dc$imp[fc$resf_price_v]), gc$imp,
                       meig = ec$imp)


# S&NVC -------------------------------------------------------------------

# FREH
mc$sn_FREH <- resf_vc(dc$main$rent_change, 
                      st_drop_geometry(dc$main[fc$resf_FREH]), 
                      x_nvc = TRUE, xgroup = gc$main, meig = ec$main)
mc$sn_FREH_m <- resf_vc(dc$main$rent_change, 
                        st_drop_geometry(dc$main[fc$resf_FREH_m]), 
                        x_nvc = TRUE, xgroup = gc$main, meig = ec$main)
mc$sn_FREH_v <- resf_vc(dc$vac$rent_change, 
                        st_drop_geometry(dc$vac[fc$resf_FREH_v]), 
                        x_nvc = TRUE, xgroup = gc$vac, meig = ec$vac)
mc$sn_FREH_i <- resf_vc(dc$imp$rent_change, 
                        st_drop_geometry(dc$imp[fc$resf_FREH]), 
                        x_nvc = TRUE, xgroup = gc$imp, meig = ec$imp)
mc$sn_FREH_mi <- resf_vc(dc$imp$rent_change, 
                         st_drop_geometry(dc$imp[fc$resf_FREH_m]),
                         x_nvc = TRUE, xgroup = gc$imp, meig = ec$imp)
mc$sn_FREH_vi <- resf_vc(dc$imp$rent_change, 
                         st_drop_geometry(dc$imp[fc$resf_FREH_v]),
                         x_nvc = TRUE, xgroup = gc$imp, meig = ec$imp)

# Rev
mc$sn_rev <- resf_vc(dc$main$rent_change, 
                     st_drop_geometry(dc$main[fc$resf_rev]),
                     x_nvc = TRUE, xgroup = gc$main, meig = ec$main)
mc$sn_rev_m <- resf_vc(dc$main$rent_change, 
                       st_drop_geometry(dc$main[fc$resf_rev_m]),
                       x_nvc = TRUE, xgroup = gc$main, meig = ec$main)
mc$sn_rev_v <- resf_vc(dc$vac$rent_change, 
                       st_drop_geometry(dc$vac[fc$resf_rev_v]),
                       x_nvc = TRUE, xgroup = gc$vac, meig = ec$vac)
mc$sn_rev_i <- resf_vc(dc$imp$rent_change, 
                       st_drop_geometry(dc$imp[fc$resf_rev]),
                       x_nvc = TRUE, xgroup = gc$imp, meig = ec$imp)
mc$sn_rev_mi <- resf_vc(dc$imp$rent_change, 
                        st_drop_geometry(dc$imp[fc$resf_rev_m]),
                        x_nvc = TRUE, xgroup = gc$imp, meig = ec$imp)
mc$sn_rev_vi <- resf_vc(dc$imp$rent_change, 
                        st_drop_geometry(dc$imp[fc$resf_rev_v]),
                        x_nvc = TRUE, xgroup = gc$imp, meig = ec$imp)

# Price
mc$sn_price <- resf_vc(dc$main$rent_change, 
                       st_drop_geometry(dc$main[fc$resf_price]),
                       x_nvc = TRUE, xgroup = gc$main, meig = ec$main)
mc$sn_price_m <- resf_vc(dc$main$rent_change, 
                         st_drop_geometry(dc$main[fc$resf_price_m]), 
                         x_nvc = TRUE, xgroup = gc$main, meig = ec$main)
mc$sn_price_v <- resf_vc(dc$vac$rent_change, 
                         st_drop_geometry(dc$vac[fc$resf_price_v]), 
                         x_nvc = TRUE, xgroup = gc$vac, meig = ec$vac)
mc$sn_price_i <- resf_vc(dc$imp$rent_change, 
                         st_drop_geometry(dc$imp[fc$resf_price]), 
                         x_nvc = TRUE, xgroup = gc$imp, meig = ec$imp)
mc$sn_price_mi <- resf_vc(dc$imp$rent_change, 
                          st_drop_geometry(dc$imp[fc$resf_price_m]), 
                          x_nvc = TRUE, xgroup = gc$imp, meig = ec$imp)
mc$sn_price_vi <- resf_vc(dc$imp$rent_change, 
                          st_drop_geometry(dc$imp[fc$resf_price_v]), 
                          x_nvc = TRUE, xgroup = gc$imp, meig = ec$imp)


# Save partial output -----------------------------------------------------

qsave(mc, file = "output/mc.qs", nthreads = availableCores())

