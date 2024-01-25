#### SPATIAL REGRESSIONS #######################################################

library(spdep)
library(spatialreg)
library(spmoran)
library(pspatreg)

msr <- list()

# Pseudo-R2 helper
pseudo_r2 <- function(model, time, type) {
  if (type == "rent") {
    1 - (model$SSE / (var((filter(data_rent, year == time))$rent) * 
                        (length((filter(data_rent, year == time))$rent) - 1)))    
  } else if (type == "change") {
    1 - (model$SSE / (var((filter(data_change, year == time))$rent_change_pct) * 
                        (length((filter(data_change, year == time)
                                 )$rent_change_pct) - 1)))  
  }
} 


## Spatial regressions #########################################################

# Prepare eigenvectors ----------------------------------------------------

meig_rent <- 
  data_rent |> 
  st_transform(4326) |> 
  st_centroid() |> 
  st_coordinates() |> 
  meigen(s_id = data_rent$id)

meig_change <- 
  data_change |> 
  st_transform(4326) |> 
  st_centroid() |> 
  st_coordinates() |> 
  meigen(s_id = data_change$id)


# rent models -------------------------------------------------------------

msr$rent_FREH <- resf(
  y = data_rent$log_rent,
  x = st_drop_geometry(data_rent[, c(
    "log_FREH_share", "log_universe", "tenant_share", "log_pop_CMA", 
    "log_accommodation")]),
  meig = meig_rent,
  xgroup = st_drop_geometry(data_rent[,c("id", "year")]))

msr$rent_rev <- resf(
  y = data_rent$log_rent,
  x = st_drop_geometry(data_rent[, c(
    "log_rev_share", "log_universe", "tenant_share", "log_pop_CMA", 
    "log_accommodation")]),
  meig = meig_rent,
  xgroup = st_drop_geometry(data_rent[,c("id", "year")]))

msr$rent_both <- resf(
  y = data_rent$log_rent,
  x = st_drop_geometry(data_rent[, c(
    "log_FREH_share", "log_rev_share", "log_universe", "tenant_share", 
    "log_pop_CMA", "log_accommodation")]),
  meig = meig_rent,
  xgroup = st_drop_geometry(data_rent[,c("id", "year")]))

msr$rent_FREH
msr$rent_rev
msr$rent_both


# rent change models ------------------------------------------------------

msr$change_FREH <- resf(
  y = data_change$rent_change_pct,
  x = st_drop_geometry(data_change[, c(
    "FREH_change_pct", "FREH_share", "log_universe", "tenant_share", 
    "log_pop_CMA", "log_accommodation")]),
  meig = meig_change,
  xgroup = st_drop_geometry(data_change[,c("id", "year")]))

msr$change_rev <- resf(
  y = data_change$rent_change_pct,
  x = st_drop_geometry(data_change[, c(
    "rev_change_pct", "rev_share", "log_universe", "tenant_share", 
    "log_pop_CMA", "log_accommodation")]),
  meig = meig_change,
  xgroup = st_drop_geometry(data_change[,c("id", "year")]))

msr$change_both <- resf(
  y = data_change$rent_change_pct,
  x = st_drop_geometry(data_change[, c(
    "FREH_change_pct", "FREH_share", "rev_change_pct", "rev_share", 
    "log_universe", "tenant_share", "log_pop_CMA", "log_accommodation")]),
  meig = meig_change,
  xgroup = st_drop_geometry(data_change[,c("id", "year")]))

msr$change_FREH
msr$change_rev
msr$change_both


## Spatial panel models ########################################################

# Prepare data ------------------------------------------------------------

data_rent_panel <- 
  data_rent |> 
  filter(year != 2016) |> 
  filter(n() == 6, .by = id)

data_change_panel <-
  data_change |> 
  filter(n() == 6, .by = id)

data_rent_panel_nb <- poly2nb(filter(data_rent_panel, year == 2022))
data_change_panel_nb <- poly2nb(filter(data_change_panel, year == 2022))
data_rent_panel_lw <- nb2listw(data_rent_panel_nb, zero.policy = TRUE)
data_change_panel_lw <- nb2listw(data_change_panel_nb, zero.policy = TRUE)


# Spatial lag models ------------------------------------------------------

msl <- list()

msl$rent_FREH <- pspatfit(
  log_rent ~ log_FREH_share + log_universe + tenant_share + log_accommodation, 
  data = data_rent_panel, 
  listw = data_rent_panel_lw,
  demean = TRUE,
  type = "sar",
  index = c("id", "year"))

msl$rent_rev <- pspatfit(
  log_rent ~ log_rev_share + log_universe + tenant_share + log_accommodation, 
  data = data_rent_panel, 
  listw = data_rent_panel_lw,
  demean = TRUE,
  type = "sar",
  index = c("id", "year"))

msl$rent_both <- pspatfit(
  log_rent ~ log_FREH_share + log_rev_share + log_universe + tenant_share + 
    log_accommodation, 
  data = data_rent_panel, 
  listw = data_rent_panel_lw,
  demean = TRUE,
  type = "sar",
  index = c("id", "year"))

msl$change_FREH <- pspatfit(
  rent_change_pct ~ FREH_change_pct + FREH_share + log_universe + tenant_share + 
    log_accommodation, 
  data = data_change_panel, 
  listw = data_change_panel_lw,
  demean = TRUE,
  type = "sar",
  index = c("id", "year"))

msl$change_rev <- pspatfit(
  rent_change_pct ~ rev_change_pct + rev_share + log_universe + tenant_share + 
    log_accommodation, 
  data = data_change_panel, 
  listw = data_change_panel_lw,
  demean = TRUE,
  type = "sar",
  index = c("id", "year"))

msl$change_both <- pspatfit(
  rent_change_pct ~ FREH_change_pct + FREH_share + rev_change_pct + 
    rev_share + log_universe + tenant_share + log_accommodation, 
  data = data_change_panel, 
  listw = data_change_panel_lw,
  demean = TRUE,
  type = "sar",
  index = c("id", "year"))

map(msl, summary)


# Spatial error models ----------------------------------------------------

mse <- list()

mse$rent_FREH <- pspatfit(
  log_rent ~ log_FREH_share + log_universe + tenant_share + log_accommodation, 
  data = data_rent_panel, 
  listw = data_rent_panel_lw,
  demean = TRUE,
  type = "sem",
  index = c("id", "year"))

mse$rent_rev <- pspatfit(
  log_rent ~ log_rev_share + log_universe + tenant_share + log_accommodation, 
  data = data_rent_panel, 
  listw = data_rent_panel_lw,
  demean = TRUE,
  type = "sem",
  index = c("id", "year"))

mse$rent_both <- pspatfit(
  log_rent ~ log_FREH_share + log_rev_share + log_universe + tenant_share + 
    log_accommodation, 
  data = data_rent_panel, 
  listw = data_rent_panel_lw,
  demean = TRUE,
  type = "sem",
  index = c("id", "year"))

mse$change_FREH <- pspatfit(
  rent_change_pct ~ FREH_change_pct + FREH_share + log_universe + tenant_share + 
    log_accommodation, 
  data = data_change_panel, 
  listw = data_change_panel_lw,
  demean = TRUE,
  type = "sem",
  index = c("id", "year"))

mse$change_rev <- pspatfit(
  rent_change_pct ~ rev_change_pct + rev_share + log_universe + tenant_share + 
    log_accommodation, 
  data = data_change_panel, 
  listw = data_change_panel_lw,
  demean = TRUE,
  type = "sem",
  index = c("id", "year"))

mse$change_both <- pspatfit(
  rent_change_pct ~ FREH_change_pct + FREH_share + rev_change_pct + 
    rev_share + log_universe + tenant_share + log_accommodation, 
  data = data_change_panel, 
  listw = data_change_panel_lw,
  demean = TRUE,
  type = "sem",
  index = c("id", "year"))

map(mse, summary)


## Spatial Durbin models #######################################################

# Create weights ----------------------------------------------------------

data_rent_nb <- map(2016:2022, \(x) poly2nb(filter(data_rent, year == x)))
data_change_nb <- map(2017:2022, \(x) poly2nb(filter(data_change, year == x)))
data_rent_lw <- map(data_rent_nb, \(x) nb2listw(x, zero.policy = TRUE))
data_change_lw <- map(data_change_nb, \(x) nb2listw(x, zero.policy = TRUE))

msd <- list()


# rent models -------------------------------------------------------------

msd$rent_FREH <- map2(2016:2022, data_rent_lw, \(x, y) lagsarlm(
  log_rent ~ log_FREH_share + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation, data = filter(data_rent, year == x), listw = y, 
  zero.policy = TRUE))
  
msd$rent_rev <- map2(2016:2022, data_rent_lw, \(x, y) lagsarlm(
  log_rent ~ log_rev_share + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation, data = filter(data_rent, year == x), listw = y, 
  zero.policy = TRUE))

msd$rent_both <- map2(2016:2022, data_rent_lw, \(x, y) lagsarlm(
  log_rent ~ log_FREH_share + log_rev_share + log_universe + tenant_share + 
    log_pop_CMA + log_accommodation, data = filter(data_rent, year == x), 
  listw = y, zero.policy = TRUE))

map(msd$rent_FREH, summary)
map(msd$rent_rev, summary)
map(msd$rent_both, summary)

# Pseudo R2
map2(msd$rent_FREH, 2016:2022, \(x, y) pseudo_r2(x, y, "rent"))
map2(msd$rent_rev, 2016:2022, \(x, y) pseudo_r2(x, y, "rent"))
map2(msd$rent_both, 2016:2022, \(x, y) pseudo_r2(x, y, "rent"))


# rent_change_pct models --------------------------------------------------

msd$change_FREH <- map2(2017:2022, data_change_lw, \(x, y) lagsarlm(
  rent_change_pct ~ FREH_change_pct + FREH_share + log_universe + tenant_share + 
    log_pop_CMA + log_accommodation, data = filter(data_change, year == x), 
  listw = y, zero.policy = TRUE))

msd$change_rev <- map2(2017:2022, data_change_lw, \(x, y) lagsarlm(
  rent_change_pct ~ rev_change_pct + rev_share + log_universe + tenant_share + 
    log_pop_CMA + log_accommodation, data = filter(data_change, year == x), 
  listw = y, zero.policy = TRUE))

msd$change_both <- map2(2017:2022, data_change_lw, \(x, y) lagsarlm(
  rent_change_pct ~ FREH_change_pct + rev_change_pct + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation, 
  data = filter(data_change, year == x), listw = y, zero.policy = TRUE))

map(msd$change_FREH, summary)
map(msd$change_rev, summary)
map(msd$change_both, summary)

# Pseudo R2
map2(msd$change_FREH, 2017:2022, \(x, y) pseudo_r2(x, y, "change"))
map2(msd$change_rev, 2017:2022, \(x, y) pseudo_r2(x, y, "change"))
map2(msd$change_both, 2017:2022, \(x, y) pseudo_r2(x, y, "change"))
