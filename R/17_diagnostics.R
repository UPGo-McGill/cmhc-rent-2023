#### APPENDIX: DIAGNOSTICS AND ROBUSTNESS CHECKS ###############################

source("R/08_rent_models.R")
source("R/10_rent_change_models.R")
DA_union <- qread("output/DA_union.qs", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())
water <- qread("output/water.qs", nthreads = availableCores())

largest_CMAs <- 
  dr$main |> 
  st_drop_geometry() |> 
  count(CMA, sort = TRUE) |> 
  slice(1:6) |> 
  pull(CMA)


### rent_log diagnostics #######################################################

# OLS models --------------------------------------------------------------

# QQ plot good, but with low rent predictions for high-rent cases.
# Residuals-fitted plot suggests some heteroskedasticity.
# No issues with leverage.
fig_A1 <-
  autoplot(mr$l_both, size = 0.5) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

fig_A1 <- 
  fig_A1@plots |> 
  wrap_plots()

ggsave("output/figure_A1.png", fig_A1, width = 8, height = 5, units = "in")


# So compute heteroskedasticity-robust standard errors
lmtest::coeftest(mr$l_both)

# VIF on simplified version of model: All values under 5
lm(rent_log ~ FREH_log + rev_log + universe_log + tenant + tourism_log + CMA + 
     year, data = dr$main) |> 
  car::vif()

# Moran's I: extreme values are clustered
lm.morantest(mr$l_both, nb2listw(poly2nb(dr$main), zero.policy = TRUE), 
             alternative = "two.sided")


# Spatial models ----------------------------------------------------------

# QQ plot good, but with low rent predictions for high-rent cases
qqnorm(mr$sf_both$resid)
qqline(mr$sf_both$resid)

# QQ plot good, but with low rent predictions for high-rent cases
qqnorm(mr$sn_both$resid)
qqline(mr$sn_both$resid)


# Figure A2: Map of residuals ---------------------------------------------

fig_A2_1 <- map(largest_CMAs, \(x) {
  
  name <- 
    dr$main |> 
    filter(CMA == x) |> 
    pull(name_CMA) |> 
    unique()
  
  prov_name <- 
    dr$main |> 
    filter(CMA == x) |> 
    pull(province) |> 
    unique()
  
  bbox <- st_bbox(filter(dr$main, CMA == x))
  
  prov <- filter(DA_union, province == prov_name)
  wat <- filter(water, province == prov_name)
  
  dr$main |> 
    mutate(.resid = residuals(mr$l_both)) |> 
    filter(CMA == x) |> 
    summarize(.resid = mean(.resid, na.rm = TRUE), 
              across(geometry, st_union),
              .by = id) |> 
    ggplot(aes(fill = .resid)) +
    geom_sf(data = prov, fill = "grey", colour = "transparent", lwd = 0.1) +
    geom_sf(colour = "white") +
    geom_sf(data = wat, colour = "transparent", fill = "white") +
    scale_fill_viridis_b(name = "Residual", limits = c(-0.5, 0.5), 
                         n.breaks = 7, oob = scales::squish) +
    ggtitle(name) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10),
          text = element_text(family = "Futura"))
  
})

fig_A2_2 <- map(largest_CMAs, \(x) {
  
  name <- 
    dr$main |> 
    filter(CMA == x) |> 
    pull(name_CMA) |> 
    unique()
  
  prov_name <- 
    dr$main |> 
    filter(CMA == x) |> 
    pull(province) |> 
    unique()
  
  bbox <- st_bbox(filter(dr$main, CMA == x))
  
  prov <- filter(DA_union, province == prov_name)
  wat <- filter(water, province == prov_name)
  
  dr$main |> 
    mutate(.resid = mr$sf_both$resid) |> 
    filter(CMA == x) |> 
    summarize(.resid = mean(.resid, na.rm = TRUE), 
              across(geometry, st_union),
              .by = id) |> 
    ggplot(aes(fill = .resid)) +
    geom_sf(data = prov, fill = "grey", colour = "transparent", lwd = 0.1) +
    geom_sf(colour = "white") +
    geom_sf(data = wat, colour = "transparent", fill = "white") +
    scale_fill_viridis_b(name = "Residual", limits = c(-0.5, 0.5), 
                         n.breaks = 7, oob = scales::squish) +
    ggtitle(name) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10),
          text = element_text(family = "Futura"))
  
})

fig_A2_3 <- map(largest_CMAs, \(x) {
  
  name <- 
    dr$main |> 
    filter(CMA == x) |> 
    pull(name_CMA) |> 
    unique()
  
  prov_name <- 
    dr$main |> 
    filter(CMA == x) |> 
    pull(province) |> 
    unique()
  
  bbox <- st_bbox(filter(dr$main, CMA == x))
  
  prov <- filter(DA_union, province == prov_name)
  wat <- filter(water, province == prov_name)
  
  dr$main |> 
    mutate(.resid = mr$sn_both$resid) |> 
    filter(CMA == x) |> 
    summarize(.resid = mean(.resid, na.rm = TRUE), 
              across(geometry, st_union),
              .by = id) |> 
    ggplot(aes(fill = .resid)) +
    geom_sf(data = prov, fill = "grey", colour = "transparent", lwd = 0.1) +
    geom_sf(colour = "white") +
    geom_sf(data = wat, colour = "transparent", fill = "white") +
    scale_fill_viridis_b(name = "Residual", limits = c(-0.5, 0.5), 
                         n.breaks = 7, oob = scales::squish) +
    ggtitle(name) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10),
          text = element_text(family = "Futura"))
  
})

fig_A2 <- wrap_plots(c(fig_A2_1, fig_A2_2, fig_A2_3), nrow = 6) + 
  plot_layout(guides = "collect") + plot_annotation(tag_levels = list(
    c("Model iii", rep("", 5), "Model v", rep("", 5), "Model vii", rep("", 5))
  )) & theme(plot.tag = element_text(face = "bold"), legend.position = "bottom")


ggsave("output/figure_A2.png", fig_A2, width = 10, height = 13, units = "in")


### rent_log robustness checks #################################################

# Alternative linear models
modelsummary(list(iii = mr$l_both, iv = mr$l_vacancy, xvii = mr$l_outliers, 
                  xviii = mr$l_no_zero, xix = mr$l_impute, 
                  xx = mr$l_impute_vacancy, xxi = mr$l_alt, 
                  xxii = mr$l_housing), 
             coef_map = c("(Intercept)", "FREH_log", "FREH_dummyTRUE", 
                          "rev_log", "rev_dummyTRUE", "universe_log", "vacancy",
                          "tenant", "tourism_log"), stars = TRUE,
             add_rows = tibble(x = "Fixed effects",
                               y1 = "CMA by year",
                               y2 = "CMA by year",
                               y3 = "CMA by year",
                               y4 = "CMA by year",
                               y5 = "CMA by year",
                               y6 = "CMA by year",
                               y7 = "CMA by year",
                               y8 = "CMA by year") |> 
               structure(position = 19), 
             output = "kableExtra")

# Provincial models
modelsummary(set_names(mr$l_p, c("BC", "Prairies", "Ontario", "Quebec",
                                 "Atlantic")),
             coef_map = c("FREH_log", "FREH_dummyTRUE", "rev_log", 
                          "rev_dummyTRUE", "tenant", "universe_log", 
                          "tourism_log"), stars = TRUE,
             add_rows = tibble(x = "Fixed effects",
                               y1 = "CMA by year",
                               y2 = "CMA by year",
                               y3 = "CMA by year",
                               y4 = "CMA by year",
                               y5 = "CMA by year") |> 
               structure(position = 15), output = "kableExtra")

# Alternative RE-ESF models
mr$sf_both
mr$sf_vacancy
mr$sf_outliers
mr$sf_no_zero
mr$sf_impute
mr$sf_impute_vacancy
mr$sf_alt
mr$sf_housing

# Alternative S&NVC models
mr$sn_both
mr$sn_vacancy
mr$sn_outliers
mr$sn_no_zero
mr$sn_impute
mr$sn_impute_vacancy
mr$sn_alt
mr$sn_housing


### rent_change diagnostics ####################################################

# OLS models --------------------------------------------------------------

# QQ plot good shows significant divergence for low and high values.
# Residuals-fitted plot suggests minimal heteroskedasticity.
# No issues with leverage.
fig_A3 <- 
  autoplot(mc$l_both, size = 0.5) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

fig_A3 <- 
  fig_A3@plots |> 
  wrap_plots()

ggsave("output/figure_A3.png", fig_A3, width = 8, height = 5, units = "in")

# VIF on simplified version of model: All values under 5
lm(rent_change ~ FREH_change + rev_change + universe_change + universe_log + 
     tenant + tourism_log + CMA + year, data = dc$main) |> 
  car::vif()

# Moran's I: values are dispersed
lm.morantest(mc$l_both, nb2listw(poly2nb(dc$main), zero.policy = TRUE), 
             alternative = "two.sided")


# Spatial models ----------------------------------------------------------

# QQ plot ok, but with low rent predictions for high-rent cases
qqnorm(mc$sf_both$resid)
qqline(mc$sf_both$resid)

# QQ plot ok, but with low rent predictions for high-rent cases
qqnorm(mc$sn_both$resid)
qqline(mc$sn_both$resid)



# Figure A4: Map of residuals ---------------------------------------------

fig_A4_1 <- map(largest_CMAs, \(x) {
  
  name <- 
    dc$main |> 
    filter(CMA == x) |> 
    pull(name_CMA) |> 
    unique()
  
  prov_name <- 
    dc$main |> 
    filter(CMA == x) |> 
    pull(province) |> 
    unique()
  
  bbox <- st_bbox(filter(dc$main, CMA == x))
  
  prov <- filter(DA_union, province == prov_name)
  wat <- filter(water, province == prov_name)
  
  dc$main |> 
    mutate(.resid = residuals(mc$l_both)) |> 
    filter(CMA == x) |> 
    summarize(.resid = mean(.resid, na.rm = TRUE), 
              across(geometry, st_union),
              .by = id) |> 
    ggplot(aes(fill = .resid)) +
    geom_sf(data = prov, fill = "grey", colour = "transparent", lwd = 0.1) +
    geom_sf(colour = "white") +
    geom_sf(data = wat, colour = "transparent", fill = "white") +
    scale_fill_viridis_b(name = "Residual", limits = c(-0.5, 0.5), 
                         n.breaks = 7, oob = scales::squish) +
    ggtitle(name) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10),
          text = element_text(family = "Futura"))
  
})

fig_A4_2 <- map(largest_CMAs, \(x) {
  
  name <- 
    dc$main |> 
    filter(CMA == x) |> 
    pull(name_CMA) |> 
    unique()
  
  prov_name <- 
    dc$main |> 
    filter(CMA == x) |> 
    pull(province) |> 
    unique()
  
  bbox <- st_bbox(filter(dc$main, CMA == x))
  
  prov <- filter(DA_union, province == prov_name)
  wat <- filter(water, province == prov_name)
  
  dc$main |> 
    mutate(.resid = mc$sf_both$resid) |> 
    filter(CMA == x) |> 
    summarize(.resid = mean(.resid, na.rm = TRUE), 
              across(geometry, st_union),
              .by = id) |> 
    ggplot(aes(fill = .resid)) +
    geom_sf(data = prov, fill = "grey", colour = "transparent", lwd = 0.1) +
    geom_sf(colour = "white") +
    geom_sf(data = wat, colour = "transparent", fill = "white") +
    scale_fill_viridis_b(name = "Residual", limits = c(-0.5, 0.5), 
                         n.breaks = 7, oob = scales::squish) +
    ggtitle(name) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10),
          text = element_text(family = "Futura"))
  
})

fig_A4_3 <- map(largest_CMAs, \(x) {
  
  name <- 
    dc$main |> 
    filter(CMA == x) |> 
    pull(name_CMA) |> 
    unique()
  
  prov_name <- 
    dc$main |> 
    filter(CMA == x) |> 
    pull(province) |> 
    unique()
  
  bbox <- st_bbox(filter(dc$main, CMA == x))
  
  prov <- filter(DA_union, province == prov_name)
  wat <- filter(water, province == prov_name)
  
  dc$main |> 
    mutate(.resid = mc$sn_both$resid) |> 
    filter(CMA == x) |> 
    summarize(.resid = mean(.resid, na.rm = TRUE), 
              across(geometry, st_union),
              .by = id) |> 
    ggplot(aes(fill = .resid)) +
    geom_sf(data = prov, fill = "grey", colour = "transparent", lwd = 0.1) +
    geom_sf(colour = "white") +
    geom_sf(data = wat, colour = "transparent", fill = "white") +
    scale_fill_viridis_b(name = "Residual", limits = c(-0.5, 0.5), 
                         n.breaks = 7, oob = scales::squish) +
    ggtitle(name) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10),
          text = element_text(family = "Futura"))
  
})

fig_A4 <- wrap_plots(c(fig_A4_1, fig_A4_2, fig_A4_3), nrow = 6) + 
  plot_layout(guides = "collect") + plot_annotation(tag_levels = list(
    c("Model xi", rep("", 5), "Model xiii", rep("", 5), "Model xv", rep("", 5))
  )) & theme(plot.tag = element_text(face = "bold"), legend.position = "bottom")


ggsave("output/figure_A4.png", fig_A4, width = 10, height = 13, units = "in")


# rent_change robustness checks -------------------------------------------

# Alternative linear models
modelsummary(list(xi = mc$l_both, xxii = mc$l_vacancy, xvii = mc$l_outliers, 
                  xxiv = mc$l_impute, xxv = mc$l_impute_vacancy, 
                  xxvi = mc$l_alt, xxvii = mc$l_housing), 
             coef_map = c("(Intercept)", "FREH_change", "rev_change",
                          "universe_change", "universe_log", "vacany_change",
                          "vacancy", "tenant", "tourism_log"), stars = TRUE,
             add_rows = tibble(x = "Fixed effects",
                               y1 = "CMA by year",
                               y2 = "CMA by year",
                               y3 = "CMA by year",
                               y4 = "CMA by year",
                               y5 = "CMA by year",
                               y6 = "CMA by year",
                               y7 = "CMA by year") |> 
               structure(position = 19), 
             output = "kableExtra")

# Alternative RE-ESF models
mc$sf_both
mc$sf_outliers
mc$sf_impute
mc$sf_alt
mc$sf_count
mc$sf_housing
mc$sf_lag

# Alternative S&NVC models
mc$sn_both
mc$sn_outliers
mc$sn_impute
mc$sn_alt
mc$sn_count
mc$sn_housing
mc$sn_lag



### DID


# Parallel trends assumption ----------------------------------------------

dd |> 
  filter(n() == 7, .by = id) |> 
  mutate(reg = if_else(treat == 2024, FALSE, reg),
         treat = if_else(treat == 2024, 0, treat)) |> 
  mutate(treated = reg & year >= treat) |> 
  filter(!treated, year <= 2022) |> 
  # summarize(rent_log = mean(rent_log), .by = c(treat, year)) |> 
  summarize(rent_log = mean(rent_log), .by = c(reg, year)) |> 
  # mutate(treat = as.character(treat)) |> 
  # ggplot(aes(year, rent_log, colour = treat)) +
  ggplot(aes(year, rent_log, colour = reg)) +
  geom_line()


