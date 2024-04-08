#### RENT MODEL OUTPUTS ########################################################

source("R/08_rent_models.R")
DA_union <- qread("output/DA_union.qs", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())

# Get largest six CMAs to map residuals for
largest_CMAs <- 
  dr$main |> 
  st_drop_geometry() |> 
  count(CMA, sort = TRUE) |> 
  slice(1:6) |> 
  pull(CMA)


### Model summaries ############################################################

# Table 2: OLS and RE-ESF models ------------------------------------------

# OLS models
modelsummary(list(FREH = mr$l_FREH, rev = mr$l_rev, Both = mr$l_both), 
             coef_map = c("(Intercept)", "FREH_log", "FREH_dummyTRUE", 
                          "rev_log", "rev_dummyTRUE", "universe_log", "tenant", 
                          "tourism_log"), stars = TRUE,
             add_rows = tibble(x = "Fixed effects",
                               y1 = "CMA by year",
                               y2 = "CMA by year",
                               y3 = "CMA by year") |> 
               structure(position = 15), 
             output = "kableExtra")

# RE-ESF model
mr$sf_both


# Table 3: S&NVC model ----------------------------------------------------

mr$sn_both


### Spatial effects ############################################################

# Figure 5: Map of S&NVC coefficient --------------------------------------

fig_5_top <- 
  map(list(
    c("British Columbia", "Alberta"), 
    c("Ontario", "Quebec")), \(x) {
      
      dr$main |> 
        mutate(effect = mr$sn_both$b_vc$FREH_log) |> 
        select(id:province, effect) |> 
        filter(province %in% x) |>
        filter(year == 2022) |> 
        ggplot(aes(fill = effect)) +
        geom_sf(data = filter(province, province %in% x), 
                colour = "white", fill = "grey80") +
        geom_sf(colour = "transparent") +
        scale_fill_fermenter(name = "FREH_log", type = "div", 
                             limits = c(-0.2, 0.2), oob = scales::squish, 
                             n.breaks = 9) +
        coord_sf(xlim = st_bbox(filter(dr$main, province %in% x))[c(1, 3)],
                 ylim = st_bbox(filter(dr$main, province %in% x))[c(2, 4)]) +
        ggtitle(paste(x, collapse = " and ")) +
        theme_void() +
        theme(text = element_text(family = "Futura"), 
              legend.position = "bottom",
              legend.key.width = unit(40, "points"))
    }) |> 
  wrap_plots() +
  plot_layout(nrow = 1, guides = "collect") &
  theme(plot.margin = margin(5, 5, 5, 5), legend.position = "bottom")

fig_5_bottom <-
  map(largest_CMAs, \(x) {
    
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
    
    bbox <- st_bbox(filter(dr$main, CMA == x, year == 2022))
    
    prov <- filter(DA_union, province == prov_name)
    
    dr$main |> 
      mutate(effect = mr$sn_both$b_vc$FREH_log) |> 
      filter(CMA == x, year == 2022) |> 
      ggplot(aes(fill = effect)) +
      geom_sf(data = prov, fill = "grey", colour = "transparent", lwd = 0.1) +
      geom_sf(colour = "white") +
      scale_fill_viridis_b(name = "FREH_log", n.breaks = 5) +
      ggtitle(name) +
      coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
               ylim = c(bbox["ymin"], bbox["ymax"])) +
      theme_void() +
      theme(plot.margin = margin(10, 10, 10, 10),
            legend.position = "bottom",
            legend.key.width = unit(20, "points"),
            text = element_text(family = "Futura"))
  })

fig_5 <- fig_5_top / patchwork::wrap_plots(fig_5_bottom) + plot_layout(nrow = 2)

ggsave("output/figure_5.png", fig_5, width = 8, height = 11, units = "in")


### Effects by year ############################################################

# Table 4: Effects by year ------------------------------------------------

modelsummary(set_names(mr$l_year_FREH, 2016:2022), 
             coef_map = "FREH_log", stars = TRUE,
             output = "kableExtra")

modelsummary(set_names(mr$l_year_rev, 2016:2022), 
             coef_map = "rev_log", stars = TRUE,
             output = "kableExtra")

modelsummary(set_names(mr$l_year_both, 2016:2022), 
             coef_map = c("FREH_log", "rev_log"), stars = TRUE,
             output = "kableExtra")

map(mr$sf_year, \(x) {
  x$b |> 
    select(Estimate, SE, p_value) |> 
    slice(c(2, 4))
})


# Figure 6: Effects by year -----------------------------------------------

fig_6_1 <- map(mr$l_year_both, \(x) {
  y <- summary(x)$coefficients
  tibble(
    FREH_log = y[["FREH_log", "Estimate"]],
    rev_log = y[["rev_log", "Estimate"]],
    F_SE = y[["FREH_log", "Std. Error"]],
    R_SE = y[["rev_log", "Std. Error"]])}) |> 
  bind_rows() |> 
  mutate(year = 2016:2022, 
         model = "FREH_log and rev_log together (model iii)", 
         .before = FREH_log) |> 
  pivot_longer(c(FREH_log, rev_log), names_to = "var") |> 
  mutate(down = value - 1.96 * if_else(var == "FREH_log", F_SE, R_SE),
         up = value + 1.96 * if_else(var == "FREH_log", F_SE, R_SE)) |> 
  select(-F_SE, -R_SE)

fig_6_2 <- map(mr$l_year_FREH, \(x) {
  y <- summary(x)$coefficients
  tibble(
    FREH_log = y[["FREH_log", "Estimate"]],
    F_SE = y[["FREH_log", "Std. Error"]])}) |> 
  bind_rows() |> 
  mutate(year = 2016:2022, 
         model = "FREH_log and rev_log separate (models i & ii)", 
         .before = FREH_log) |> 
  pivot_longer(c(FREH_log), names_to = "var") |> 
  mutate(down = value - 1.96 * F_SE, up = value + 1.96 * F_SE) |> 
  select(-F_SE)

fig_6_3 <- map(mr$l_year_rev, \(x) {
  y <- summary(x)$coefficients
  tibble(
    rev_log = y[["rev_log", "Estimate"]],
    R_SE = y[["rev_log", "Std. Error"]])}) |> 
  bind_rows() |> 
  mutate(year = 2016:2022, 
         model = "FREH_log and rev_log separate (models i & ii)", 
         .before = rev_log) |> 
  pivot_longer(c(rev_log), names_to = "var") |> 
  mutate(down = value - 1.96 * R_SE, up = value + 1.96 * R_SE) |> 
  select(-R_SE)

fig_6_4 <- map(mr$sf_year, \(x) {
  y <- x$b
  tibble(
    FREH_log = y[["FREH_log", "Estimate"]],
    rev_log = y[["rev_log", "Estimate"]],
    F_SE = y[["FREH_log", "SE"]],
    R_SE = y[["rev_log", "SE"]])}) |> 
  bind_rows() |> 
  mutate(year = 2016:2022, 
         model = "FREH_log and rev_log together (model iv)", 
         .before = FREH_log) |> 
  pivot_longer(c(FREH_log, rev_log), names_to = "var") |> 
  mutate(down = value - 1.96 * if_else(var == "FREH_log", F_SE, R_SE),
         up = value + 1.96 * if_else(var == "FREH_log", F_SE, R_SE)) |> 
  select(-F_SE, -R_SE)

fig_6 <- 
  bind_rows(fig_6_1, fig_6_2, fig_6_3, fig_6_4) |> 
  ggplot(aes(year, value, colour = var)) +
  geom_smooth(method = "lm", se = FALSE, lwd = 0.3, linetype = "dashed") +
  geom_pointrange((aes(ymin = down, ymax = up)), position = position_jitter(
    width = 0.1, height = 0, seed = 6)) +
  scale_color_brewer(name = NULL, palette = "Dark2") +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Estimate") +
  facet_wrap(~model, nrow = 3) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom")

ggsave("output/figure_6.png", fig_6, width = 8, height = 7, units = "in")
