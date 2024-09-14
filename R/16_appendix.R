#### 16 APPENDIX: DIAGNOSTICS AND ROBUSTNESS CHECKS ############################

source("R/06_data_for_models.R")
source("R/08_process_DAGs.R")
qload("output/cmhc.qsm", nthreads = availableCores())
source("R/05_imputation.R")
dd <- qread("output/dd.qs")
mc <- qread("output/mc.qs", nthreads = availableCores())
md <- qread("output/md.qs")
tc <- qread("output/tc.qs")
DA_union <- qread("output/DA_union.qs", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())
water <- qread("output/water.qs", nthreads = availableCores())

largest_CMAs <- 
  dc$main |> 
  st_drop_geometry() |> 
  count(CMA, sort = TRUE) |> 
  slice(1:6) |> 
  pull(CMA)


### DiD dataset ################################################################

# Load regulations
reg <-
  qread("data/reg.qs") |> 
  mutate(reg = if_else(reg == "TBD", FALSE, as.logical(reg))) |> 
  mutate(date = if_else(date >= "2023-01-02", NA, date)) |> 
  mutate(reg = if_else(is.na(date), FALSE, TRUE)) |> 
  inner_join(st_drop_geometry(cmhc_nbhd), by = c("id", "name")) |> 
  select(-c(pop:tenant))


# Table A1: STR regulations -----------------------------------------------

reg |> 
  filter(reg) |> 
  count(name_CSD, province, date) |> 
  select(-n) |> 
  gt::gt()

# Montreal borough details
reg |> 
  filter(reg) |> 
  filter(name_CSD == "Montréal")


### DiD robustness checks ######################################################

ad <- map(md, map, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .05))

# Provincial population
get_census("CA21", regions = list(C = 1), level = "PR") |> 
  mutate(in_df = GeoUID %in% c("13", "24", "35", "59")) |> 
  summarize(dwellings = sum(Dwellings), .by = in_df) |> 
  mutate(pct = dwellings / sum(dwellings))


# Table A2: ATT for all model variants ------------------------------------

map(names(ad), \(x) {
  tibble(
    model = x,
    var = names(ad[[x]]),
    att = round(map_dbl(ad[[x]], \(x) x$overall.att), 3),
    se = round(map_dbl(ad[[x]], \(x) x$overall.se), 3))}) |> 
  bind_rows() |> 
  mutate(att = paste0(att, "\n(", se, ")")) |> 
  select(-se) |> 
  pivot_wider(names_from = var, values_from = att) |> 
  filter(model != "no_2023") |> 
  gt::gt()

# Confidence intervals
map(md, map, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .001))
map(md, map, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .01))
ad
map(md, map, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .1))


### DiD diagnostics ############################################################

# Parallel trends assumption ----------------------------------------------

fig_A1_list <-
  map(setdiff(names(md), "no_2023"), \(x) {
    aggte(md[[x]]$rent_log, type = "dynamic") |> 
      ggdid() +
      ggtitle(x) +
      theme_minimal() +
      scale_x_continuous(name = "Years post-treatment", 
                         breaks = c(-6, -4, -2, 0, 2, 4)) + 
      scale_y_continuous(name = "ATT") +
      scale_color_brewer(name = NULL, palette = "Dark2", labels = c(
        "Pre-treatment", "Post-treatment")) +
      theme(text = element_text(family = "Futura"),
            legend.position = "bottom")
  })

fig_A1 <-
  wrap_plots(fig_A1_list, guides = "collect") & 
  theme(legend.position = "bottom")  

ggsave("output/figure_A1.png", fig_A1, width = 8, height = 8, units = "in")


### Structural causal model ####################################################

# Figure A2: DAGs ---------------------------------------------------------

fig_A2_1 <-
  hc$non_FREH |>
  tidy_dagitty() |>
  node_status() |>
  node_ancestors("non_FREH_change") |>
  select(everything(), non_FREH_anc = ancestor) |>
  node_ancestors("rent_change") |>
  mutate(status = case_when(
    status == "exposure" ~ "b",
    status == "outcome" ~ "a",
    status == "latent" ~ "f",
    name == "non_FREH_lag_log" ~ "c",
    non_FREH_anc == "ancestor" & ancestor == "ancestor" ~ "e",
    ancestor == "ancestor" ~ "d")) |>
  mutate(label = case_when(
    status == "a" ~ "Outcome",
    status == "b" ~ "Exposure",
    status == "c" ~ "Ancestor\n(treat.)",
    status == "d" ~ "Ancestor\n(outcome)",
    status == "e" ~ "Ancestor\n(both)",
    status == "f" ~ "Latent")) |>
  mutate(y = y * -1, yend = yend * -1) |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, colour = status)) +
  geom_dag_edges(edge_width = 0.2) +
  geom_dag_point(size = 16) +
  geom_label(aes(label = name), family = "Futura", size = 3, 
             show.legend = FALSE) +
  scale_colour_manual(name = NULL, values = c(
    a = "#1b9e77", b = "#d95f02", c = "#e6ab02", d = "#7570b3", e = "#e7298a",
    f = "grey50"),
    labels = c("Outcome", "Treatment", "Ancestor of treatment",
               "Ancestor of outcome", "Ancestor of both", "Latent")) +
  guides(colour = guide_legend(nrow = 1)) +
  theme_dag() +
  theme(plot.background = element_rect(colour = "transparent", fill = "white"),
        text = element_text(family = "Futura"), legend.position = "bottom")

fig_A2_2 <-
  hc$price |>
  tidy_dagitty() |>
  node_status() |>
  node_ancestors("price_change") |>
  select(everything(), price_anc = ancestor) |>
  node_ancestors("rent_change") |>
  mutate(status = case_when(
    status == "exposure" ~ "b",
    status == "outcome" ~ "a",
    status == "latent" ~ "f",
    name == "price_lag_log" ~ "c",
    price_anc == "ancestor" & ancestor == "ancestor" ~ "e",
    ancestor == "ancestor" ~ "d")) |>
  mutate(label = case_when(
    status == "a" ~ "Outcome",
    status == "b" ~ "Exposure",
    status == "c" ~ "Ancestor\n(treat.)",
    status == "d" ~ "Ancestor\n(outcome)",
    status == "e" ~ "Ancestor\n(both)",
    status == "f" ~ "Latent")) |>
  mutate(y = y * -1, yend = yend * -1) |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, colour = status)) +
  geom_dag_edges(edge_width = 0.2) +
  geom_dag_point(size = 16) +
  geom_label(aes(label = name), family = "Futura", size = 3, show.legend = FALSE) +
  scale_colour_manual(name = NULL, values = c(
    a = "#1b9e77", b = "#d95f02", c = "#e6ab02", d = "#7570b3", e = "#e7298a",
    f = "grey50"),
    labels = c("Outcome", "Treatment", "Ancestor of treatment",
               "Ancestor of outcome", "Ancestor of both", "Latent")) +
  guides(colour = guide_legend(nrow = 1)) +
  theme_dag() +
  theme(plot.background = element_rect(colour = "transparent", fill = "white"),
        text = element_text(family = "Futura"), legend.position = "bottom")

fig_A2 <-
  wrap_plots(fig_A2_1, fig_A2_2) + 
  plot_layout(nrow = 2, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("output/figure_A2.png", fig_A2, width = 12, height = 13, units = "in")


# Table A3: Conditional independence tests --------------------------------

tc$var_2 |> 
  mutate(var_1 = str_extract(vars, ".*(?= _\\|\\|_)"),
         var_2 = str_extract(vars, "(?<= _\\|\\|_ ).*"),
         .before = vars) |> 
  select(-vars) |> 
  mutate(cond_vars = str_extract(var_2, "(?<= \\| ).*"),
         var_2 = str_remove(var_2, " \\| .*"),
         .after = var_2) |> 
  mutate(stars = case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ "+",
    .default = "")) |> 
  mutate(across(estimate:high, \(x) scales::comma(x, 0.001))) |> 
  mutate(estimate = paste0(estimate, stars)) |> 
  select(-p, -stars) |> 
  gt::gt()


### rent_change robustness checks ##############################################

# List of adjustment sets
map(ac, \(x) paste(x, collapse = ", "))


# Table A4: Model variants ------------------------------------------------

rob_model_names <- names(mc)[c(1, 4, 5, 2, 3, 8, 6, 7)]

map(rob_model_names, \(x) {
  mc[[x]]$b |> 
    as_tibble(rownames = "var") |> 
    mutate(low = Estimate - SE * 1.96, high = Estimate + SE * 1.96) |> 
    mutate(stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.1 ~ "+",
      .default = "")) |> 
    mutate(across(c(Estimate, SE, low, high), \(x) scales::comma(x, 0.001))) |> 
    mutate(Estimate = paste0(Estimate, stars),
           SE = paste0("(", SE, ")")) |> 
    mutate(Estimate = paste0(Estimate, "\n", SE)) |> 
    select(var, Estimate) |> 
    set_names(c("var", x))}) |> 
  reduce(full_join, "var") |> 
  bind_rows(
    map(rob_model_names, \(x) {
      mc[[x]]$s |> 
        as_tibble(rownames = "var") |> 
        set_names(c("var", "value")) |> 
        mutate(var = c("Spat. eff. (SD)", "Spat. eff. (Moran's I)")) |> 
        add_row(var = "RE (nbhd)", value = mc[[x]]$s_g$id) |>
        add_row(var = "RE (region-by-year)", 
                value = mc[[x]]$s_g$CMA_year) |>
        mutate(value = scales::comma(value, 0.001)) |> 
        add_row(var = "N. obs.", 
                value = scales::comma(length(mc[[x]]$resid))) |> 
        add_row(var = "Adj. R2 (cond.)", 
                value = scales::comma(mc[[x]]$e$stat[2], 0.001)) |> 
        add_row(var = c("R. Log. Lik.", "AIC", "BIC"),
                value = scales::comma(mc[[x]]$e$stat[3:5])) |> 
        set_names(c("var", x))}) |> 
      reduce(inner_join, "var")) |> 
  gt::gt()


# Figure A3: Parameter estimates ------------------------------------------

fig_A3 <-
  names(ac) |> 
  map(\(x) {
    mc[[x]]$b |> 
      as_tibble(rownames = "var") |> 
      mutate(model = x)}) |> 
  bind_rows() |> 
  bind_rows(
    mc$non_gauss$b |> 
      as_tibble(rownames = "var") |> 
      mutate(model = "Non-Gauss")) |> 
  bind_rows(
    mc$no_imp$b |> 
      as_tibble(rownames = "var") |> 
      mutate(model = "No-imp")) |> 
  bind_rows(
    mc$no_imp$b |> 
      as_tibble(rownames = "var") |> 
      mutate(model = "No-vac")) |> 
  filter(var %in% c("FREH_change", "non_FREH_change", "price_change")) |> 
  mutate(down_90 = Estimate - 1.645 * SE, up_90 = Estimate + 1.645 * SE) |> 
  mutate(down_95 = Estimate - 1.96 * SE, up_95 = Estimate + 1.96 * SE) |> 
  mutate(down_99 = Estimate - 2.576 * SE, up_99 = Estimate + 2.576 * SE) |> 
  mutate(model = case_when(
    model == "common.1" ~ "Main",
    model == "FREH_non_FREH.2" ~ "FREH-1",
    model == "FREH_non_FREH.3" ~ "FREH-2",
    model == "price.2" ~ "Price-1",
    model == "price.3" ~ "Price-2",
    .default = model)) |> 
  mutate(model = factor(model, levels = c(
    "Main", "FREH-1", "FREH-2", "Price-1", "Price-2", "Non-Gauss", "No-imp",
    "No-vac"))) |> 
  mutate(lw_90 = "90%", lw_95 = "95%", lw_99 = "99%") |> 
  ggplot(aes(var, Estimate, colour = model)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_pointrange(aes(ymin = down_99, ymax = up_99, lwd = lw_99), size = 0.4,
                position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = down_95, ymax = up_95, lwd = lw_95), size = 0.4,
                position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = down_90, ymax = up_90, lwd = lw_90), size = 0.4,
                position = position_dodge(0.3)) +
  scale_color_brewer(name = "Model", palette = "Dark2") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Estimate") +
  scale_linewidth_manual(name = "Confidence interval", values = c(
    "90%" = 1.2, "95%" = 0.6, "99%" = 0.2)) +
  scale_linetype(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "bottom")

ggsave("output/figure_A3.png", fig_A3, width = 8, height = 4, units = "in")


# Table A5: SNVC model ----------------------------------------------------

mc$sn_common_force


### rent_change diagnostics ####################################################

# Figure A4: Residuals-fitted plot ----------------------------------------

fig_A4 <-
  tibble(pred = mc$common.1$pred$pred, resid = mc$common.1$resid) |> 
  ggplot(aes(pred, resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_continuous(name = "Fitted values") +
  scale_y_continuous(name = "Residuals") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

ggsave("output/figure_A4.png", fig_A4, width = 8, height = 4, units = "in")


# Figure A5: QQ plots -----------------------------------------------------

fig_A5_1 <-
  ggplot() +
  geom_qq(aes(sample = mc$common.1$resid)) +
  geom_qq_line(aes(sample = mc$common.1$resid)) +
  scale_x_continuous(name = "Theoretical quantiles") + 
  scale_y_continuous(name = "Sample quantiles") + 
  ggtitle("Main RE-ESF model") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

fig_A5_2 <-
  ggplot() +
  geom_qq(aes(sample = mc$non_gauss$resid)) +
  geom_qq_line(aes(sample = mc$non_gauss$resid)) +
  scale_x_continuous(name = "Theoretical quantiles") + 
  scale_y_continuous(name = "Sample quantiles") + 
  ggtitle("Non-Gaussian RE-ESF model") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

fig_A5 <- wrap_plots(fig_A5_1, fig_A5_2, nrow = 1)

ggsave("output/figure_A5.png", fig_A5, width = 8, height = 4, units = "in")


# VIF ---------------------------------------------------------------------

# VIF on simplified version of model: All values under 2
lm(rent_change ~ FREH_change + non_FREH_change + price_change + rent_lag_log + 
     vacancy_lag_log + apart_log + income_log, data = dc$main) |> 
  car::vif()


# Figure A6: Map of residuals ---------------------------------------------

fig_A6_list <- map(largest_CMAs, \(x) {
  
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
    mutate(.resid = mc$common.1$resid) |> 
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

fig_A6 <- 
  wrap_plots(c(fig_A6_list), nrow = 2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

ggsave("output/figure_A6.png", fig_A6, width = 10, height = 7, units = "in")


# Figure A7: Stationarity of residuals and dependent variable -------------

fig_A7 <- 
  dc$main |> 
  st_drop_geometry() |> 
  mutate(Residuals = mc$common.1$resid[,1]) |> 
  select(year, Residuals, rent_change) |> 
  pivot_longer(-year) |> 
  mutate(year = as.character(year)) |> 
  ggplot(aes(year, value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

ggsave("output/figure_A7.png", fig_A7, width = 8, height = 4, units = "in")

