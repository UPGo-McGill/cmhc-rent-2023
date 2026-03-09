#### 15 APPENDIX: DIAGNOSTICS AND ROBUSTNESS CHECKS ############################

source("R/06_data_for_models.R")
source("R/07_process_DAGs.R")
qload("output/cmhc.qsm", nthreads = availableCores())
source("R/05_imputation.R")
monthly <- qread("output/monthly.qs", nthreads = availableCores())
dd <- qread("output/dd.qs")
mc <- qread("output/mc.qs", nthreads = availableCores())
md <- qread("output/md.qs")
md_no_pool <- qread("output/md_no_pool.qs")
md_CSD <- qread("output/md_CSD.qs")
DA_union <- qread("output/DA_union.qs", nthreads = availableCores())
CSD <- qread("output/CSD.qs", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())
water <- qread("output/water.qs", nthreads = availableCores())

largest_CMAs <-
  dc$main |>
  st_drop_geometry() |>
  count(CMA, sort = TRUE) |>
  slice(1:6) |>
  pull(CMA)


# Airbnb spatial obfuscation ###################################################

cmhc_buffer <-
  cmhc_nbhd |>
  select(id:name_CMA, geometry) |>
  st_cast("MULTILINESTRING") |>
  st_buffer(200)

monthly_unique <-
  monthly |>
  distinct(property_ID, .keep_all = TRUE) |>
  strr_as_sf(3347)

ints <-
  monthly_unique |>
  select(property_ID, geometry) |>
  st_filter(cmhc_buffer)

ints_2 <-
  ints |>
  st_intersection(cmhc_buffer)

ints_2 |>
  st_drop_geometry() |>
  count(property_ID) |>
  count(n) |>
  add_row(n = 0, nn = nrow(monthly_unique)) |>
  mutate(nn = if_else(n == 0, 2 * nn - sum(nn), nn)) |>
  arrange(n) |>
  mutate(pct = nn / sum(nn)) |>
  group_by(overlap = n > 1) |>
  summarize(pct = sum(pct))

ints_CSD <-
  cmhc_nbhd |>
  select(id:name_CMA) |>
  mutate(nbhd_area = as.numeric(st_area(geometry)), .before = geometry) |>
  st_intersection(
    CSD |>
      select(CSD:name_CSD) |>
      mutate(CSD_area = as.numeric(st_area(geometry)), .before = geometry)
  )

nbhd_csd <-
  ints_CSD |>
  mutate(int_area = as.numeric(st_area(geometry)), .before = geometry) |>
  st_drop_geometry() |>
  mutate(area_pct = int_area / CSD_area) |>
  filter(area_pct > 0.9)

ints_2 |>
  st_drop_geometry() |>
  filter(id %in% nbhd_csd$id) |>
  count(property_ID) |>
  count(n) |>
  filter(n > 1)


# DiD dataset ##################################################################

# Load regulations
reg <-
  qread("data/reg.qs") |>
  mutate(reg = if_else(reg == "TBD", FALSE, as.logical(reg))) |>
  mutate(date = if_else(date >= "2023-01-02", NA, date)) |>
  mutate(reg = if_else(is.na(date), FALSE, TRUE)) |>
  inner_join(st_drop_geometry(cmhc_nbhd), by = c("id", "name")) |>
  select(-c(pop:tenant))


## Table A1: STR regulations ###################################################

reg |>
  filter(reg) |>
  count(name_CSD, province, date) |>
  select(-n) |>
  gt::gt()

# Montreal borough details
reg |>
  filter(reg) |>
  filter(name_CSD == "Montréal")


# RE-ESF correlation matrix ####################################################

fig_A1 <-
  monthly_sept |>
  filter(year >= 2018) |>
  st_drop_geometry() |>
  select(rent_change, FREH_change, non_FREH_change, price_change) |>
  filter(
    abs(rent_change) < 400,
    abs(FREH_change) < 0.01,
    abs(non_FREH_change) < 0.01,
    abs(price_change) < 400
  ) |>
  GGally::ggpairs(
    aes(size = "fixed", alpha = "fixed"),
    upper = list(
      continuous = GGally::wrap(
        GGally::ggally_cor,
        display_grid = FALSE,
        family = "Futura"
      )
    ),
    lower = list(
      continuous = GGally::wrap(
        GGally::ggally_smooth_lm,
        se = FALSE
      )
    )
  ) +
  scale_size_manual(values = c(fixed = 0.2)) +
  scale_alpha_manual(values = c(fixed = 0.4)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Futura"),
    axis.text = element_text(size = 5),
    strip.text = element_text(size = 7)
  )

ggsave("figures/figure_A1.png", fig_A1, width = 8, height = 4, units = "in")


# DiD robustness checks ########################################################

ad <- map(md, map, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .05))
ad_no_pool <- map(md_no_pool$main, \(x) {
  aggte(x, type = "simple", na.rm = TRUE, alp = .05)
})
ad_CSD <- map(md_CSD$main, \(x) {
  aggte(x, type = "simple", na.rm = TRUE, alp = .05)
})

# Provincial population
get_census("CA21", regions = list(C = 1), level = "PR") |>
  mutate(in_df = GeoUID %in% c("13", "24", "35", "59")) |>
  summarize(dwellings = sum(Dwellings), .by = in_df) |>
  mutate(pct = dwellings / sum(dwellings))


## Table A2: ATT for all model variants ########################################

map(names(ad), \(x) {
  tibble(
    model = x,
    var = names(ad[[x]]),
    att = round(map_dbl(ad[[x]], \(x) x$overall.att), 3),
    se = round(map_dbl(ad[[x]], \(x) x$overall.se), 3)
  )
}) |>
  bind_rows() |>
  mutate(att = paste0(att, "\n(", se, ")")) |>
  select(-se) |>
  pivot_wider(names_from = var, values_from = att) |>
  filter(model != "no_2023") |>
  bind_rows(
    tibble(
      model = "non_pooled",
      var = names(ad_no_pool),
      att = round(map_dbl(ad_no_pool, \(x) x$overall.att), 3),
      se = round(map_dbl(ad_no_pool, \(x) x$overall.se), 3)
    ) |>
      mutate(att = paste0(att, "\n(", se, ")")) |>
      select(-se) |>
      pivot_wider(names_from = var, values_from = att),
    tibble(
      model = "CSD_pooled",
      var = names(ad_CSD),
      att = round(map_dbl(ad_CSD, \(x) x$overall.att), 3),
      se = round(map_dbl(ad_CSD, \(x) x$overall.se), 3)
    ) |>
      mutate(att = paste0(att, "\n(", se, ")")) |>
      select(-se) |>
      pivot_wider(names_from = var, values_from = att)
  ) |>
  gt::gt()

# Confidence intervals
map(md, map, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .001))
map(md, map, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .01))
ad
map(md, map, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .1))

map(md_no_pool$main, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .001))
map(md_no_pool$main, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .01))
map(md_no_pool$main, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .05))
map(md_no_pool$main, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .1))

map(md_CSD$main, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .001))
map(md_CSD$main, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .01))
map(md_CSD$main, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .05))
map(md_CSD$main, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .1))


# DiD diagnostics ##############################################################

## Figure A2: Parallel trends assumption #######################################

fig_A2_list <-
  map(setdiff(names(md), c("no_2023", "FREH_60", "FREH_120", "adj")), \(x) {
    aggte(md[[x]]$rent_log, type = "dynamic") |>
      ggdid() +
      ggtitle(x) +
      theme_minimal() +
      scale_x_continuous(
        name = "Years post-treatment",
        breaks = c(-6, -4, -2, 0, 2, 4),
        limits = c(-6, 4)
      ) +
      scale_y_continuous(name = "ATT", limits = c(-0.4, 0.2)) +
      scale_color_brewer(
        name = NULL,
        palette = "Dark2",
        labels = c(
          "Pre-treatment",
          "Post-treatment"
        )
      ) +
      theme(text = element_text(family = "Futura"), legend.position = "bottom")
  })

fig_A2 <-
  wrap_plots(fig_A2_list, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("figures/figure_A2.png", fig_A2, width = 8, height = 8, units = "in")


# Structural causal model ######################################################

## Figure A3: DAGs #############################################################

fig_A3_1 <-
  hc$FREH |>
  tidy_dagitty() |>
  node_status() |>
  node_ancestors("FREH_change") |>
  select(everything(), FREH_anc = ancestor) |>
  node_ancestors("rent_change") |>
  mutate(
    status = case_when(
      status == "exposure" ~ "b",
      status == "outcome" ~ "a",
      status == "latent" ~ "f",
      name == "FREH_lag_log" ~ "c",
      FREH_anc == "ancestor" & ancestor == "ancestor" ~ "e",
      ancestor == "ancestor" ~ "d"
    )
  ) |>
  mutate(
    label = case_when(
      status == "a" ~ "Outcome",
      status == "b" ~ "Exposure",
      status == "c" ~ "Ancestor\n(treat.)",
      status == "d" ~ "Ancestor\n(outcome)",
      status == "e" ~ "Ancestor\n(both)",
      status == "f" ~ "Latent"
    )
  ) |>
  mutate(y = y * -1, yend = yend * -1) |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, colour = status)) +
  geom_dag_edges(edge_width = 0.2) +
  geom_dag_point(size = 16) +
  geom_label(
    aes(label = name),
    family = "Futura",
    size = 3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      a = "#1b9e77",
      b = "#d95f02",
      c = "#e6ab02",
      d = "#7570b3",
      e = "#e7298a",
      f = "grey50"
    ),
    labels = c(
      "Outcome",
      "Treatment",
      "Ancestor of treatment",
      "Ancestor of outcome",
      "Ancestor of both",
      "Latent"
    )
  ) +
  guides(colour = guide_legend(nrow = 1)) +
  theme_dag() +
  theme(
    plot.background = element_rect(colour = "transparent", fill = "white"),
    text = element_text(family = "Futura"),
    legend.position = "bottom"
  )

fig_A3_2 <-
  hc$non_FREH |>
  tidy_dagitty() |>
  node_status() |>
  node_ancestors("non_FREH_change") |>
  select(everything(), non_FREH_anc = ancestor) |>
  node_ancestors("rent_change") |>
  mutate(
    status = case_when(
      status == "exposure" ~ "b",
      status == "outcome" ~ "a",
      status == "latent" ~ "f",
      name == "non_FREH_lag_log" ~ "c",
      non_FREH_anc == "ancestor" & ancestor == "ancestor" ~ "e",
      ancestor == "ancestor" ~ "d"
    )
  ) |>
  mutate(
    label = case_when(
      status == "a" ~ "Outcome",
      status == "b" ~ "Exposure",
      status == "c" ~ "Ancestor\n(treat.)",
      status == "d" ~ "Ancestor\n(outcome)",
      status == "e" ~ "Ancestor\n(both)",
      status == "f" ~ "Latent"
    )
  ) |>
  mutate(y = y * -1, yend = yend * -1) |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, colour = status)) +
  geom_dag_edges(edge_width = 0.2) +
  geom_dag_point(size = 16) +
  geom_label(
    aes(label = name),
    family = "Futura",
    size = 3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      a = "#1b9e77",
      b = "#d95f02",
      c = "#e6ab02",
      d = "#7570b3",
      e = "#e7298a",
      f = "grey50"
    ),
    labels = c(
      "Outcome",
      "Treatment",
      "Ancestor of treatment",
      "Ancestor of outcome",
      "Ancestor of both",
      "Latent"
    )
  ) +
  guides(colour = guide_legend(nrow = 1)) +
  theme_dag() +
  theme(
    plot.background = element_rect(colour = "transparent", fill = "white"),
    text = element_text(family = "Futura"),
    legend.position = "bottom"
  )

fig_A3_3 <-
  hc$price |>
  tidy_dagitty() |>
  node_status() |>
  node_ancestors("price_change") |>
  select(everything(), price_anc = ancestor) |>
  node_ancestors("rent_change") |>
  mutate(
    status = case_when(
      status == "exposure" ~ "b",
      status == "outcome" ~ "a",
      status == "latent" ~ "f",
      name == "price_lag_log" ~ "c",
      price_anc == "ancestor" & ancestor == "ancestor" ~ "e",
      ancestor == "ancestor" ~ "d"
    )
  ) |>
  mutate(
    label = case_when(
      status == "a" ~ "Outcome",
      status == "b" ~ "Exposure",
      status == "c" ~ "Ancestor\n(treat.)",
      status == "d" ~ "Ancestor\n(outcome)",
      status == "e" ~ "Ancestor\n(both)",
      status == "f" ~ "Latent"
    )
  ) |>
  mutate(y = y * -1, yend = yend * -1) |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, colour = status)) +
  geom_dag_edges(edge_width = 0.2) +
  geom_dag_point(size = 16) +
  geom_label(
    aes(label = name),
    family = "Futura",
    size = 3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      a = "#1b9e77",
      b = "#d95f02",
      c = "#e6ab02",
      d = "#7570b3",
      e = "#e7298a",
      f = "grey50"
    ),
    labels = c(
      "Outcome",
      "Treatment",
      "Ancestor of treatment",
      "Ancestor of outcome",
      "Ancestor of both",
      "Latent"
    )
  ) +
  guides(colour = guide_legend(nrow = 1)) +
  theme_dag() +
  theme(
    plot.background = element_rect(colour = "transparent", fill = "white"),
    text = element_text(family = "Futura"),
    legend.position = "bottom"
  )

fig_A3 <-
  wrap_plots(fig_A3_1, fig_A3_2, fig_A3_3) +
  plot_layout(nrow = 3, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("figures/figure_A3.png", fig_A3, width = 12, height = 19.5, units = "in")


## Conditional independence tests ##############################################

tc <- list()

tc <- map(1:4, \(x) {
  hc$common |>
    localTests(
      data = select(st_drop_geometry(dc$main), -c(id:vacancy_lag_dummy)),
      type = "cis",
      abbreviate.names = FALSE,
      max.conditioning.variables = x,
      conf.level = 0.95
    ) |>
    as_tibble(rownames = "vars") |>
    set_names(c("vars", "estimate", "p", "low", "high"))
}) |>
  set_names(paste0("var_", 1:4))

# Test successes
map(tc, \(x) count(x, pass = low * high <= 0))

# Test failures
test_failures <- map(tc, \(x) {
  x |>
    filter(low * high > 0) |>
    pull(vars) |>
    str_split(" \\_\\|\\|\\_ ") |>
    unlist() |>
    str_split(" \\| ") |>
    unlist() |>
    str_split(", ") |>
    unlist() |>
    table()
})

# Largest failure
max(tc$var_2$estimate)


## Figure A4: Conditional independence #########################################

fig_A4 <-
  tc$var_2 |>
  ggplot() +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_pointrange(
    aes(x = estimate, y = vars, xmin = low, xmax = high),
    size = 0.2,
    linewidth = 0.8
  ) +
  scale_y_discrete(name = NULL) +
  scale_x_continuous(
    name = "Pearson correlation coefficient",
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  theme(text = element_text(family = "futura"))

ggsave("figures/figure_A4.png", fig_A4, width = 8, height = 5, units = "in")


## Table A3: Conditional independence tests ####################################

tc$var_2 |>
  mutate(
    var_1 = str_extract(vars, ".*(?= _\\|\\|_)"),
    var_2 = str_extract(vars, "(?<= _\\|\\|_ ).*"),
    .before = vars
  ) |>
  select(-vars) |>
  mutate(
    cond_vars = str_extract(var_2, "(?<= \\| ).*"),
    var_2 = str_remove(var_2, " \\| .*"),
    .after = var_2
  ) |>
  mutate(
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      p < 0.1 ~ "+",
      .default = ""
    )
  ) |>
  mutate(across(estimate:high, \(x) scales::comma(x, 0.001))) |>
  mutate(estimate = paste0(estimate, stars)) |>
  select(-p, -stars) |>
  gt::gt()


# rent_change robustness checks ################################################

# List of adjustment sets
map(ac, \(x) paste(x, collapse = ", "))


## Table A4: Model variants ####################################################

rob_model_names <- names(mc)[c(1, 4, 5, 2, 3, 8, 6, 7, 9, 10)]

map(rob_model_names, \(x) {
  mc[[x]]$b |>
    as_tibble(rownames = "var") |>
    mutate(var = str_replace(var, "_60_", "_")) |>
    mutate(var = str_replace(var, "_120_", "_")) |>
    mutate(low = Estimate - SE * 1.96, high = Estimate + SE * 1.96) |>
    mutate(
      stars = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        p_value < 0.1 ~ "+",
        .default = ""
      )
    ) |>
    mutate(across(c(Estimate, SE, low, high), \(x) scales::comma(x, 0.001))) |>
    mutate(Estimate = paste0(Estimate, stars), SE = paste0("(", SE, ")")) |>
    mutate(Estimate = paste0(Estimate, "\n", SE)) |>
    select(var, Estimate) |>
    set_names(c("var", x))
}) |>
  reduce(full_join, "var") |>
  bind_rows(
    map(rob_model_names, \(x) {
      mc[[x]]$s |>
        as_tibble(rownames = "var") |>
        set_names(c("var", "value")) |>
        mutate(var = c("Spat. eff. (SD)", "Spat. eff. (Moran's I)")) |>
        add_row(var = "RE (nbhd)", value = mc[[x]]$s_g$id) |>
        add_row(var = "RE (region-by-year)", value = mc[[x]]$s_g$CMA_year) |>
        mutate(value = scales::comma(value, 0.001)) |>
        add_row(
          var = "N. obs.",
          value = scales::comma(length(mc[[x]]$resid))
        ) |>
        add_row(
          var = "Adj. R2 (cond.)",
          value = scales::comma(mc[[x]]$e$stat[2], 0.001)
        ) |>
        add_row(
          var = c("R. Log. Lik.", "AIC", "BIC"),
          value = scales::comma(mc[[x]]$e$stat[3:5])
        ) |>
        set_names(c("var", x))
    }) |>
      reduce(inner_join, "var")
  ) |>
  gt::gt()


## Figure A5: Parameter estimates ##############################################

fig_A5 <-
  names(ac) |>
  map(\(x) {
    mc[[x]]$b |>
      as_tibble(rownames = "var") |>
      mutate(model = x)
  }) |>
  bind_rows() |>
  bind_rows(
    mc$non_gauss$b |>
      as_tibble(rownames = "var") |>
      mutate(model = "Non-Gauss")
  ) |>
  bind_rows(
    mc$no_imp$b |>
      as_tibble(rownames = "var") |>
      mutate(model = "No-imp")
  ) |>
  bind_rows(
    mc$no_imp$b |>
      as_tibble(rownames = "var") |>
      mutate(model = "No-vac")
  ) |>
  bind_rows(
    mc$FREH_60$b |>
      as_tibble(rownames = "var") |>
      mutate(var = str_replace(var, "_60_", "_")) |>
      mutate(model = "FREH_60")
  ) |>
  bind_rows(
    mc$FREH_120$b |>
      as_tibble(rownames = "var") |>
      mutate(var = str_replace(var, "_120_", "_")) |>
      mutate(model = "FREH_120")
  ) |>
  filter(var %in% c("FREH_change", "non_FREH_change", "price_change")) |>
  mutate(down_90 = Estimate - 1.645 * SE, up_90 = Estimate + 1.645 * SE) |>
  mutate(down_95 = Estimate - 1.96 * SE, up_95 = Estimate + 1.96 * SE) |>
  mutate(down_99 = Estimate - 2.576 * SE, up_99 = Estimate + 2.576 * SE) |>
  mutate(
    model = case_when(
      model == "common.1" ~ "Main",
      model == "FREH_non_FREH.2" ~ "FREH-1",
      model == "FREH_non_FREH.3" ~ "FREH-2",
      model == "price.2" ~ "Price-1",
      model == "price.3" ~ "Price-2",
      .default = model
    )
  ) |>
  mutate(
    model = factor(
      model,
      levels = c(
        "Main",
        "FREH-1",
        "FREH-2",
        "Price-1",
        "Price-2",
        "Non-Gauss",
        "No-imp",
        "No-vac",
        "FREH_60",
        "FREH_120"
      )
    )
  ) |>
  mutate(lw_90 = "90%", lw_95 = "95%", lw_99 = "99%") |>
  ggplot(aes(var, Estimate, colour = model)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_pointrange(
    aes(ymin = down_99, ymax = up_99, lwd = lw_99),
    size = 0.4,
    position = position_dodge(0.3)
  ) +
  geom_pointrange(
    aes(ymin = down_95, ymax = up_95, lwd = lw_95),
    size = 0.4,
    position = position_dodge(0.3)
  ) +
  geom_pointrange(
    aes(ymin = down_90, ymax = up_90, lwd = lw_90),
    size = 0.4,
    position = position_dodge(0.3)
  ) +
  scale_color_viridis_d(name = "Model") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Estimate") +
  scale_linewidth_manual(
    name = "Confidence interval",
    values = c(
      "90%" = 1.2,
      "95%" = 0.6,
      "99%" = 0.2
    )
  ) +
  scale_linetype(name = NULL) +
  guides(linewidth = guide_legend(nrow = 2)) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "bottom")

ggsave("figures/figure_A5.png", fig_A5, width = 8.5, height = 4, units = "in")


# Table A5: SNVC model #########################################################

mc$sn_common_force


# rent_change diagnostics ######################################################

## Figure A6: Residuals-fitted plot ############################################

fig_A6 <-
  tibble(pred = mc$common.1$pred$pred, resid = mc$common.1$resid) |>
  ggplot(aes(pred, resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_continuous(name = "Fitted values") +
  scale_y_continuous(name = "Residuals") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

ggsave("figures/figure_A6.png", fig_A6, width = 8, height = 4, units = "in")


## Figure A7: QQ plots #########################################################

fig_A7_1 <-
  ggplot() +
  geom_qq(aes(sample = mc$common.1$resid)) +
  geom_qq_line(aes(sample = mc$common.1$resid)) +
  scale_x_continuous(name = "Theoretical quantiles") +
  scale_y_continuous(name = "Sample quantiles") +
  ggtitle("Main RE-ESF model") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

fig_A7_2 <-
  ggplot() +
  geom_qq(aes(sample = mc$non_gauss$resid)) +
  geom_qq_line(aes(sample = mc$non_gauss$resid)) +
  scale_x_continuous(name = "Theoretical quantiles") +
  scale_y_continuous(name = "Sample quantiles") +
  ggtitle("Non-Gaussian RE-ESF model") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

fig_A7 <- wrap_plots(fig_A7_1, fig_A7_2, nrow = 1)

ggsave("figures/figure_A7.png", fig_A7, width = 8, height = 4, units = "in")


## VIF #########################################################################

# VIF on simplified version of model: All values under 2
lm(
  rent_change ~ FREH_change +
    non_FREH_change +
    price_change +
    rent_lag_log +
    vacancy_lag_log +
    apart_log +
    income_log,
  data = dc$main
) |>
  car::vif()


## Figure A8: Map of residuals #################################################

fig_A8_list <- map(largest_CMAs, \(x) {
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
    summarize(
      .resid = mean(.resid, na.rm = TRUE),
      across(geometry, st_union),
      .by = id
    ) |>
    ggplot(aes(fill = .resid)) +
    geom_sf(data = prov, fill = "grey", colour = "transparent", lwd = 0.1) +
    geom_sf(colour = "white") +
    geom_sf(data = wat, colour = "transparent", fill = "white") +
    scale_fill_viridis_b(
      name = "Residual",
      limits = c(-0.5, 0.5),
      n.breaks = 7,
      oob = scales::squish
    ) +
    ggtitle(name) +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    theme_void() +
    theme(
      plot.margin = margin(10, 10, 10, 10),
      text = element_text(family = "Futura")
    )
})

fig_A8 <-
  wrap_plots(c(fig_A8_list), nrow = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

ggsave("figures/figure_A8.png", fig_A8, width = 10, height = 7, units = "in")


## Figure A9: Stationarity of residuals and dependent variable #################

fig_A9 <-
  dc$main |>
  st_drop_geometry() |>
  mutate(Residuals = mc$common.1$resid[, 1]) |>
  select(year, Residuals, rent_change) |>
  pivot_longer(-year) |>
  mutate(year = as.character(year)) |>
  ggplot(aes(year, value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

ggsave("figures/figure_A9.png", fig_A9, width = 8, height = 4, units = "in")
