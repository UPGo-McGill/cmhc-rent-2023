#### 16 APPENDIX: DIAGNOSTICS AND ROBUSTNESS CHECKS ############################

source("R/05_process_DAGs.R")
source("R/08_data_for_models.R")
qload("output/cmhc.qsm", nthreads = availableCores())
source("R/06_imputation.R")
dd <- qread("output/dd.qs")
mc <- qread("output/mc.qs", nthreads = availableCores())
md <- qread("output/md.qs")
tc <- qread("output/tc.qs")


### Structural causal model ####################################################

# Figure A1: DAGs ---------------------------------------------------------

fig_A1_1 <-
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

fig_A1_2 <-
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

fig_A1 <-
  wrap_plots(fig_A1_1, fig_A1_2) + 
  plot_layout(nrow = 2, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("output/figure_A1.png", fig_A1, width = 12, height = 13, units = "in")


# Table A1: Conditional independence tests --------------------------------

tc$var_2 |> 
  mutate(var_1 = str_extract(vars, ".*(?= _\\|\\|_)"),
         var_2 = str_extract(vars, "(?<= _\\|\\|_ ).*"),
         .before = vars) |> 
  select(-vars) |> 
  mutate(cond_vars = str_extract(var_2, "(?<= \\| ).*"),
         var_2 = str_remove(var_2, " \\| .*"),
         .after = var_2) |> 
  mutate(across(estimate:high, \(x) scales::comma(x, 0.001))) |> 
  gt::gt()


### rent_change robustness checks ##############################################

# List of adjustment sets
map(ac, \(x) paste(x, collapse = ", "))


# Table A2: Model variants ------------------------------------------------

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


# Figure A2: Parameter estimates ------------------------------------------

fig_A2 <-
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

ggsave("output/figure_A2.png", fig_A2, width = 8, height = 4, units = "in")


# Table A3: SNVC model ----------------------------------------------------

mc$sn_common_force


### rent_change diagnostics ####################################################

# Figure 3: Residuals-fitted plot -----------------------------------------

fig_A3 <-
  tibble(pred = mc$common.1$pred$pred, resid = mc$common.1$resid) |> 
  ggplot(aes(pred, resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_continuous(name = "Fitted values") +
  scale_y_continuous(name = "Residuals") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

ggsave("output/figure_A3.png", fig_A3, width = 8, height = 4, units = "in")


# Figure A4: QQ plots -----------------------------------------------------

fig_A4_1 <-
  ggplot() +
  geom_qq(aes(sample = mc$common.1$resid)) +
  geom_qq_line(aes(sample = mc$common.1$resid)) +
  scale_x_continuous(name = "Theoretical quantiles") + 
  scale_y_continuous(name = "Sample quantiles") + 
  ggtitle("Main RE-ESF model") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

fig_A4_2 <-
  ggplot() +
  geom_qq(aes(sample = mc$non_gauss$resid)) +
  geom_qq_line(aes(sample = mc$non_gauss$resid)) +
  scale_x_continuous(name = "Theoretical quantiles") + 
  scale_y_continuous(name = "Sample quantiles") + 
  ggtitle("Non-Gaussian RE-ESF model") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

fig_A4 <- wrap_plots(fig_A4_1, fig_A4_2, nrow = 1)

ggsave("output/figure_A4.png", fig_A4, width = 8, height = 4, units = "in")


# VIF ---------------------------------------------------------------------

# VIF on simplified version of model: All values under 2
lm(rent_change ~ FREH_change + non_FREH_change + price_change + rent_lag_log + 
     vacancy_lag_log + apart_log + income_log, data = dc$main) |> 
  car::vif()
