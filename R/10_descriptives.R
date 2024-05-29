#### 10 DESCRIPTIVE STATISTICS #################################################

source("R/09_data_for_models.R")


# Figure 6: Correlation between STRs and rent -----------------------------

fig_6 <-
  monthly_sept |> 
  mutate(rent_log = log(rent),
         FREH_lag_log = log(lag(FREH)),
         rev_lag_log = log(lag(rev)),
         price_lag_log = log(lag(price))) |>
  filter(!is.na(rent_log), !is.na(FREH_lag_log), !is.na(rev_lag_log), 
         !is.na(price_lag_log)) |>
  st_drop_geometry() |> 
  select(rent_log, FREH_lag_log, rev_lag_log, price_lag_log, rent_change, 
         FREH_change, rev_change, price_change) |> 
  filter(!is.infinite(FREH_lag_log), !is.infinite(rev_lag_log), 
         !is.infinite(price_lag_log)) |> 
  GGally::ggpairs(aes(size = "fixed", alpha = "fixed"),
          upper = list(continuous = GGally::wrap(
            GGally::ggally_cor, display_grid = FALSE, family = "Futura")),
          lower = list(continuous = GGally::wrap(
            GGally::ggally_smooth_lm, se = FALSE))) +
  theme_minimal() +
  scale_size_manual(values = c(fixed = 0.2)) +
  scale_alpha_manual(values = c(fixed = 0.4)) +
  theme(text = element_text(family = "Futura"),
        axis.text = element_text(size = 5))

ggsave("output/figure_6.png", fig_6, width = 8, height = 7, units = "in")


# Table 2: Variables ------------------------------------------------------

monthly_sept |> 
  select(rent, rent_change, FREH_lag, FREH_change, rev_lag, rev_change, 
         price_lag, price_change, universe_change, vacancy, income, apart, 
         tourism) |> 
  st_drop_geometry() |> 
  mutate(FREH_lag_dummy = FREH_lag == 0, rev_price_lag_dummy = rev_lag == 0, 
         .before = rent) |> 
  mutate(FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
         rev_lag = if_else(rev_lag == 0, min(rev_lag[rev_lag > 0]), rev_lag),
         price_lag = if_else(price_lag == 0, min(price_lag[price_lag > 0]), 
                             price_lag)) |> 
  # Create logged versions of all variables
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x)))) |> 
  select(rent_log, rent_change, FREH_lag_log, FREH_lag_dummy, FREH_change, 
         rev_lag_log, rev_price_lag_dummy, rev_change, price_lag_log,
         price_change, universe_change, vacancy, income_log, apart, 
         tourism_log) |> 
  pivot_longer(everything()) |> 
  summarize(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .by = name) |> 
  mutate(across(-c(name, n), \(x) scales::comma(x, 0.001))) |> 
  suppressWarnings()
