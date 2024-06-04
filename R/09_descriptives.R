#### 09 DESCRIPTIVE STATISTICS #################################################

source("R/08_data_for_models.R")


# Figure 5: Correlation between STRs and rent -----------------------------

fig_5 <-
  monthly_sept |> 
  st_drop_geometry() |> 
  select(rent_change, FREH_change, rev_change, price_change) |> 
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

ggsave("output/figure_5.png", fig_5, width = 8, height = 5, units = "in")


# Table 2: Variables ------------------------------------------------------

monthly_sept |> 
  select(rent, rent_change, FREH_change, non_FREH_change, price_change, 
         vacancy_lag, income, apart, tourism) |> 
  st_drop_geometry() |> 
  mutate(vacancy_lag = if_else(
    vacancy_lag == 0, min(vacancy_lag[vacancy_lag > 0]), vacancy_lag)) |> 
  # Create logged versions of all variables
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x)))) |> 
  select(rent_log, rent_change, FREH_change, non_FREH_change, price_change, 
         vacancy_lag_log, income_log, apart_log, tourism_log) |> 
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
