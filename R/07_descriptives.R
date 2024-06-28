#### 07 DESCRIPTIVE STATISTICS #################################################

source("R/06_data_for_models.R")
qload("output/cmhc.qsm", nthreads = availableCores())


# DiD treatment -----------------------------------------------------------

reg <-
  qread("data/reg.qs") |> 
  mutate(reg = if_else(reg == "TBD", FALSE, as.logical(reg))) |> 
  mutate(date = if_else(date >= "2023-01-02", NA, date)) |> 
  mutate(reg = if_else(is.na(date), FALSE, TRUE)) |> 
  inner_join(st_drop_geometry(cmhc_nbhd), by = c("id", "name")) |> 
  select(-c(pop:tenant))

# How many cities with treatment
reg |> 
  filter(reg) |> 
  count(name_CSD) |> 
  nrow()

# How many neighbourhoods with treatment
reg |> 
  filter(reg) |> 
  nrow()

# How many neighbourhoods in total
dr$main |> 
  inner_join(select(reg, id, date, reg), by = "id") |> 
  mutate(treat = if_else(is.na(date), 0, year(date - 1) + 1), 
         # Add prefix so leading 0s don't get removed
         id = as.numeric(paste0("1111", id))) |> 
  filter(treat == 0 | treat > 2017) |> 
  # Remove provinces with no treatment in the time period
  filter(!province %in% c("Manitoba", "Saskatchewan", "Nova Scotia", 
                          "Prince Edward Island", "Alberta",
                          "Newfoundland and Labrador")) |> 
  count(id) |> 
  nrow()


# Table 2: Variables ------------------------------------------------------

monthly_sept |> 
  mutate(across(c(FREH_change, non_FREH_change, price_change),
                \(x) if_else(year <= 2018, NA, x))) |> 
  select(rent, rent_change, FREH_change, non_FREH_change, price_change, 
         rent_lag, vacancy_lag, income, apart, tourism) |> 
  st_drop_geometry() |> 
  mutate(vacancy_lag = if_else(
    vacancy_lag == 0, min(vacancy_lag[vacancy_lag > 0]), vacancy_lag)) |> 
  # Create logged versions of all variables
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x)))) |> 
  select(rent_log, rent_change, FREH_change, non_FREH_change, price_change, 
         rent_lag_log, vacancy_lag_log, income_log, apart_log, tourism_log) |> 
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


# Figure 4: Correlation between STR change and rent_change ----------------

fig_4 <-
  monthly_sept |> 
  filter(year >= 2018) |> 
  st_drop_geometry() |> 
  select(rent_change, FREH_change, non_FREH_change, price_change) |> 
  filter(abs(rent_change) < 400,
         abs(FREH_change) < 0.01,
         abs(non_FREH_change) < 0.01,
         abs(price_change) < 400) |> 
  GGally::ggpairs(aes(size = "fixed", alpha = "fixed"),
                  upper = list(continuous = GGally::wrap(
                    GGally::ggally_cor, display_grid = FALSE, family = "Futura")),
                  lower = list(continuous = GGally::wrap(
                    GGally::ggally_smooth_lm, se = FALSE))) +
  scale_size_manual(values = c(fixed = 0.2)) +
  scale_alpha_manual(values = c(fixed = 0.4)) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        axis.text = element_text(size = 5),
        strip.text = element_text(size = 7))

ggsave("output/figure_4.png", fig_4, width = 8, height = 4, units = "in")
