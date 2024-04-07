#### 07 DESCRIPTIVE STATISTICS #################################################

source("R/06_data_for_models.R")


# Correlation between STRs and rent ---------------------------------------

fig_4 <- 
  monthly_sept |> 
  mutate(FREH_log = log(FREH),
         rent_log = log(rent),
         rev_log = log(rev)) |>
  filter(!is.na(rent_log), !is.na(FREH_log), !is.na(rev_log)) |>
  st_drop_geometry() |> 
  select(rent_log, FREH_log, rev_log, rent_change, FREH_change, rev_change) |> 
  filter(!is.infinite(FREH_log), !is.infinite(rev_log)) |> 
  ggpairs(aes(size = "fixed", alpha = "fixed"),
          upper = list(continuous = wrap(
            ggally_cor, display_grid = FALSE, family = "Futura")),
          lower = list(continuous = wrap(ggally_smooth_lm, se = FALSE))) +
  theme_minimal() +
  scale_size_manual(values = c(fixed = 0.2)) +
  scale_alpha_manual(values = c(fixed = 0.4)) +
  theme(text = element_text(family = "Futura"))

ggsave("output/figure_4.png", fig_4, width = 8, height = 5, units = "in")


# Variable table ----------------------------------------------------------

monthly_sept |> 
  select(rent, rent_change, FREH, FREH_change, rev, rev_change, universe, 
         universe_change, tenant, tourism) |> 
  st_drop_geometry() |> 
  mutate(FREH_dummy = FREH == 0, rev_dummy = rev == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x)))) |> 
  select(rent_log, rent_change, FREH_log, FREH_dummy, FREH_change, rev_log,
         rev_dummy, rev_change, universe_log, universe_change, tenant,
         tourism_log) |> 
  pivot_longer(everything()) |> 
  summarize(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .by = name) |> 
  mutate(across(-name, \(x) scales::comma(x, 0.001)))
