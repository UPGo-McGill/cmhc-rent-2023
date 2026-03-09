#### 12 RENT CHANGE MODEL OUTPUTS ##############################################

source("R/07_process_DAGs.R")
mc <- qread("output/mc.qs", nthreads = availableCores())


### Model summary ##############################################################

# Table 5: RE-ESF model ---------------------------------------------------

# Main results
mc$common.1

# Rounded parameters
mc$common.1$b |>
  as_tibble(rownames = "var") |>
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
  select(-t_value, -p_value, -stars) |>
  gt::gt()

nrow(mc$common.1$sf)
