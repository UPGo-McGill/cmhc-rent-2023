#### RENT CHANGE MODEL OUTPUTS #################################################

source("R/10_rent_change_models.R")
DA_union <- qread("output/DA_union.qs", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())

# Get largest six CMAs to map residuals for
largest_CMAs <- 
  dc$main |> 
  st_drop_geometry() |> 
  count(CMA, sort = TRUE) |> 
  slice(1:6) |> 
  pull(CMA)


### Model summaries ############################################################

# Table 5: OLS and RE-ESF models ------------------------------------------

# OLS models
modelsummary(list(FREH = mc$l_FREH, rev = mc$l_rev, Both = mc$l_both), 
             coef_map = c("(Intercept)", "FREH_change", "rev_change", 
                          "universe_change", "universe_log", "tenant", 
                          "tourism_log"), stars = TRUE,
             add_rows = tibble(x = "Fixed effects",
                               y1 = "CMA by year",
                               y2 = "CMA by year",
                               y3 = "CMA by year") |>
               structure(position = 15),
             output = "kableExtra")

# RE-ESF model
mc$sf_both

# S&NVC model
mc$sn_both


### Effects by year ############################################################

# Table 7: Effects by year ------------------------------------------------

modelsummary(set_names(mc$l_year_FREH, 2017:2022), 
             coef_map = "FREH_change", stars = TRUE,
             output = "kableExtra")

modelsummary(set_names(mc$l_year_rev, 2017:2022), 
             coef_map = "rev_change", stars = TRUE,
             output = "kableExtra")

modelsummary(set_names(mc$l_year_both, 2017:2022), 
             coef_map = c("FREH_change", "rev_change"), stars = TRUE,
             output = "kableExtra")

map(mc$sf_year, \(x) {
  x$b |> 
    select(Estimate, SE, p_value) |> 
    slice(c(2, 3))
})



# Figure 7: Effects by year -----------------------------------------------

fig_7_1 <- map(mc$l_year_both, \(x) {
  y <- summary(x)$coefficients
  tibble(
    FREH_change = y[["FREH_change", "Estimate"]],
    rev_change = y[["rev_change", "Estimate"]],
    F_SE = y[["FREH_change", "Std. Error"]],
    R_SE = y[["rev_change", "Std. Error"]])}) |> 
  bind_rows() |> 
  mutate(year = 2017:2022, 
         model = "FREH_change and rev_change together (model viii)", 
         .before = FREH_change) |> 
  pivot_longer(c(FREH_change, rev_change), names_to = "var") |> 
  mutate(down = value - 1.96 * if_else(var == "FREH_change", F_SE, R_SE),
         up = value + 1.96 * if_else(var == "FREH_change", F_SE, R_SE)) |> 
  select(-F_SE, -R_SE)

fig_7_2 <- map(mc$l_year_FREH, \(x) {
  y <- summary(x)$coefficients
  tibble(
    FREH_change = y[["FREH_change", "Estimate"]],
    F_SE = y[["FREH_change", "Std. Error"]])}) |> 
  bind_rows() |> 
  mutate(year = 2017:2022, 
         model = "FREH_change and rev_change separate (models vi & vii)", 
         .before = FREH_change) |> 
  pivot_longer(c(FREH_change), names_to = "var") |> 
  mutate(down = value - 1.96 * F_SE, up = value + 1.96 * F_SE) |> 
  select(-F_SE)

fig_7_3 <- map(mc$l_year_rev, \(x) {
  y <- summary(x)$coefficients
  tibble(
    rev_change = y[["rev_change", "Estimate"]],
    R_SE = y[["rev_change", "Std. Error"]])}) |> 
  bind_rows() |> 
  mutate(year = 2017:2022, 
         model = "FREH_change and rev_change separate (models vi & vii)", 
         .before = rev_change) |> 
  pivot_longer(c(rev_change), names_to = "var") |> 
  mutate(down = value - 1.96 * R_SE, up = value + 1.96 * R_SE) |> 
  select(-R_SE)

fig_7_4 <- map(mc$sf_year, \(x) {
  y <- x$b
  tibble(
    FREH_change = y[["FREH_change", "Estimate"]],
    rev_change = y[["rev_change", "Estimate"]],
    F_SE = y[["FREH_change", "SE"]],
    R_SE = y[["rev_change", "SE"]])}) |> 
  bind_rows() |> 
  mutate(year = 2017:2022, 
         model = "FREH_change and rev_change together (model ix)", 
         .before = FREH_change) |> 
  pivot_longer(c(FREH_change, rev_change), names_to = "var") |> 
  mutate(down = value - 1.96 * if_else(var == "FREH_change", F_SE, R_SE),
         up = value + 1.96 * if_else(var == "FREH_change", F_SE, R_SE)) |> 
  select(-F_SE, -R_SE)

fig_7 <-
  bind_rows(fig_7_1, fig_7_2, fig_7_3, fig_7_4) |> 
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

ggsave("output/figure_7.png", fig_7, width = 8, height = 7, units = "in")

