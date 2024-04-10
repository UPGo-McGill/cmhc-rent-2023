#### DISCUSSION ################################################################

source("R/08_rent_models.R")
source("R/10_rent_change_models.R")
monthly <- qread("output/monthly.qs", nthreads = availableCores())
prop_cmhc <- qread("output/prop_cmhc.qs", nthreads = availableCores())
qload("output/cmhc.qsm", nthreads = availableCores())


# Size of STR effect ------------------------------------------------------

effect_yty <- bind_rows(
  model_year(2016, 2017),
  model_year(2017, 2018),
  model_year(2018, 2019),
  model_year(2019, 2020),
  model_year(2020, 2021),
  model_year(2021, 2022))

effect_cumulative <- bind_rows(
  model_year(2016, 2017),
  model_year(2016, 2018),
  model_year(2016, 2019),
  model_year(2016, 2020),
  model_year(2016, 2021),
  model_year(2016, 2022))

# Average effect before pandemic
effect_yty |> 
  filter(end_year <= 2019) |> 
  pull(rent_change_pct) |> 
  mean() |> 
  scales::percent(0.1)

# Average effect during pandemic
effect_yty |> 
  filter(end_year %in% 2020:2021) |> 
  pull(rent_change_pct) |> 
  mean() |> 
  scales::percent(0.1)


# Figure 9: STR share of total rent ---------------------------------------

fig_9 <-
  effect_yty |> 
  select(year = end_year, rent_change_pct, dif) |> 
  mutate(type = "Year-to-year effect") |> 
  bind_rows(
    effect_cumulative |> 
      select(year = end_year, rent_change_pct, dif) |> 
      mutate(type = "Cumulative effect")) |> 
  mutate(type = factor(type, levels = c("Year-to-year effect", 
                                        "Cumulative effect"))) |> 
  ggplot(aes(year, rent_change_pct, colour = rent_change_pct)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 5) +
  geom_segment(aes(yend = 0), lwd = 2) +
  geom_label(
    aes(label = scales::dollar(dif, 0.1, scale = 1/1000000, suffix = "M")),
    family = "Futura", nudge_y = c(-0.05, -0.06, 0.06, rep(0.07, 2), -0.055, 
                                   rep(-0.02, 3), rep(0.02, 3))) +
  facet_wrap(~type, nrow = 2, scales = "free") +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  scale_colour_fermenter(type = "div", palette = "RdBu",
                         limits = c(-0.01, 0.01), oob = scales::squish) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "none")

ggsave("output/figure_9.png", fig_9, width = 8, height = 5, units = "in")


# Size of STR market compared to LTR market -------------------------------

# Oct 2022 rent in CMHC neighbourhoods
cmhc |> 
  filter(year == 2022) |> 
  summarize(total_rent = sum(rent * universe, na.rm = TRUE)) |> 
  pull() |> 
  scales::dollar(1000000)

# Sep 2022 STR host revenue in CMHC neighbourhoods
monthly |> 
  mutate(year = year(month)) |>
  inner_join(prop_cmhc, by = "property_ID") |>
  filter(month == yearmonth("2022-09")) |> 
  pull(rev) |> 
  sum() |> 
  scales::dollar(1000000)

# Ratio of the two
((monthly |> 
    mutate(year = year(month)) |>
    inner_join(prop_cmhc, by = "property_ID") |>
    filter(month == yearmonth("2022-09")) |> 
    pull(rev) |> 
    sum()) / (cmhc |> 
                filter(year == 2022) |> 
                summarize(total_rent = sum(rent * universe, na.rm = TRUE)) |> 
                pull())) |> 
  scales::percent(0.1)

# STR revenue as % of total residential revenue
((monthly |> 
    mutate(year = year(month)) |>
    inner_join(prop_cmhc, by = "property_ID") |>
    filter(month == yearmonth("2022-09")) |> 
    pull(rev) |> 
    sum()) / ((cmhc |> 
                 filter(year == 2022) |> 
                 summarize(total_rent = sum(rent * universe, na.rm = TRUE)) |> 
                 pull()) + (monthly |> 
                              mutate(year = year(month)) |>
                              inner_join(prop_cmhc, by = "property_ID") |>
                              filter(month == yearmonth("2022-09")) |> 
                              pull(rev) |> 
                              sum()))) |> 
  scales::percent(0.1)


# Comparison of rent_log and rent_change models ---------------------------

effect_change <- bind_rows(
  model_change_manual(2017, 0, 0),
  model_change_manual(2018, 0, 0),
  model_change_manual(2019, 0, 0),
  model_change_manual(2020, 0, 0),
  model_change_manual(2021, 0, 0),
  model_change_manual(2022, 0, 0)
)

# Average effect size compared to rent_log model
mean(effect_change$rent_change_pct / effect_yty$rent_change_pct)


# Figure 10: Comparison of rent_log and rent_change ------------------------

fig_10_data <- 
  effect_yty |> 
  select(year = end_year, rent_change_pct) |> 
  mutate(type = "rent_log effect") |> 
  bind_rows(
    effect_change |> 
      select(year, rent_change_pct) |> 
      mutate(type = "rent_change effect")) |> 
  pivot_wider(names_from = type, values_from = rent_change_pct)

fig_10 <-
  effect_yty |> 
  select(year = end_year, rent_change_pct, dif) |> 
  mutate(type = "rent_log effect") |> 
  bind_rows(
    effect_change |> 
      select(year, rent_change_pct, dif) |> 
      mutate(type = "rent_change effect")) |>
  mutate(type = factor(type, levels = c(
    "rent_log effect", "rent_change effect"))) |>
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = year, y = `rent_log effect`, 
                   yend = `rent_change effect`),
               data = fig_10_data) +
  geom_point(aes(year, rent_change_pct, colour = type), size = 5) +
  geom_label(aes(year, rent_change_pct, colour = type, 
                 label = scales::dollar(dif, 0.1, scale = 1/1000000, suffix = "M")),
             family = "Futura", nudge_y = c(
               rep(0.025, 4), rep(-0.025, 2), rep(-0.025, 3),
               rep(0.025, 2), -0.025), show.legend = FALSE) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  scale_color_brewer(name = NULL, palette = "Dark2") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "bottom")

ggsave("output/figure_10.png", fig_10, width = 8, height = 5, units = "in")


# FREH-focused scenarios --------------------------------------------------

# Table 11: Regional FREH contributions -----------------------------------

model_year(2021, 2022, province, use_rev = FALSE) |> 
  mutate_region() |> 
  summarize(total_rent_dif = sum(total_rent_dif),
            dif = sum(dif),
            rent_change_pct = dif / total_rent_dif,
            .by = region) |> 
  mutate(total_rent_dif = scales::dollar(total_rent_dif, 0.1, scale = 1/1000000, 
                                         suffix = "M"),
         dif = scales::dollar(dif, 0.1, scale = 1/1000000, suffix = "M"),
         rent_change_pct = scales::percent(rent_change_pct, 0.1)) |> 
  set_names(c("Region", "Total rent increase", "FREH contribution ($)", 
              "FREH contribution (%)")) |> 
  gt::gt()
  
model_change_manual(2022, 0, dc$impute$rev_change[dc$impute$year == "2021"],
                    province) |> 
  mutate_region() |> 
  summarize(total_rent_dif = sum(total_rent_dif),
            dif = sum(dif),
            rent_change_pct = dif / total_rent_dif,
            .by = region) |> 
  mutate(total_rent_dif = scales::dollar(total_rent_dif, 0.1, scale = 1/1000000, 
                                         suffix = "M"),
         dif = scales::dollar(dif, 0.1, scale = 1/1000000, suffix = "M"),
         rent_change_pct = scales::percent(rent_change_pct, 0.1)) |> 
  set_names(c("Region", "Total rent increase", "FREH contribution ($)", 
              "FREH contribution (%)")) |> 
  gt::gt()
