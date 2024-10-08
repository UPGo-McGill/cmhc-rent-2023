#### 15 DISCUSSION #############################################################

source("R/06_data_for_models.R")
source("R/08_process_DAGs.R")
qload("output/cmhc.qsm", nthreads = availableCores())
source("R/05_imputation.R")
dd <- qread("output/dd.qs")
mc <- qread("output/mc.qs", nthreads = availableCores())
md <- qread("output/md.qs")


# Size of STR effect ------------------------------------------------------

effect_yty <- bind_rows(
  model_change(2018),
  model_change(2019),
  model_change(2020),
  model_change(2021),
  model_change(2022))

dc_2017 <-
  dc$main |> 
  st_drop_geometry() |> 
  filter(year == 2018) |> 
  select(id, FREH_lag_raw, FREH_lag_dummy, non_FREH_lag_raw, non_FREH_lag_dummy,
         price_lag_raw, price_lag_dummy) |> 
  mutate(FREH_lag_raw = if_else(FREH_lag_dummy, 0, FREH_lag_raw),
         non_FREH_lag_raw = if_else(non_FREH_lag_dummy, 0, non_FREH_lag_raw),
         price_lag_raw = if_else(price_lag_dummy, 0, price_lag_raw)) |> 
  select(id, FREH_2017 = FREH_lag_raw, non_FREH_2017 = non_FREH_lag_raw, 
         price_2017 = price_lag_raw)
    
back_to_2017 <- 
  map(2018:2022, \(x) {
    dc$main |> 
      st_drop_geometry() |> 
      filter(year == x) |> 
      mutate(FREH_lag_raw = if_else(FREH_lag_dummy, 0, FREH_lag_raw),
             non_FREH_lag_raw = if_else(non_FREH_lag_dummy, 0, 
                                        non_FREH_lag_raw),
             price_lag_raw = if_else(price_lag_dummy, 0, price_lag_raw)) |> 
      inner_join(dc_2017, by = "id") |> 
      mutate(FREH_change_new = FREH_2017 - FREH_lag_raw,
             non_FREH_change_new = non_FREH_2017 - non_FREH_lag_raw,
             price_change_new = price_2017 - price_lag_raw) |> 
      select(id, FREH_change_new, non_FREH_change_new, price_change_new)}) |> 
  set_names(2018:2022)

effect_cum <- bind_rows(
  model_change(2018, 
               change_FREH = back_to_2017$`2018`$FREH_change_new, 
               change_non_FREH = back_to_2017$`2018`$non_FREH_change_new, 
               change_price = back_to_2017$`2018`$price_change_new),
  model_change(2019, 
               change_FREH = back_to_2017$`2019`$FREH_change_new, 
               change_non_FREH = back_to_2017$`2019`$non_FREH_change_new, 
               change_price = back_to_2017$`2019`$price_change_new),
  model_change(2020, 
               change_FREH = back_to_2017$`2020`$FREH_change_new, 
               change_non_FREH = back_to_2017$`2020`$non_FREH_change_new, 
               change_price = back_to_2017$`2020`$price_change_new),
  model_change(2021, 
               change_FREH = back_to_2017$`2021`$FREH_change_new, 
               change_non_FREH = back_to_2017$`2021`$non_FREH_change_new, 
               change_price = back_to_2017$`2021`$price_change_new),
  model_change(2022, 
               change_FREH = back_to_2017$`2022`$FREH_change_new, 
               change_non_FREH = back_to_2017$`2022`$non_FREH_change_new, 
               change_price = back_to_2017$`2022`$price_change_new))

# Average effect before pandemic
effect_yty |> 
  filter(year <= 2019) |> 
  pull(str_pct) |> 
  mean() |> 
  scales::percent(0.1)

# Average effect during pandemic
effect_yty |> 
  filter(year == 2020) |> 
  pull(str_pct) |> 
  scales::percent(0.1)

# Cumulative effect before pandemic
effect_cum |> 
  filter(year == 2019) |> 
  pull(str_pct) |> 
  scales::percent(0.1)

# Cumulative effect by 2022
effect_cum |> 
  filter(year == 2022) |> 
  pull(str_pct) |> 
  scales::percent(0.1)


# Figure 11: STR share of total rent --------------------------------------

fig_11 <-
  effect_yty |> 
  select(year, str_pct, str_share) |> 
  mutate(type = "Year-to-year effect") |> 
  bind_rows(
    effect_cum |>
      select(year, str_pct, str_share) |>
      mutate(type = "Cumulative effect")) |>
  mutate(type = factor(type, levels = c("Year-to-year effect",
                                        "Cumulative effect"))) |>
  ggplot(aes(year, str_pct, colour = str_pct > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 5) +
  geom_segment(aes(yend = 0), lwd = 2) +
  geom_label(
    aes(label = scales::dollar(str_share, 0.1, scale = 1/1000000, 
                               suffix = "M")),
    family = "Futura", nudge_y = c(-0.015, -0.015, 0.015, 0.015, -0.015, 
                                   -0.01, -0.01, 0.01, 0.01, -0.01)) +
  facet_wrap(~type, nrow = 2, scales = "free") +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  scale_colour_brewer(palette = "Dark2", direction = -1) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "none")

ggsave("output/figure_11.png", fig_11, width = 8, height = 4.5, units = "in")


# Size of STR market compared to LTR market -------------------------------

# Oct 2022 rent in CMHC neighbourhoods
monthly_sept |> 
  st_drop_geometry() |> 
  # Impute missing values
  impute() |> 
  # Update tenant_count to reflect trend in universe
  mutate(tenant_count = universe * tenant_count[year == 2021] / 
           universe[year == 2021], .by = id) |> 
  filter(year == 2022) |> 
  summarize(total_rent = sum(rent * tenant_count, na.rm = TRUE)) |> 
  pull() |> 
  scales::dollar(1000000)

# Sep 2022 STR host revenue in CMHC neighbourhoods
monthly_sept |> 
  filter(year == 2022) |> 
  pull(rev_count) |> 
  sum() |> 
  scales::dollar(100000)

# Ratio of the two
((monthly_sept |> 
    filter(year == 2022) |> 
    pull(rev_count) |> 
    sum()) / (monthly_sept |> 
                st_drop_geometry() |> 
                impute() |> 
                mutate(tenant_count = universe * tenant_count[year == 2021] / 
                         universe[year == 2021], .by = id) |> 
                filter(year == 2022) |> 
                summarize(total_rent = sum(
                  rent * tenant_count, na.rm = TRUE)) |> 
                pull())) |> 
  scales::percent(0.1)

# STR revenue as % of total residential revenue
((monthly_sept |> 
    filter(year == 2022) |> 
    pull(rev_count) |> 
    sum()) / ((monthly_sept |> 
                 st_drop_geometry() |> 
                 impute() |> 
                 mutate(tenant_count = universe * tenant_count[year == 2021] / 
                          universe[year == 2021], .by = id) |> 
                 filter(year == 2022) |> 
                 summarize(total_rent = sum(
                   rent * tenant_count, na.rm = TRUE)) |> 
                 pull()) + (monthly_sept |> 
                              filter(year == 2022) |> 
                              pull(rev_count) |> 
                              sum()))) |> 
  scales::percent(0.1)


# DiD implication setup ---------------------------------------------------

did_effects <- 
  tibble(treat = md$main$rent_log$group,
         year = md$main$rent_log$t,
         att = md$main$rent_log$att) |> 
  filter(year >= treat)

# Get tenant count to normalize per-neighbourhood rents
tenant_count <- 
  monthly_sept |> 
  st_drop_geometry() |> 
  # Impute missing values
  impute() |> 
  # Update tenant_count to reflect trend in universe
  mutate(tenant_count = universe * tenant_count[year == 2021] / 
           universe[year == 2021], .by = id) |> 
  select(id, year, tenant_count)

did_rent_dif <-
  dd$main |> 
  mutate(id = as.character(id)) |> 
  # Add effects to data
  inner_join(did_effects, by = c("year", "treat")) |> 
  # Create counterfactual for rent_log in the absence of treatment
  mutate(rent_log_cf = rent_log - att) |> 
  # Un-standardize rent_log_cf
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  # Exponentiate rent_log_cf_raw to get counterfactual rent
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  # Exponentiate rent_log_raw to get actual rent
  mutate(rent_raw = exp(rent_log_raw)) |>
  # Get rent difference
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, treat, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

did_FREH_effects <- 
  tibble(treat = md$main$FREH$group,
         year = md$main$FREH$t,
         att = md$main$FREH$att) |> 
  filter(year >= treat)

did_FREH_dif <-
  dd$main |> 
  mutate(id = as.character(id)) |> 
  inner_join(did_FREH_effects, by = c("year", "treat")) |> 
  mutate(FREH_cf = FREH - att) |> 
  mutate(FREH_cf_raw = FREH_cf * sd(dr$main$FREH_raw, na.rm = TRUE) + 
           mean(dr$main$FREH_raw, na.rm = TRUE)) |>
  mutate(FREH_dif = FREH_cf_raw - FREH_raw) |> 
  select(id, year, treat, FREH_raw, FREH_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

did_non_FREH_effects <- 
  tibble(treat = md$main$non_FREH$group,
         year = md$main$non_FREH$t,
         att = md$main$non_FREH$att) |> 
  filter(year >= treat)

did_non_FREH_dif <-
  dd$main |> 
  mutate(id = as.character(id)) |> 
  inner_join(did_non_FREH_effects, by = c("year", "treat")) |> 
  mutate(non_FREH_cf = non_FREH - att) |> 
  mutate(non_FREH_cf_raw = non_FREH_cf * 
           sd(dr$main$non_FREH_raw, na.rm = TRUE) + 
           mean(dr$main$non_FREH_raw, na.rm = TRUE)) |>
  mutate(non_FREH_dif = non_FREH_cf_raw - non_FREH_raw) |> 
  select(id, year, treat, non_FREH_raw, non_FREH_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))


# DiD first-year implications ---------------------------------------------

# Number/% of treated neighbourhoods
nrow(filter(did_rent_dif, year == treat))

# Average rent decrease in first year
did_rent_dif |> 
  filter(year == treat) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = scales::percent(
    total_rent_dif / (total_rent + total_rent_dif), 0.1))

# Average FREH decrease in first year
did_FREH_dif |> 
  filter(year == treat) |> 
  mutate(total_FREH = FREH_raw * tenant_count,
         total_FREH_dif = FREH_dif * tenant_count) |> 
  summarize(
    mean_FREH = weighted.mean(FREH_raw, tenant_count),
    mean_FREH_dif = weighted.mean(FREH_dif, tenant_count),
    total_FREH = sum(total_FREH),
    total_FREH_dif = sum(total_FREH_dif)) |> 
  mutate(pct = scales::percent(
    total_FREH_dif / (total_FREH + total_FREH_dif), 0.1))

# Average non-FREH decrease in first year
did_non_FREH_dif |> 
  filter(year == treat) |> 
  mutate(total_non_FREH = non_FREH_raw * tenant_count,
         total_non_FREH_dif = non_FREH_dif * tenant_count) |> 
  summarize(
    mean_non_FREH = weighted.mean(non_FREH_raw, tenant_count),
    mean_non_FREH_dif = weighted.mean(non_FREH_dif, tenant_count),
    total_non_FREH = sum(total_non_FREH),
    total_non_FREH_dif = sum(total_non_FREH_dif)) |> 
  mutate(pct = scales::percent(
    total_non_FREH_dif / (total_non_FREH + total_non_FREH_dif), 0.1))


# DiD 2022 implications ---------------------------------------------------

# Average rent decrease in 2022
did_rent_dif |> 
  filter(year == 2022) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = scales::percent(
    total_rent_dif / (total_rent + total_rent_dif), 0.1))

# Average FREH decrease in 2022
did_FREH_dif |> 
  filter(year == 2022) |> 
  mutate(total_FREH = FREH_raw * tenant_count,
         total_FREH_dif = FREH_dif * tenant_count) |> 
  summarize(
    mean_FREH = weighted.mean(FREH_raw, tenant_count),
    mean_FREH_dif = weighted.mean(FREH_dif, tenant_count),
    total_FREH = sum(total_FREH),
    total_FREH_dif = sum(total_FREH_dif)) |> 
  mutate(pct = scales::percent(
    total_FREH_dif / (total_FREH + total_FREH_dif), 0.1))

# Average non-FREH decrease in 2022
did_non_FREH_dif |> 
  filter(year == 2022) |> 
  mutate(total_non_FREH = non_FREH_raw * tenant_count,
         total_non_FREH_dif = non_FREH_dif * tenant_count) |> 
  summarize(
    mean_non_FREH = weighted.mean(non_FREH_raw, tenant_count),
    mean_non_FREH_dif = weighted.mean(non_FREH_dif, tenant_count),
    total_non_FREH = sum(total_non_FREH),
    total_non_FREH_dif = sum(total_non_FREH_dif)) |> 
  mutate(pct = scales::percent(
    total_non_FREH_dif / (total_non_FREH + total_non_FREH_dif), 0.1))


# Effect of regulations on non-regulated places ---------------------------

dyn <- aggte(md$main$rent_log, type = "dynamic")

did_rent_non_treated <-
  dr$main |> 
  anti_join(did_rent_dif, by = c("id")) |> 
  # Remove Georgina ON, because it removed its PR restriction
  filter(year == 2022) |> 
  mutate(att = dyn$att.egt[dyn$egt == 0]) |> 
  mutate(rent_log_cf = rent_log - att) |> 
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  mutate(rent_raw = exp(rent_log_raw)) |> 
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

# Possible average rent decrease in 2022
did_rent_non_treated |> 
  filter(year == 2022) |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif)) |> 
  mutate(pct = scales::percent(
    total_rent_dif / (total_rent + total_rent_dif), 0.1))


# Table 5: Regional breakdown ---------------------------------------------

prov_FREH <-
  model_change(2022, province, use_non_FREH = FALSE, use_price = FALSE) |> 
  mutate_region() |> 
  filter(!is.na(region)) |> 
  summarize(across(total_before:str_share, sum),
            str_pct = str_share / total_change,
            .by = region) |> 
  mutate(total_change = scales::dollar(total_change, 0.1, scale = 1/1000000, 
                                       suffix = "M"),
         str_share = scales::dollar(str_share, 0.1, scale = 1/1000000, 
                                    suffix = "M"),
         str_pct = scales::percent(str_pct, 0.1)) |> 
  mutate(var = paste0(str_share, " (", str_pct, ")")) |> 
  select(region, total_change, var) |> 
  set_names(c("Region", "Total rent increase", "Estimated FREH contribution"))

prov_non_FREH <-
  model_change(2022, province, use_FREH = FALSE, use_price = FALSE) |> 
  mutate_region() |> 
  filter(!is.na(region)) |> 
  summarize(across(total_before:str_share, sum),
            str_pct = str_share / total_change,
            .by = region) |> 
  mutate(total_change = scales::dollar(total_change, 0.1, scale = 1/1000000, 
                                       suffix = "M"),
         str_share = scales::dollar(str_share, 0.1, scale = 1/1000000, 
                                    suffix = "M"),
         str_pct = scales::percent(str_pct, 0.1)) |> 
  mutate(var = paste0(str_share, " (", str_pct, ")")) |> 
  select(region, total_change, var) |> 
  set_names(c("Region", "Total rent increase", 
              "Estimated non-FREH contribution"))

prov_price <-
  model_change(2022, province, use_FREH = FALSE, use_non_FREH = FALSE) |> 
  mutate_region() |> 
  filter(!is.na(region)) |> 
  summarize(across(total_before:str_share, sum),
            str_pct = str_share / total_change,
            .by = region) |> 
  mutate(total_change = scales::dollar(total_change, 0.1, scale = 1/1000000, 
                                       suffix = "M"),
         str_share = scales::dollar(str_share, 0.1, scale = 1/1000000, 
                                    suffix = "M"),
         str_pct = scales::percent(str_pct, 0.1)) |> 
  mutate(var = paste0(str_share, " (", str_pct, ")")) |> 
  select(region, total_change, var) |> 
  set_names(c("Region", "Total rent increase", "Estimated price contribution"))

prov_FREH |> 
  inner_join(prov_non_FREH, by = c("Region", "Total rent increase")) |> 
  inner_join(prov_price, by = c("Region", "Total rent increase")) |> 
  gt::gt()
