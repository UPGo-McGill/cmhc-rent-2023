#### 13 DIFFERENCE-IN-DIFFERENCES MODELS #######################################

source("R/01_startup.R")
source("R/08_data_for_models.R")
monthly_sept <- qread("output/monthly_sept.qs")
qload("output/cmhc.qsm", nthreads = availableCores())


# Prepare data ------------------------------------------------------------

reg <-
  qread("data/reg.qs") |> 
  mutate(reg = if_else(reg == "TBD", FALSE, as.logical(reg))) |> 
  mutate(date = if_else(date >= "2022-01-02", NA, date)) |> 
  mutate(reg = if_else(is.na(date), FALSE, TRUE)) |> 
  inner_join(st_drop_geometry(cmhc_nbhd), by = c("id", "name")) |> 
  select(-c(pop:tenant))

dd <-
  map(dr, \(x) {
    x |> 
      inner_join(select(reg, id, date, reg), by = "id") |> 
      mutate(treat = if_else(is.na(date), 0, year(date - 1) + 1), 
             id = as.numeric(id)) |> 
      filter(treat == 0 | treat > 2017) |> 
      # Remove provinces with no treatment in the time period
      filter(!province %in% c("Manitoba", "Saskatchewan", "Nova Scotia", 
                              "Prince Edward Island", "Alberta",
                              "Newfoundland and Labrador"))
  })

dd$all <- 
  dr$main |> 
  inner_join(select(reg, id, date, reg), by = "id") |> 
  mutate(treat = if_else(is.na(date), 0, year(date - 1) + 1), 
         id = as.numeric(id))

# Variant with Vancouver treatment delayed by a year to account for slow
# regulatory progress
dd$van <- 
  dr$main |> 
  inner_join(select(mutate(reg, date = if_else(
    name_CSD == "Vancouver", as.Date("2019-08-31"), date)), id, date, reg), 
    by = "id") |> 
  mutate(treat = if_else(is.na(date), 0, year(date - 1) + 1), 
         id = as.numeric(id)) |> 
  filter(treat == 0 | treat > 2017) |> 
  # Remove provinces with no treatment in the time period
  filter(!province %in% c("Manitoba", "Saskatchewan", "Nova Scotia", 
                          "Prince Edward Island", "Alberta",
                          "Newfoundland and Labrador"))


# Fit models --------------------------------------------------------------

md_vars <- c("rent_log", "FREH", "non_FREH", "price")

md <- map(dd, \(y) {
  md_vars |> 
    set_names(md_vars) |> 
    map(\(x) att_gt(x, tname = "year", idname = "id", gname = "treat", 
                    allow_unbalanced_panel = TRUE, data = y))}) |> 
  set_names(names(dd))


# Save output -------------------------------------------------------------

qsave(dd, "output/dd.qs")
qsave(md, "output/md.qs")


# Results -----------------------------------------------------------------

ad <- map(md, map, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .05))

# Maximum confidence intervals
aggte(md$main$rent_log, type = "simple", na.rm = TRUE, alp = .001)
aggte(md$main$FREH, type = "simple", na.rm = TRUE, alp = .001)
aggte(md$main$non_FREH, type = "simple", na.rm = TRUE, alp = .001)
aggte(md$main$price, type = "simple", na.rm = TRUE, alp = .1)


# Table 4 -----------------------------------------------------------------

tibble(
  var = names(ad_95),
  att = round(map_dbl(ad_95, \(x) x$overall.att), 3),
  se = round(map_dbl(ad_95, \(x) x$overall.se), 3)
)


# Figure 8: Calendar effects ----------------------------------------------

cal <- list()
cal$main <- aggte(md$main$rent_log, type = "calendar")
cal$van <- aggte(md$van$rent_log, type = "calendar")
cal$FREH <- aggte(md$main$FREH, type = "calendar")
cal$van_FREH <- aggte(md$van$FREH, type = "calendar")

fig_8 <-
  tibble(year = 2019:2023, att = cal$main$att.egt, SE = cal$main$se.egt, 
         var = "rent_log", model = "Main") |> 
  bind_rows(tibble(year = 2019:2023, att = cal$van$att.egt, 
                   SE = cal$van$se.egt, var = "rent_log",
                   model = "Vancouver-adjusted")) |> 
  bind_rows(tibble(year = 2019:2022, att = cal$FREH$att.egt, 
                   SE = cal$FREH$se.egt, var = "FREH", model = "Main")) |> 
  bind_rows(tibble(year = 2019:2022, att = cal$van_FREH$att.egt, 
                   SE = cal$van_FREH$se.egt,
                   var = "FREH", model = "Vancouver-adjusted")) |> 
  mutate(down = att - 1.96 * SE, up = att + 1.96 * SE) |> 
  mutate(var = factor(var, levels = c("rent_log", "FREH")), 
         model = factor(model, levels = c("Main", "Vancouver-adjusted"))) |> 
  ggplot(aes(year, att, colour = model)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_point(position = position_dodge(0.25)) +
  geom_errorbar(aes(ymin = down, ymax = up), width = 0.2, 
                position = position_dodge(0.25)) +
  facet_wrap(~var, nrow = 1, scales = "free") +
  scale_color_brewer(name = NULL, palette = "Dark2") +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Estimate") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "bottom")

ggsave("output/figure_8.png", fig_8, width = 8, height = 4, units = "in")


# Figure 9: Parallel trends assumption ------------------------------------

fig_9 <- 
  aggte(md$main$rent_log, type = "dynamic") |> 
  ggdid() +
  ggtitle(NULL) +
  theme_minimal() +
  scale_x_continuous(name = "Years post-treatment") + 
  scale_y_continuous(name = "ATT") +
  scale_color_brewer(name = NULL, palette = "Dark2", labels = c(
    "Pre-treatment", "Post-treatment")) +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom")

ggsave("output/figure_9.png", fig_9, width = 8, height = 4, units = "in")
