#### 13 DIFFERENCE-IN-DIFFERENCES OUTPUTS ######################################

source("R/01_startup.R")
dd <- qread("output/dd.qs")
md <- qread("output/md.qs")


# Results -----------------------------------------------------------------

ad <- map(md, map, \(x) aggte(x, type = "simple", na.rm = TRUE, alp = .05))


# Figure 6: Parallel trends assumption ------------------------------------

fig_6 <- 
  aggte(md$main$rent_log, type = "dynamic") |> 
  ggdid() +
  ggtitle(NULL) +
  theme_minimal() +
  scale_x_continuous(name = "Years post-treatment", 
                     breaks = c(-6, -4, -2, 0, 2, 4)) + 
  scale_y_continuous(name = "ATT") +
  scale_color_brewer(name = NULL, palette = "Dark2", labels = c(
    "Pre-treatment", "Post-treatment")) +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom")

ggsave("output/figure_6.png", fig_6, width = 8, height = 4, units = "in")


# Table 3 -----------------------------------------------------------------

tibble(
  var = names(ad$main),
  att = round(map_dbl(ad$main, \(x) x$overall.att), 3),
  se = round(map_dbl(ad$main, \(x) x$overall.se), 3))

ad$main

# Maximum confidence intervals
aggte(md$main$rent_log, type = "simple", na.rm = TRUE, alp = .001)
aggte(md$main$FREH, type = "simple", na.rm = TRUE, alp = .001)
aggte(md$main$non_FREH, type = "simple", na.rm = TRUE, alp = .001)
aggte(md$main$price, type = "simple", na.rm = TRUE, alp = .1)


# Figure 9: Calendar effects ----------------------------------------------

cal <- list()
cal$main <- aggte(md$main$rent_log, type = "calendar")
cal$van <- aggte(md$van$rent_log, type = "calendar")
cal$FREH <- aggte(md$main$FREH_log, type = "calendar")
cal$van_FREH <- aggte(md$van$FREH_log, type = "calendar")
cal$non_FREH <- aggte(md$main$non_FREH_log, type = "calendar")
cal$van_non_FREH <- aggte(md$van$non_FREH_log, type = "calendar")

fig_9 <-
  tibble(year = 2019:2023, att = cal$main$att.egt, SE = cal$main$se.egt, 
         var = "rent_log", model = "Main") |> 
  bind_rows(tibble(year = 2019:2023, att = cal$van$att.egt, 
                   SE = cal$van$se.egt, var = "rent_log",
                   model = "Vancouver-adjusted")) |> 
  bind_rows(tibble(year = 2019:2022, att = cal$FREH$att.egt, 
                   SE = cal$FREH$se.egt, var = "FREH_log", model = "Main")) |> 
  bind_rows(tibble(year = 2019:2022, att = cal$van_FREH$att.egt, 
                   SE = cal$van_FREH$se.egt,
                   var = "FREH_log", model = "Vancouver-adjusted")) |> 
  bind_rows(tibble(year = 2019:2022, att = cal$non_FREH$att.egt, 
                   SE = cal$non_FREH$se.egt, var = "non_FREH_log", 
                   model = "Main")) |> 
  bind_rows(tibble(year = 2019:2022, att = cal$van_non_FREH$att.egt, 
                   SE = cal$van_non_FREH$se.egt,
                   var = "non_FREH_log", model = "Vancouver-adjusted")) |> 
  mutate(down = att - 1.96 * SE, up = att + 1.96 * SE) |> 
  mutate(var = factor(var, levels = c("rent_log", "FREH_log", "non_FREH_log",
                                      "price_log")), 
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

ggsave("output/figure_9.png", fig_9, width = 8, height = 4, units = "in")
