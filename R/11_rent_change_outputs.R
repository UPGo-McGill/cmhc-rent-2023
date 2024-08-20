#### 11 RENT CHANGE MODEL OUTPUTS ##############################################

source("R/08_process_DAGs.R")
mc <- qread("output/mc.qs", nthreads = availableCores())


### Model summary ##############################################################


# Table 3: RE-ESF model ---------------------------------------------------

# Main results
mc$common.1

# Rounded parameters
mc$common.1$b |> 
  as_tibble(rownames = "var") |> 
  mutate(low = Estimate - SE * 1.96, high = Estimate + SE * 1.96) |> 
  mutate(stars = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    p_value < 0.1 ~ "+",
    .default = "")) |> 
  mutate(across(c(Estimate, SE, low, high), \(x) scales::comma(x, 0.001))) |> 
  mutate(Estimate = paste0(Estimate, stars),
         SE = paste0("(", SE, ")")) |> 
  select(-t_value, -p_value, -stars) |> 
  gt::gt()

nrow(mc$common.1$sf)


# Figure 7: Parameter estimates -------------------------------------------

fig_8 <-
  names(ac) |> 
  map(\(x) {
    mc[[x]]$b |> 
      as_tibble(rownames = "var") |> 
      mutate(model = x)}) |> 
  bind_rows() |> 
  filter(var %in% c("FREH_change", "non_FREH_change", "price_change")) |> 
  mutate(down = Estimate - 1.96 * SE, up = Estimate + 1.96 * SE) |> 
  mutate(model = case_when(
    model == "common.1" ~ "Main",
    model == "FREH_non_FREH.2" ~ "FREH-1",
    model == "FREH_non_FREH.3" ~ "FREH-2",
    model == "price.2" ~ "Price-1",
    model == "price.3" ~ "Price-2")) |> 
  mutate(model = factor(model, levels = c(
    "Main", "FREH-1", "FREH-2", "Price-1", "Price-2"))) |> 
  ggplot(aes(var, Estimate, colour = model)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_point(position = position_dodge(0.25)) +
  geom_errorbar(aes(ymin = down, ymax = up), width = 0.2, 
                position = position_dodge(0.25)) +
  scale_color_brewer(name = NULL, palette = "Dark2") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Estimate") +
  scale_linetype(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "bottom")

ggsave("output/figure_8.png", fig_8, width = 8, height = 4, units = "in")
