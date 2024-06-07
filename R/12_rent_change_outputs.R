#### RENT CHANGE MODEL OUTPUTS #################################################

source("R/01_startup.R")
source("R/05_process_DAGs.R")
mc <- qread("output/mc.qs", nthreads = availableCores())


### Model summary ##############################################################


# Table 3: RE-ESF model ---------------------------------------------------

# Main results
mc$common.1

# Rounded parameters
mc$common.1$b |> 
  as_tibble(rownames = "var") |> 
  mutate(low = Estimate - SE * 1.96, high = Estimate + SE * 1.96) |> 
  mutate(across(-var, \(x) round(x, 3)))


# Figure 7: Parameter estimates -------------------------------------------

fig_7 <-
  names(ac) |> 
  map(\(x) {
    mc[[x]]$b |> 
      as_tibble(rownames = "var") |> 
      mutate(model = x)}) |> 
  bind_rows() |> 
  filter(var %in% c("FREH_change", "non_FREH_change", "price_change")) |> 
  mutate(down = Estimate - 1.96 * SE, up = Estimate + 1.96 * SE) |> 
  mutate(model = factor(model, levels = names(ac))) |> 
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

ggsave("output/figure_7.png", fig_7, width = 8, height = 4, units = "in")
