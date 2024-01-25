#### LINEAR MODELS #############################################################

source("R/04_data_for_models.R")

ml <- list()


# rent models -------------------------------------------------------------

# FREH: positive relationship
ml$rent_FREH <- lm(
  log_rent ~ log_FREH_share + year + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation + CMA, data = data_rent)

# Rev: positive relationship but insignificant
ml$rent_rev <- lm(
  log_rent ~ log_rev_share + year + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation + CMA, data = data_rent)

# Both: positive relationship FREH, negative rev
ml$rent_both <- lm(
  log_rent ~ log_FREH_share + log_rev_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + CMA, data = data_rent)

# Both with zone controls: positive relationship FREH, negative rev
ml$rent_both_id <- lm(
  log_rent ~ log_FREH_share + log_rev_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + id, data = data_rent)

summary(ml$rent_FREH)
summary(ml$rent_rev)
summary(ml$rent_both)
summary(ml$rent_both_id)


# rent_change_pct models --------------------------------------------------

# FREH: small positive relationship
ml$change_FREH <- lm(
  rent_change_pct ~ FREH_change_pct + FREH_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + CMA, data = data_change)

# Rev: no relationship (log or no log)
ml$change_rev <- lm(
  rent_change_pct ~ rev_change_pct + rev_share + year + log_universe + 
   tenant_share + log_pop_CMA + log_accommodation + CMA, data = data_change)

# Both: positive relationship FREH, no relationship rev
ml$change_both <- lm(
  rent_change_pct ~ FREH_change_pct + FREH_share + rev_change_pct + rev_share + 
    year + log_universe + tenant_share + log_pop_CMA + log_accommodation + CMA, 
  data = data_change)

# Both with zone controls: positive relationship FREH, no relationship rev
ml$change_both_id <- lm(
  rent_change_pct ~ FREH_change_pct + FREH_share + rev_change_pct + rev_share + 
    year + log_universe + tenant_share + log_pop_CMA + log_accommodation + id, 
  data = data_change)

summary(ml$change_FREH)
summary(ml$change_rev)
summary(ml$change_both)
summary(ml$change_both_id)


# rent models by province -------------------------------------------------

provinces <- list(
  "British Columbia", 
  c("Alberta", "Saskatchewan", "Manitoba"),
  "Ontario", 
  "Quebec", 
  c("New Brunswick", "Nova Scotia", "Prince Edward Island", 
    "Newfoundland and Labrador"))

# FREH
ml$rent_FREH_p <- map(provinces, \(x) lm(
  log_rent ~ log_FREH_share + year + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation + CMA, data = filter(data_rent, province %in% x)))
  
# Rev
ml$rent_rev_p <- map(provinces, \(x) lm(
  log_rent ~ log_rev_share + year + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation + CMA, data = filter(data_rent, province %in% x)))

# Both
ml$rent_both_p <- map(provinces, \(x) lm(
  log_rent ~ log_FREH_share + log_rev_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + CMA, 
  data = filter(data_rent, province %in% x)))

# Both with zone controls
ml$rent_both_id_p <- map(provinces, \(x) lm(
  log_rent ~ log_FREH_share + log_rev_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + id, 
  data = filter(data_rent, province %in% x)))

map(ml$rent_FREH_p, summary)
map(ml$rent_rev_p, summary)
map(ml$rent_both_p, summary)
map(ml$rent_both_id_p, summary)


# rent_change_pct models by province --------------------------------------

# FREH
ml$change_FREH_p <- map(provinces, \(x) lm(
  rent_change_pct ~ FREH_change_pct + FREH_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + CMA, 
  data = filter(data_change, province %in% x)))

# Rev
ml$change_rev_p <- map(provinces, \(x) lm(
  rent_change_pct ~ rev_change_pct + rev_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + CMA, 
  data = filter(data_change, province %in% x)))

# Both
ml$change_both_p <- map(provinces, \(x) lm(
  rent_change_pct ~ FREH_change_pct + FREH_share + rev_change_pct + rev_share + 
    year + log_universe + tenant_share + log_pop_CMA + log_accommodation + CMA, 
  data = filter(data_change, province %in% x)))

# Both with zone controls
ml$change_both_id_p <- map(provinces, \(x) lm(
  rent_change_pct ~ FREH_change_pct + FREH_share + rev_change_pct + rev_share + 
    year + log_universe + tenant_share + log_pop_CMA + log_accommodation + id, 
  data = filter(data_change, province %in% x)))

map(ml$change_FREH_p, summary)
map(ml$change_rev_p, summary)
map(ml$change_both_p, summary)
map(ml$change_both_id_p, summary)
