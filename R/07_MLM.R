#### MULTI-LEVEL MODELS ########################################################

library(lme4)
library(r2mlm)

mm <- list()


# rent models -------------------------------------------------------------

# FREH: positive relationship
mm$rent_FREH <- lmer(
  log_rent ~ log_FREH_share + year + log_universe + tenant_share + 
    log_accommodation + (1 | CMA), data = data_rent)

# Rev: positive relationship
mm$rent_rev <- lmer(
  log_rent ~ log_rev_share + year + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation + (1 | CMA), data = data_rent)

# Both: positive relationship FREH, negative rev
mm$rent_both <- lmer(
  log_rent ~ log_FREH_share + log_rev_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + (1 | CMA), 
  data = data_rent)

# Both with zone fixed effects: positive relationship FREH, negative rev
mm$rent_both_id <- lmer(
  log_rent ~ log_FREH_share + log_rev_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + (1 | id), data = data_rent)

# Both with 3-level CMA/zone model: positive relationship FREH, negative rev
mm$rent_both_3z <- lmer(
  log_rent ~ log_FREH_share + log_rev_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + (1 | CMA / id), 
  data = data_rent)

# Both with 3-level prov/CMA model: positive relationship FREH, negative rev
mm$rent_both_3p <- lmer(
  log_rent ~ log_FREH_share + log_rev_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + (1 | province / CMA), 
  data = data_rent)

summary(mm$rent_FREH)
summary(mm$rent_rev)
summary(mm$rent_both)
summary(mm$rent_both_id)
summary(mm$rent_both_3z)
summary(mm$rent_both_3p)

stargazer::stargazer(mm$rent_FREH, type = "text")
stargazer::stargazer(mm$rent_rev, type = "text")
stargazer::stargazer(mm$rent_both, type = "text")
stargazer::stargazer(mm$rent_both_id, type = "text")
stargazer::stargazer(mm$rent_both_3z, type = "text")
stargazer::stargazer(mm$rent_both_3p, type = "text")

# Not working
r2mlm(mm$rent_FREH)
r2mlm(mm$rent_rev)
r2mlm(mm$rent_both)
r2mlm(mm$rent_both_id)
r2mlm(mm$rent_both_3z)
r2mlm(mm$rent_both_3p)


# rent_change_pct models --------------------------------------------------

# FREH: small positive relationship
mm$change_FREH <- lmer(
  rent_change_pct ~ FREH_change_pct + FREH_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + (1 | CMA), 
  data = data_change)

# Rev: no relationship (log or no log)
mm$change_rev <- lmer(
  rent_change_pct ~ 1 + rev_change_pct + rev_share + year + log_universe + 
    tenant_share + log_pop_CMA + log_accommodation + (1 | CMA), 
  data = data_change)

# Both: positive relationship FREH, no relationship rev
mm$change_both <- lmer(
  rent_change_pct ~ 1 + FREH_change_pct + FREH_share + rev_change_pct +
    rev_share + year + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation + (1 | CMA), data = data_change)

# Both with zone fixed effects: positive relationship FREH, no relationship rev
mm$change_both_id <- lmer(
  rent_change_pct ~ 1 + FREH_change_pct + FREH_share + rev_change_pct +
    rev_share + year + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation + (1 | id), data = data_change)

# Both with 3-level CMA/zone model: positive relationship FREH, no relationship rev
mm$change_both_3z <- lmer(
  rent_change_pct ~ 1 + FREH_change_pct + FREH_share + rev_change_pct +
    rev_share + year + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation + (1 | CMA / id), data = data_change)

# Both with 3-level prov/CMA model: positive relationship FREH, no relationship rev
mm$change_both_3p <- lmer(
  rent_change_pct ~ 1 + FREH_change_pct + FREH_share + rev_change_pct +
    rev_share + year + log_universe + tenant_share + log_pop_CMA + 
    log_accommodation + (1 | province / CMA), data = data_change)

summary(mm$change_FREH)
summary(mm$change_rev)
summary(mm$change_both)
summary(mm$change_both_id)
summary(mm$change_both_3z)
summary(mm$change_both_3p)

stargazer::stargazer(mm$change_FREH, type = "text")
stargazer::stargazer(mm$change_rev, type = "text")
stargazer::stargazer(mm$change_both, type = "text")
stargazer::stargazer(mm$change_both_id, type = "text")
stargazer::stargazer(mm$change_both_3z, type = "text")
stargazer::stargazer(mm$change_both_3p, type = "text")

# Not working
r2mlm(mm$change_FREH)
r2mlm(mm$change_rev)
r2mlm(mm$change_both)
r2mlm(mm$change_both_id)
r2mlm(mm$change_both_3z)
r2mlm(mm$change_both_3p)
