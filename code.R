library(tidyverse)
library(brms)
library(rstan)

d1 = read_csv("https://raw.githubusercontent.com/pennekampster/Code_and_data_OverallEcosystemStability/refs/heads/master/data/total_biomass_BEEP_OES.csv")
d2 = read_csv("https://raw.githubusercontent.com/pennekampster/Code_and_data_OverallEcosystemStability/refs/heads/master/data/species_biomass_BEEP_OES.csv")

dd1 = d1 %>% group_by(microcosmID) %>% 
  mutate(cum.bio = sum(total_biomass)) %>% 
  ungroup() %>% 
  filter(cum.bio > 0) 

m1 = brm(
  data = dd1 %>% filter(temperature == 19 & richness == 6)
  ,bf(
    total_biomass ~ s(day, k = 8) + (day|microcosmID)
  ),
  family = lognormal(),
  iter = 8000,
  warmup = 4000,
  chains = 4, 
  cores = 4, 
  seed = 123,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.99,
                 max_treedepth = 10)
  ,file = "fits/m1"
)
pp_check(m1, ndraws = 100)
summary(m1)
plot(conditional_effects(m1), points = T)



m2 = brm(
  data = dd1 %>% filter(temperature == 19)
  ,bf(
    total_biomass ~ t2(day, richness) + (day|combination/microcosmID),
    hu ~ t2(day, richness) + (day|combination/microcosmID),
    sigma ~ 1 + (1|combination)
  ),
  family = hurdle_lognormal(),
  iter = 8000,
  warmup = 4000,
  chains = 4, 
  cores = 4, 
  seed = 123,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.99,
                 max_treedepth = 10)
  ,file = "fits/m2"
)
pp_check(m2, ndraws = 100)
summary(m2)
plot(conditional_effects(m2, effects = "day:richness",
                         spaghetti = T, 
                         ndraws = 300,
                         int_conditions = data.frame(richness = 1:6)), 
     points = T)

# plot(conditional_effects(m2, effects = "day:richness",
#                          surface = T), 
#      stype = "raster")

m3 = brm(
  data = dd1
  ,bf(
    total_biomass ~ t2(day, temperature, richness) + (day|combination/microcosmID),
    hu ~ t2(day, temperature, richness) + (day|combination/microcosmID),
    sigma ~ 1 + (1|combination)
  ),
  family = hurdle_lognormal(),
  iter = 8000,
  warmup = 4000,
  chains = 4, 
  cores = 4, 
  seed = 123,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.99,
                 max_treedepth = 10)
  ,file = "fits/m3"
)
pp_check(m3, ndraws = 100)
summary(m3)
plot(conditional_effects(m3, effects = "day:richness",
                         spaghetti = T, 
                         ndraws = 300,
                         int_conditions = data.frame(richness = 1:6)), 
     points = T)