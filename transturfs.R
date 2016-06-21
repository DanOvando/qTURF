

# transturfs --------------------------------------------------------------
# A wrapper for dealing with a better version of TURF itqs

library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)
library(purrr)

# Initialize --------------------------------------------------------------

turf = c('a', 'b')

patches = data.frame(turf = turf,
                     k = c(100, 100),
                     r = c(0.2, 0.2))

movement = c('m0', 'm1', 'm2', 'm3')

move_mat = expand.grid(turf = turf, movement = movement) %>%
  mutate(move_rate = c(0, 0, 0.5, 0.5, 0.2, 0.05, 0.05, 0.2))

skill = c('same', 'different')

# coop = c(1, 0)
#
# trade = c(1, 0)

base_q = 0.1

q_diff = 1.5

base_price = 1

base_cost = .1

no_coop_cost = 0.5

skill_mat = expand.grid(turf = turf, skill = skill) %>%
  mutate(q = c(base_q, base_q, q_diff * base_q, base_q))

runs = expand.grid(
  turf = turf,
  movement = movement,
  skill = skill
  # coop = coop,
  # trade = trade
) %>%
  left_join(skill_mat, by = c('turf', 'skill')) %>%
  left_join(move_mat, by = c('turf', 'movement')) %>%
  mutate(
    run = paste(movement, skill, sep = '-')
    # price = base_price,
    # cost = base_cost * (1 + (as.numeric(coop == 0) * no_coop_cost))
  )

# run model ---------------------------------------------------------------

scenes = unique(runs$run)

