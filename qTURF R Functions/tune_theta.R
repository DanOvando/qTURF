tune_theta = function(theta,
                      total_effort,
                      scene,
                      patches,
                      time,
                      start_pop,
                      kmode = 'global',
                      stock_effect) {

  game_result = sim_pop(
    scene = scene,
    patches = patches,
    start_pop = start_pop,
    effort = total_effort * c(theta, 1 - theta),
    time = time,
    stock_effect = stock_effect,
    kmode = kmode
  ) %>%
    filter(year == max(year)) %>%
    select(marginal_profits)

  # return(diff(game_result$marginal_profits))

  return(diff(game_result$marginal_profits)^2)


}