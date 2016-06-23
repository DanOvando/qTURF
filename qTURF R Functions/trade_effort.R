trade_effort = function(total_effort,
                        scene,
                        patches,
                        time,
                        start_pop,
                        kmode = 'global',
                        stock_effect) {
  #  tuned_theta = uniroot(
  #   tune_theta,
  #   interval = c(02, 20),
  #   total_effort = total_effort,
  #   scene = scene,
  #   patches = patches,
  #   time = time,
  #   start_pop = start_pop,
  #   kmode = kmode,
  #   stock_effect = stock_effect
  # )

  tuned_theta = nlminb(
    1,
    tune_theta,
    lower = 0,
    upper = 2,
    total_effort = total_effort,
    scene = scene,
    patches = patches,
    time = time,
    start_pop = start_pop,
    kmode = kmode,
    stock_effect = stock_effect
  )

  # if (abs(tuned_theta$f.root) > 1e-4) {
  #   warning('theta failed to tune')
  # }
  #
  # theta = min(1, tuned_theta$root)

  theta = min(1, tuned_theta$par)

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
    select(profits)

  return(-sum(game_result$profits))

}