opt_effort = function(effort,
                     scene,
                     patches,
                     time,
                     start_pop,
                     kmode = 'global') {

  game_result = sim_pop(
    scene = scene,
    patches = patches,
    start_pop = start_pop,
    effort = effort,
    time = time
  ) %>%
    filter(year == max(year)) %>%
    select(profits)

  return(-sum(game_result$profits))

}