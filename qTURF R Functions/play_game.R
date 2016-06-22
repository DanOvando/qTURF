play_game = function(turf_effort,
                     which_turf,
                     scene,
                     patches,
                     time,
                     start_pop,
                     effort,
                     kmode = 'global') {

  effort[which_turf] = turf_effort

  game_result = sim_pop(
    scene = scene,
    patches = patches,
    start_pop = start_pop,
    effort = effort,
    time = time,
    kmode = kmode
  ) %>%
    filter(year == max(year)  & turf == letters[which_turf]) %>%
    select(profits)

  return(-game_result$profits)

}