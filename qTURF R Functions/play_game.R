#' play_game
#'
#' @param turf_effort effort in a particular TURF
#' @param which_turf which TURF is being modified
#' @param scene
#' @param patches
#' @param time
#' @param start_pop starting population
#' @param effort effort vector for both TURFs
#' @param kmode
#'
#' @return
#' @export profits for the turf in question
#'
play_game = function(turf_effort,
                     which_turf,
                     scene,
                     patches,
                     time,
                     start_pop,
                     effort,
                     kmode = 'global',
                     stock_effect) {

  effort[which_turf] = turf_effort

  game_result = sim_pop(
    scene = scene,
    patches = patches,
    start_pop = start_pop,
    effort = effort,
    time = time,
    kmode = kmode,
    stock_effect
  ) %>%
    filter(year == max(year)  & turf == letters[which_turf]) %>%
    select(profits)

  return(-game_result$profits)

}