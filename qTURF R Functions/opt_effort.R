#' opt_effort
#'
#' @param effort vector of effort in each patch
#' @param scene the scenario being run
#' @param patches
#' @param time
#' @param start_pop
#' @param kmode
#'
#' @return negative total profits
#' @export

opt_effort = function(effort,
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
    effort = effort,
    time = time,
    stock_effect = stock_effect,
    kmode = kmode
  ) %>%
    filter(year == max(year)) %>%
    select(profits)

  return(-sum(game_result$profits))

}