#' run_scene runs a given scenario through its various policy options
#'
#' @param run_name the run being run
#' @param runs the matrix of possible runs
#' @param time number of years to run the model for
#' @param no_coop_cost the cost augmentation of not cooperating
#' @param patches data frame of patch specific factors
#' @param kmode whether carrying capacity is patch specific or global
#'
#' @return out a dataframe of results
#' @export
#'
run_scene = function(run_name,
                     runs,
                     time = 50,
                     no_coop_cost = 0.5,
                     patches,
                     kmode = 'global') {

  # run_name = scenes[1]

  show(run_name)
  scene = filter(runs, run == run_name)

  scene$price = 1

  scene$cost = .1

  eq_pop = sim_pop(
    scene = scene,
    patches = patches,
    start_pop = patches$k,
    kmode = kmode,
    time = time
  ) %>%
    filter(year == max(year)) %>%
    select(turf, biomass) %>%
    spread(turf, biomass)

  # non cooperative game ----------------------------------------------------

  non_coop_game_scene = scene

  non_coop_game_scene$coop = c(0, 0)

  non_coop_game_scene = non_coop_game_scene %>%
    mutate(cost = base_cost * (1 + (as.numeric(coop == 0) * no_coop_cost)))

  non_coop_game = run_game(
    scene = non_coop_game_scene,
    patches = patches,
    eq_pop = eq_pop,
    kmode = kmode,
    time = time
  )

  non_coop_game_result = sim_pop(
    scene = non_coop_game_scene,
    patches = patches,
    start_pop = eq_pop,
    effort = non_coop_game,
    time = time,
    kmode = kmode
  ) %>%
    filter(year == max(year)) %>%
    mutate(policy = 'non coop game')


  # ggplot(non_coop_game_result, aes(year,profits, color = turf)) +
  #   geom_line(size = 2)

  # cooperative game --------------------------------------------------------

  coop_game_scene = scene

  coop_game_scene$coop = c(1, 1)

  coop_game_scene = coop_game_scene %>%
    mutate(cost = base_cost * (1 + (as.numeric(coop == 0) * no_coop_cost)))

  coop_game = run_game(
    scene = coop_game_scene,
    patches = patches,
    eq_pop = eq_pop,
    kmode = kmode,
    time = time
  )

  coop_game_result = sim_pop(
    scene = coop_game_scene,
    patches = patches,
    start_pop = eq_pop,
    effort = non_coop_game,
    time = time,
    kmode = kmode
  ) %>%
    filter(year == max(year)) %>%
    mutate(policy = 'coop game')




  # cooperative trading -----------------------------------------------------------------


  coop_trade = nlminb(
    c(0.2, 0.2),
    objective = opt_effort,
    scene = coop_game_scene,
    patches = patches,
    time = time,
    start_pop = eq_pop,
    kmode = kmode,
    lower = c(0, 0),
    upper = c(10, 10)
  )


  coop_trading_result = sim_pop(
    scene = coop_game_scene,
    patches = patches,
    start_pop = eq_pop,
    effort = coop_trade$par,
    time = time,
    kmode = kmode
  ) %>%
    filter(year == max(year)) %>%
    mutate(policy = 'coop trading')

  # noncooperative trading --------------------------------------------------

  non_coop_trade = nlminb(
    c(0.2, 0.2),
    objective = opt_effort,
    scene = non_coop_game_scene,
    patches = patches,
    time = time,
    start_pop = eq_pop,
    kmode = kmode,
    lower = c(0, 0),
    upper = c(10, 10)
  )

  non_coop_trading_result = sim_pop(
    scene = coop_game_scene,
    patches = patches,
    start_pop = eq_pop,
    effort = non_coop_trade$par,
    time = time,
    kmode = kmode
  ) %>%
    filter(year == max(year)) %>%
    mutate(policy = 'non coop trade')

  out = non_coop_game_result %>%
    bind_rows(coop_game_result) %>%
    bind_rows(coop_trading_result) %>%
    bind_rows(non_coop_trading_result) %>%
    mutate(run = run_name) %>%
    left_join(select(scene, -cost,-price), by = c('run', 'turf'))

  return(out)
}