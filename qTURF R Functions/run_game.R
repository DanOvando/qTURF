#' run_game runs a game through fixed point iteration
#'
#' @param scene scenario being run
#' @param patches
#' @param time
#' @param eq_pop
#' @param game_tol
#' @param kmode
#'
#' @return nash equilibrium outcome of effort
#' @export
run_game = function(scene,
                    patches,
                    time = 25,
                    start_pop,
                    game_tol = 1e-3,
                    kmode,
                    stock_effect) {

  turfs = unique(scene$turf)

  cc = 1

  game_tol = 1e-5

  old_effort = c(.5,.5)

  diff = 100

  while (diff > game_tol & cc < 100) {
    cc = cc + 1
    new_effort = old_effort
    for (i in seq_along(turfs)) {
      turn = nlminb( #optimize effort given action of other TURF
        start = .2,
        play_game,
        which_turf = i,
        start_pop = start_pop,
        scene = scene,
        patches = patches,
        time = time,
        effort = new_effort,
        kmode = kmode,
        stock_effect = stock_effect,
        lower = 0,
        upper = 100
      )

      new_effort[i] = turn$par
    } #close passing

    diff = sum(new_effort - old_effort) ^ 2

    old_effort = new_effort

  } #close game loop

  return(new_effort)
}