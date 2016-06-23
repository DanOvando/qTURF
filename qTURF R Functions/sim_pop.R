#' Simulate a surplus production model
#'
#' @param scene
#' @param patches
#' @param time
#' @param start_pop
#' @param effort
#' @param kmode
#'
#' @return projected fishery
#' @export
sim_pop = function(scene,
                     patches,
                     time = 50,
                     start_pop,
                   effort = c(0,0),
                     kmode = 'patch',
                   stock_effect = 1
                     ) {
  out_frame =  data.frame(a = rep(NA, time), b = NA)

  out = list(
    biomass = out_frame,
    # effort = out_frame,
    catch = out_frame,
    revenue = out_frame,
    cost = out_frame,
    profits = out_frame,
    marginal_profits = out_frame
  )

  out$biomass[1, ] = start_pop

  out$effort = as.data.frame(matrix(rep(effort,time), nrow = time, ncol = 2, byrow = T))

  colnames(out$effort) = c('a','b')

  out$catch[1,] =  scene$q * out$effort[1, ] *  out$biomass[1, ]

  if (kmode == 'patch') {
    k = patches$k

  } else{
    k = sum(patches$k)
  }

  for (i in 2:time) {
    last_b = out$biomass[i - 1, ]

    movement =  rev(last_b * scene$move_rate) - (last_b * scene$move_rate)

    out$biomass[i, ] = last_b +  patches$r * last_b * (1 - last_b / patches$k) - out$catch[i - 1,] + movement

    out$catch[i,] = (scene$q * out$effort[i, ] *  out$biomass[i, ])

    out$revenue[i,] = out$catch[i,] * scene$price

    out$cost[i,] = out$effort[i,]^stock_effect * scene$cost

    out$profits[i,] = out$revenue[i,] - out$cost[i,]

    out$marginal_profits[i,] = scene$price * scene$q * out$biomass[i,] - stock_effect * out$effort[i,]^(stock_effect - 1) * scene$cost

  } #close pop loop


  vars = names(out)

# Flatten things ----------------------------------------------------------


  flat_fun = function(var, out) {
    out[[var]] %>%
      mutate(year = 1:dim(.)[1]) %>%
      gather('turf', 'value', a:b) %>%
      mutate(metric = var)

  }

  out = lapply(vars, flat_fun, out = out) %>%
    bind_rows() %>%
    ungroup() %>%
    spread(metric, value)

  return(out)
}