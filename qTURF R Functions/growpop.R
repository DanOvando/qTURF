sim_pop = function(scene,
                     patches,
                     time = 50,
                     start_pop,
                     kmode = 'patch'
                     ) {
  out_frame =  data.frame(a = rep(NA, time), b = NA)

  out = list(
    biomass = out_frame,
    effort = out_frame,
    catch = out_frame,
    revenue = out_frame,
    cost = out_frame,
    profits = out_frame
  )


  out$biomass[1, ] = start_pop

  out$effort[, ] = 0

  out$catch[1,] = pmin(out$biomass[1, ], scene$q * out$effort[1, ] *  out$biomass[1, ])

  if (kmode == 'patch') {
    k = patches$k

  } else{
    k = sum(patches$k)
  }

  for (i in 2:time) {
    last_b = out$biomass[i - 1, ]

    movement =  rev(last_b * scene$move_rate) - (last_b * scene$move_rate)

    out$biomass[i, ] = pmax(0,
                            as.numeric(
                              last_b +  patches$r * last_b * (1 - last_b / patches$k) - out$catch[i - 1,] + movement
                            ))

    out$catch[i,] = pmin(as.numeric(out$biomass[i, ]),
                         as.numeric(scene$q * out$effort[i - 1, ] *  out$biomass[i, ]))

    out$revenue[i,] = out$catch[i,] * scene$price

    out$cost[i,] = out$effort[i,] * scene$cost

    out$profits[i,] = out$revenue[i,] - out$cost[i,]

  } #close pop loop

  vars = names(out)

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
browser()
  # ggplot(a, aes(year, biomass, color = turf)) +
  #   geom_line(size = 2)
  #
  return(out)
}