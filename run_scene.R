run_scene = function(scene,
                     scenes,
                     time = 'eq',
                     non_coop_cost = 1.3,
                     patches) {

  scene = filter(runs, run == scenes[4])

  scene$price = NA

  scene$cost = NA

  eq_pop = sim_pop(scene = scene, patches = patches, start_pop = patches$k,kmode = 'global') %>%
  filter(year == max(year)) %>%
    select(turf,biomass) %>%
    spread(turf,biomass)

# non cooperative game ----------------------------------------------------


# cooperative game --------------------------------------------------------


# cooperative trading -----------------------------------------------------------------


# noncooperative trading --------------------------------------------------





}