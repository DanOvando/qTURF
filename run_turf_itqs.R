# transturfs --------------------------------------------------------------
# A wrapper for dealing with a better version of TURF itqs
rm(list = ls())
run_name = '4.0 no stock effect'

run_model = T

base_q = 0.1

q_diff = 1.5

base_price = 1

base_cost = 1

no_coop_cost = 1 / .77 - 1 #based off of Costello et al. 2016 Cost parameter (c) decreases from current value by 23%

no_coop_price = 1 / 1.31 - 1 # reduction in price for non-cooperation Ex-vessel price (p) increases from current value by 31%

stock_effect = 1

kmode = 'patch'

opt_policy = 'Cooperative ITQ'

run_dir = paste('qTURF Results/', run_name, '/', sep = '')

if (dir.exists(run_dir) == F) {
  dir.create(run_dir, recursive = T)
}

library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)
library(purrr)
library(pbapply)
library(scales)
library(ggalt)

sapply(list.files(
  pattern = "[.]R$",
  path = "qTURF R Functions",
  full.names = TRUE
),
source)


# Plot Parameters ---------------------------------------------------------


FontColor <- 'black'

Font <- 'Helvetica'

qturf.theme <-
  theme_light() + theme(
    legend.position = 'right',
    text = element_text(
      size = 10,
      family = Font,
      color = FontColor
    ),
    axis.text = element_text(color = FontColor),
    axis.text.x = element_text(
      angle = 35,
      vjust = 0.9,
      hjust = 0.9,
      color = FontColor
    ),
    axis.title.x = element_blank(),
    strip.text.y = element_text(size = 4)
  )

turf.1.col <- 'gray81'

turf.2.col <- 'grey39'

# Initialize --------------------------------------------------------------

turf = c('a', 'b')

patches = data.frame(turf = turf,
                     k = c(100, 100),
                     r = c(0.2, 0.2))

movement = c('M0', 'M1', 'M2', 'M3')

move_mat = expand.grid(turf = turf, movement = movement) %>%
  mutate(move_rate = c(0, 0, 0.5, 0.5, 0.4, 0.05, 0.05, 0.4))
# mutate(move_rate = c(0, 0, 0.5, 0.5, 0.2, 0.05, 0.05, 0.2))

skill = c('Identical TURFs', 'Different TURFs')



skill_mat = expand.grid(turf = turf, skill = skill) %>%
  mutate(q = c(base_q, base_q, q_diff * base_q, base_q))

runs = expand.grid(turf = turf,
                   movement = movement,
                   skill = skill) %>%
  left_join(skill_mat, by = c('turf', 'skill')) %>%
  left_join(move_mat, by = c('turf', 'movement')) %>%
  mutate(run = paste(movement, skill, sep = '-'))


# run model ---------------------------------------------------------------

scenes = unique(runs$run)

if (run_model == T) {
  results = pblapply(
    scenes,
    run_scene,
    runs = runs,
    patches = patches,
    stock_effect = stock_effect,
    time = 10,
    no_coop_cost = no_coop_cost,
    no_coop_price = no_coop_price,
    base_price = base_price,
    base_cost = base_cost,
    kmode = kmode
  ) %>%
    bind_rows()

  save(file = paste(run_dir, 'qTURF results.Rdata', sep = ''), results)
} else{
  load(file = paste(run_dir, 'qTURF results.Rdata', sep = ''))
}

if (!'Cooperative True ITQ' %in% unique(results$policy)) {
  results$policy = results$policy %>%
    factor(
      levels = c(
        "Non-Cooperative Competition",
        "Cooperative Competition",
        "Cooperative Derby Competition",
        "Non-Cooperative ITQ",
        "Cooperative ITQ"
      )
    )
} else{
  results$policy = results$policy %>%
    factor(
      levels = c(
        "Non-Cooperative Competition",
        "Cooperative Competition",
        "Cooperative Derby Competition",
        "Non-Cooperative ITQ",
        'Cooperative True ITQ',
        "Cooperative ITQ"
      )
    )

}

results$abrv_policy = abbreviate(results$policy)


agg_results = results %>%
  group_by(policy, run) %>%
  summarise(
    total_biomass = sum(biomass),
    total_catch = sum(catch),
    total_effort = sum(effort),
    total_profits = sum(profits),
    total_revenue = sum(revenue),
    total_cost = sum(cost),
    abrv_policy = unique(abrv_policy)
  )

rel_results = results %>%
  gather('metric', 'value', biomass:revenue)

opt_results = rel_results %>%
  filter(policy == opt_policy) %>%
  select(turf, run, metric, value) %>%
  rename(opt_value = value)

rel_results = rel_results %>%
  left_join(opt_results, by = c('turf', 'run', 'metric')) %>%
  ungroup() %>%
  mutate(rel_value = pmin(2.5, (1e-3 + value) / (1e-3 + opt_value))) %>%
  select(-value, -opt_value) %>%
  spread(key = metric, value = rel_value)

rel_agg_results = agg_results %>%
  gather('metric', 'value', total_biomass:total_revenue)

opt_agg_results = rel_agg_results %>%
  filter(policy == opt_policy) %>%
  ungroup() %>%
  select(run, metric, value) %>%
  rename(opt_value = value)

rel_agg_results = rel_agg_results %>%
  left_join(opt_agg_results, by = c('run', 'metric')) %>%
  ungroup() %>%
  mutate(rel_value = pmin(2.5, (1e-3 + value) / (1e-3 + opt_value))) %>%
  select(-value, -opt_value) %>%
  spread(key = metric, value = rel_value) %>%
  filter(policy != opt_policy)



# Make Plots --------------------------------------------------------


fig_height = 4

fig_width = 6


fig_effort <- results %>%
  # filter(scenario == 'M0 - Different TURFs') %>%
  ggplot(aes(run, effort, fill = turf)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  qturf.theme +
  facet_grid(policy ~ .) +
  scale_x_discrete()
ggsave(
  file = paste(run_dir, 'figure effort.pdf'),
  plot = fig_effort,
  height = fig_height,
  width = fig_width
)


fig1 <-
  (
    ggplot(agg_results,
           aes(run, total_profits, fill = policy)) +
      geom_bar(
        stat = 'identity',
        width = 0.4,
        position = position_dodge(width = 0.7),
        color = 'black'
      ) +
      #               geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7), color = 'black', show_guide = F) +
      # scale_y_continuous(labels = percent, limits = c(0, 1.2)) +
      # geom_hline(yintercept = 1, linetype = 'longdash') +
      qturf.theme + theme (legend.title = element_blank()) +
      ylab('Profits') +
      scale_fill_brewer(palette = 'Greys')
  )

# fig1 <-
#   (
#     ggplot(agg_results,
#            aes(run, total_profits, shape = policy)) +
#       geom_point(position = 'dodge') +
#       #               geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7), color = 'black', show_guide = F) +
#       # scale_y_continuous(labels = percent, limits = c(0, 1.2)) +
#       # geom_hline(yintercept = 1, linetype = 'longdash') +
#       qturf.theme + theme (legend.title = element_blank()) +
#       ylab('Profits') +
#       scale_fill_brewer(palette = 'Greys')
#   )

ggsave(
  file = paste(run_dir, 'figure 1.pdf'),
  plot = fig1,
  height = fig_height,
  width = fig_width
)

rel_fig1 <-
  (
    ggplot(rel_agg_results,
           aes(run, total_profits, fill = policy)) +
      geom_bar(
        stat = 'identity',
        width = 0.4,
        position = position_dodge(width = 0.7),
        color = 'black'
      ) +
      scale_y_continuous(labels = percent) +
      geom_hline(yintercept = 1, linetype = 'longdash') +
      qturf.theme + theme (legend.title = element_blank()) +
      ylab('Profits') +
      scale_fill_brewer(palette = 'Greys')
  )

ggsave(
  file = paste(run_dir, 'relative figure 1.pdf'),
  plot = rel_fig1,
  height = fig_height,
  width = fig_width
)

policies = unique(rel_agg_results$policy)

scale.grid <- data.frame(x = rep(seq_along(policies), each = 4),
                         y = rep(25, length(policies) * 4))



# define scale labels
scale.labs <- data.frame(
  x = rep(1, 4),
  y = seq(25, 100, by = 25),
  labels = paste(seq(25, 100, by = 25), '%', sep = '')
)

outer_labels = data.frame(x = 1:length(policies), y = 1.1*max(scale.labs$y), abrv_policy = unique(rel_agg_results$abrv_policy) )

runs_to_use = c('M0-Identical TURFs','M2-Different TURFs')

# define scale labels
scale.labs.2 <- data.frame(
  x = rep(1, 5),
  y = seq(25, 125, by = 25),
  labels = paste(seq(25, 125, by = 25), '%', sep = '')
)

scale.grid.2 <- data.frame(x = rep(seq_along(policies), each = 5),
                         y = rep(25, length(policies) * 5))

outer_labels_2 = data.frame(x = 1:length(policies), y = 1.1*max(scale.labs.2$y), abrv_policy = unique(rel_agg_results$abrv_policy) )



radar_theme = theme(
  axis.line.x = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  panel.grid = element_blank(),
  legend.text = element_text(size = 12),
  legend.title = element_blank(),
  # plot.title = element_text(lineheight = 0.8,size = 20),
  legend.position = "right",
  legend.direction = "vertical",
  strip.text.x = element_text(size = 12),
  panel.background = element_rect(fill = 'white')
)

rel_petal_profits <-
  (
    rel_agg_results %>%
      filter(run %in% runs_to_use) %>%
      ggplot(aes(policy, 100 * total_profits)) +
      geom_bar(stat = 'identity',
               color = 'black',
               aes(fill = policy)) +
      facet_wrap(~ run) +
      geom_bar(
        data = scale.grid,
        aes(x = x, y = y),
        width = 1 ,
        stat = 'identity',
        position = 'stack',
        color = 'grey80',
        fill = NA,
        alpha = 0.2
      ) +
      geom_text(
        data = outer_labels,
        aes(x = x, y = y, label = abrv_policy),
        check_overlap = T
      ) +
      geom_text(
        data = scale.labs,
        aes(
          x = x,
          y = y,
          label = labels,
          fill = NULL
        ),
        size = 2
      ) +
      coord_polar() +
      # qturf.theme +
      radar_theme +
      # qturf.theme + theme (legend.title = element_blank()) +
      # ylab('Profits') +
      scale_fill_grey(start = 1, end = 0.4)
  )


ggsave(
  file = paste(run_dir, 'relative petal profits.pdf'),
  plot = rel_petal_profits,
  height = 6,
  width = 8
)


rel_bio_fig1 <-
  (
    ggplot(rel_agg_results,
           aes(run, total_biomass, fill = policy)) +
      geom_bar(
        stat = 'identity',
        width = 0.4,
        position = position_dodge(width = 0.7),
        color = 'black'
      ) +
      scale_y_continuous(labels = percent) +
      geom_hline(yintercept = 1, linetype = 'longdash') +
      qturf.theme + theme (legend.title = element_blank()) +
      ylab('% of Profit Optimal Biomass') +
      scale_fill_brewer(palette = 'Greys')
  )

ggsave(
  file = paste(run_dir, 'relative biomass figure 1.pdf'),
  plot = rel_bio_fig1,
  height = fig_height,
  width = fig_width
)

rel_petal_biomass <-
  (
    ggplot(rel_agg_results,
           aes(policy, 100 * total_biomass)) +
      geom_bar(stat = 'identity',
               color = 'black',
               aes(fill = policy)) +
      facet_wrap( ~ run) +
      geom_bar(
        data = scale.grid.2,
        aes(x = x, y = y),
        width = 1 ,
        stat = 'identity',
        position = 'stack',
        color = 'grey80',
        fill = NA,
        alpha = 0.2
      ) +
      geom_text(
        data = outer_labels_2,
        aes(x = x, y = y, label = abrv_policy),
        check_overlap = T
      ) +
      geom_text(
        data = scale.labs.2,
        aes(
          x = x,
          y = y,
          label = labels,
          fill = NULL
        ),
        size = 2
      ) +
      coord_polar() +
      # qturf.theme +
      radar_theme +
      # qturf.theme + theme (legend.title = element_blank()) +
      # ylab('Profits') +
      scale_fill_grey(start = 1, end = 0.4)
  )




ggsave(
  file = paste(run_dir, 'relative petal biomass.pdf'),
  plot = rel_petal_biomass,
  height = 6,
  width = 8
)

rel_petal_effort <-
  (
    ggplot(rel_agg_results,
           aes(policy, 100 * total_effort)) +
      geom_bar(stat = 'identity',
               color = 'black',
               aes(fill = policy)) +
      facet_wrap( ~ run) +
      geom_bar(
        data = scale.grid.2,
        aes(x = x, y = y),
        width = 1 ,
        stat = 'identity',
        position = 'stack',
        color = 'grey80',
        fill = NA,
        alpha = 0.2
      ) +
      geom_text(
        data = outer_labels_2,
        aes(x = x, y = y, label = abrv_policy),
        check_overlap = T
      ) +
      geom_text(
        data = scale.labs.2,
        aes(
          x = x,
          y = y,
          label = labels,
          fill = NULL
        ),
        size = 2
      ) +
      coord_polar() +
      # qturf.theme +
      radar_theme +
      # qturf.theme + theme (legend.title = element_blank()) +
      # ylab('Profits') +
      scale_fill_grey(start = 1, end = 0.4)
  )

ggsave(
  file = paste(run_dir, 'relative petal effort.pdf'),
  plot = rel_petal_effort,
  height = 6,
  width = 8
)

fig3 <-
  (
    ggplot(results,
           aes(run, effort, fill = turf)) +
      geom_bar(
        stat = 'identity',
        width = 0.6,
        position = position_dodge(width = 0.7),
        color = 'black'
      ) +
      # scale_y_continuous(labels = percent, limits = c(0, 2.1)) +
      # geom_hline(yintercept = 1, linetype = 'longdash') +
      qturf.theme + theme (legend.title = element_blank()) +
      ylab('Effort') +
      facet_grid(policy ~ .) +
      scale_fill_manual(values = c(turf.1.col, turf.2.col))
  )
ggsave(
  file = paste(run_dir, 'figure 3.pdf'),
  plot = fig3,
  height = fig_height,
  width = fig_width,
  dpi = dpi_res
)

fig4 <-
  (
    results %>%
      select(turf, policy, run, profits) %>%
      left_join(filter(opt_results, metric == 'profits'), by = c('turf', 'run')) %>%
      filter(policy != opt_policy) %>%
      ggplot(aes(run, profits, fill = turf)) +
      geom_bar(
        stat = 'identity',
        width = 0.6,
        position = position_dodge(width = 0.7),
        color = 'black'
      ) +
      geom_point(
        aes(run, opt_value, group = turf),
        position = position_dodge(width = 0.7),
        color = 'black',
        size = 2
      ) +
      qturf.theme + theme (legend.title = element_blank()) +
      ylab('Profits') +
      scale_fill_manual(values = c(turf.1.col, turf.2.col)) +
      facet_grid(policy ~ ., scales = 'fixed')
  )


ggsave(
  file = paste(run_dir, 'figure 4.pdf'),
  plot = fig4,
  height = fig_height,
  width = fig_width
)


biomass_fig <-
  (
    ggplot(agg_results, aes(run, total_biomass, fill = policy)) +
      geom_bar(
        stat = 'identity',
        width = 0.6,
        position = position_dodge(width = 0.7),
        color = 'black'
      ) +
      # geom_point(
      #   aes(scenario, opt_profits, group = turf),
      #   position = position_dodge(width = 0.7),
      #   color = 'black',
      #   size = 2
      # ) +
      qturf.theme + theme (legend.title = element_blank()) +
      ylab('Biomass')       # facet_grid(policy ~ ., scales = 'fixed')
  )


ggsave(
  file = paste(run_dir, 'biomass figure.pdf'),
  plot = biomass_fig,
  height = fig_height,
  width = fig_width
)

biomass_v_cost_fig = ggplot(agg_results, aes(total_cost, total_biomass, fill = policy)) +
  geom_point(shape = 21, size = 2)

ggsave(
  file = paste(run_dir, 'biomass v cost figure.pdf'),
  plot = biomass_v_cost_fig,
  height = fig_height,
  width = fig_width
)
