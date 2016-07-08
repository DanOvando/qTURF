# transturfs --------------------------------------------------------------
# A wrapper for dealing with a better version of TURF itqs
rm(list = ls())
run_name = 'BMS Resubmission Draft'

run_model = F

run_time = 75

base_q = 0.1

q_diff = 1.5

base_price = 1

base_cost = 1

no_coop_cost = 1 / .77 - 1 #based off of Costello et al. 2016 Cost parameter (c) decreases from current value by 23%

no_coop_price = 1 / 1.31 - 1 # reduction in price for non-cooperation Ex-vessel price (p) increases from current value by 31%

stock_effect = 1.3

kmode = 'patch'

opt_policy = 'Optimal'

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
    strip.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()
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
    time = run_time,
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

# results <- results %>%
#   filter(policy != 'Derby ITQ')

if (!'Coop ITQ' %in% unique(results$policy)) {
  results$policy = results$policy %>%
    factor(levels = c("Derby Game",
                      "Coop Game",
                      "Coop Derby Game",
                       "Derby ITQ",
                      "Optimal"))
} else{
  results$policy = results$policy %>%
    factor(levels = c(
      "Derby Game",
      "Coop Game",
      "Coop Derby Game",
      "Derby ITQ",
      'Coop ITQ',
      "Optimal"
    ))

}

abrv_mat = data.frame(policy = unique(results$policy),abrv_policy = NA) %>%
  mutate(policy  = factor(policy,levels = c(
    "Derby Game",
    "Coop Game",
    "Coop Derby Game",
    "Derby ITQ",
    'Coop ITQ',
    "Optimal"
  ))) %>%
  arrange(policy) %>%
  mutate(abrv_policy =
           c('DG','CG','CDG','DITQ','CITQ','Opt'))

# results$abrv_policy = abbreviate(results$policy)

results = results %>%
  left_join(abrv_mat, by = 'policy')

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
  select(-value,-opt_value) %>%
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
  select(-value,-opt_value) %>%
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

outer_labels = data.frame(
  x = 1:length(policies),
  y = 1.2 * max(scale.labs$y),
  abrv_policy = unique(rel_agg_results$abrv_policy)
)

runs_to_use = c('M0-Identical TURFs',
                'M1-Identical TURFs',
                'M2-Different TURFs')

# define scale labels
scale.labs.2 <- data.frame(
  x = rep(1, 5),
  y = seq(25, 125, by = 25),
  labels = paste(seq(25, 125, by = 25), '%', sep = '')
)

scale.grid.2 <- data.frame(x = rep(seq_along(policies), each = 5),
                           y = rep(25, length(policies) * 5))

outer_labels_2 = data.frame(
  x = 1:length(policies),
  y = 1.25 * max(scale.labs.2$y),
  abrv_policy = unique(rel_agg_results$abrv_policy)
)



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
  legend.text = element_text(size = 10),
  legend.title = element_blank(),
  # plot.title = element_text(lineheight = 0.8,size = 20),
  legend.position = "right",
  legend.direction = "vertical",
  strip.text.x = element_text(size = 10),
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
      facet_wrap( ~ run) +
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
        check_overlap = F,
        nudge_y = 10
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
      scale_fill_grey(start = 1, end = 0.4, guide = F) +
      ggtitle('% of Optimal Profits')
  )


ggsave(
  file = paste(run_dir, 'relative petal profits.pdf'),
  plot = rel_petal_profits,
  height = 2.5,
  width = 6
)

rel_petal_profits_2 <-
  (
    rel_agg_results %>%
      filter(run %in% runs_to_use) %>%
      ggplot(aes(policy, 100 * total_profits)) +
      geom_bar(stat = 'identity',
               color = 'black',
               aes(fill = policy)) +
      facet_wrap( ~ run) +
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
      # geom_text(
      #   data = outer_labels,
      #   aes(x = x, y = y, label = abrv_policy),
      #   check_overlap = F,
      #   nudge_y = 10
      # ) +
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
      scale_fill_grey(start = 1, end = 0.2) +
      ggtitle('% of Optimal Profits')
  )


ggsave(
  file = paste(run_dir, 'relative petal profits 2.pdf'),
  plot = rel_petal_profits_2,
  height = 2.5,
  width = 6
)


rel_complete_petal_profits <-
  (
    rel_agg_results %>%
      # filter(run %in% runs_to_use) %>%
      ggplot(aes(policy, 100 * total_profits)) +
      geom_bar(stat = 'identity',
               color = 'black',
               aes(fill = policy)) +
      facet_wrap( ~ run) +
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
      scale_fill_grey(start = 1, end = 0.4, guide = F)
  )


ggsave(
  file = paste(run_dir, 'relative complete petal profits.pdf'),
  plot = rel_complete_petal_profits,
  height = 6,
  width = 8
)


rel_petal_biomass <-
  (
    ggplot(rel_agg_results,
           aes(policy, 100 * total_biomass)) +
      geom_bar(stat = 'identity',
               color = 'black',
               aes(fill = policy)) +
      facet_wrap(~ run) +
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
      facet_wrap(~ run) +
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

rel_petal_cost <-
  (
    ggplot(rel_agg_results,
           aes(policy, 100 * total_cost)) +
      geom_bar(stat = 'identity',
               color = 'black',
               aes(fill = policy)) +
      facet_wrap(~ run) +
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
  file = paste(run_dir, 'relative petal cost.pdf'),
  plot = rel_petal_cost,
  height = 6,
  width = 8
)

marginal_profit_plot <-
  (
    results %>%
      ggplot(aes(run, marginal_profits, fill = turf)) +
      geom_bar(
        stat = 'identity',
        width = 0.6,
        position = position_dodge(width = 0.7),
        color = 'black'
      ) +
      # scale_y_continuous(labels = percent, limits = c(0, 2.1)) +
      # geom_hline(yintercept = 1, linetype = 'longdash') +
      qturf.theme + theme (legend.title = element_blank()) +
      ylab('Marginal Profits') +
      facet_grid(policy ~ .) +
      scale_fill_manual(values = c(turf.1.col, turf.2.col))
  )

ggsave(
  file = paste(run_dir, 'marginal profit plot.pdf'),
  plot = marginal_profit_plot,
  height = fig_height,
  width = fig_width
)

profit_plot <-
  (
    results %>%
      ggplot(aes(run, profits, fill = turf)) +
      geom_bar(
        stat = 'identity',
        width = 0.6,
        position = position_dodge(width = 0.7),
        color = 'black'
      ) +
      # scale_y_continuous(labels = percent, limits = c(0, 2.1)) +
      # geom_hline(yintercept = 1, linetype = 'longdash') +
      qturf.theme + theme (legend.title = element_blank()) +
      ylab('Profits') +
      facet_grid(policy ~ .) +
      scale_fill_manual(values = c(turf.1.col, turf.2.col))
  )

ggsave(
  file = paste(run_dir, 'profit plot.pdf'),
  plot = profit_plot,
  height = fig_height,
  width = fig_width
)


profit_distribution_plot <- results %>%
  filter(skill == 'Identical TURFs' &
           movement == 'M2' & (policy == 'Coop ITQ' | policy == 'Derby Game')) %>%
  dplyr::select(turf,biomass,profits,policy) %>%
  rename(`Biomass (MT)` = biomass, `Profits ($)` = profits, Policy = policy) %>%
  gather('metric','value', `Biomass (MT)`:`Profits ($)`) %>%
  ggplot(aes(turf, value, fill = Policy)) +
  geom_bar(stat = 'identity',
           position = 'dodge',
           color = 'black') +
  facet_grid(metric ~ ., scales = 'free_y') +
  scale_fill_grey(start = 1,
                  end = 0.4,
                  name = element_blank()) +
  qturf.theme +
  theme(axis.title.x = element_text(), axis.title.y = element_blank()) +
  xlab('TURF')

ggsave(
  file = paste(run_dir, 'profit distribution plot.pdf'),
  plot = profit_distribution_plot,
  height = fig_height,
  width = fig_width
)


catch_distribution_plot <- results %>%
  mutate(turf2 = ifelse(turf == 'a',1,2)) %>%
  filter(skill == 'Identical TURFs' &
           movement == 'M2' & (policy == 'Coop ITQ' | policy == 'Coop Game')) %>%
  dplyr::select(turf2,biomass,catch,policy) %>%
  rename(`Biomass` = biomass, `Quota` = catch, Policy = policy) %>%
  gather('metric','value', `Biomass`:`Quota`) %>%
  ggplot(aes(factor(turf2), value, fill = Policy)) +
  geom_bar(stat = 'identity',
           position = 'dodge',
           color = 'black') +
  facet_grid(metric ~ ., scales = 'free_y') +
  scale_fill_grey(start = 1,
                  end = 0.4,
                  name = element_blank()) +
  qturf.theme +
  theme(axis.title.x = element_text(), axis.title.y = element_blank()) +
  xlab('TURF')

ggsave(
  file = paste(run_dir, 'catch distribution plot.pdf'),
  plot = catch_distribution_plot,
  height = fig_height,
  width = fig_width
)

biomass_cost_figure = rel_agg_results %>%
  ungroup() %>%
  filter(run %in% runs_to_use) %>%
  mutate(game = grepl('game', policy, ignore.case = T)) %>%
  ggplot(aes(total_cost, jitter(total_biomass))) +
  # geom_point(shape = 21,size = 2) +
  geom_smooth(aes(color = game), method = 'lm', se = F) +
  geom_point(aes(fill = game),
              shape = 21,
              size = 5,
              alpha = 0.75)+
              #  width = 0.4,
              # height = 0.1) +
  # geom_text(aes(label = abrv_policy), check_overlap = F, nudge_x = .1) +
  facet_grid(run ~ .) +
  scale_fill_grey(start = .8, end = 0.2, name = 'Game?') +
  scale_color_grey(start = .8, end = 0.2, guide = F) +
  scale_y_continuous(labels = percent, name = '% of Optimal Biomass', limits = c(.8,1.15)) +
  qturf.theme +
  theme(axis.title.x = element_text(), strip.text.y = element_text(size = 8)
) +
  xlab('Total Fishing Costs')

ggsave(
  file = paste(run_dir, 'biomass v cost figure.pdf'),
  plot = biomass_cost_figure,
  height = fig_height,
  width = fig_width
)
