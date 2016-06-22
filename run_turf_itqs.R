# transturfs --------------------------------------------------------------
# A wrapper for dealing with a better version of TURF itqs

run_name = 'qTURF 2.1'

run_model = F

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
  mutate(move_rate = c(0, 0, 0.5, 0.5, 0.2, 0.05, 0.05, 0.2))

skill = c('Identical TURFs', 'Different TURFs')

base_q = 0.1

q_diff = 1.5

base_price = 1

base_cost = 1

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
  scenes = 'M2-Different TURFs'

  results = pblapply(scenes,  run_scene, runs = runs, patches = patches) %>%
    bind_rows()

  save(file = paste(run_dir, 'qTURF results.Rdata', sep = ''), results)
} else{
  load(file = paste(run_dir, 'qTURF results.Rdata', sep = ''))
}

results$policy = results$policy %>%
  factor(levels = c("Non-Cooperative Competition","Cooperative Competition",
                    "Cooperative Derby Competition",
                    "Non-Cooperative ITQ",
                    "Cooperative ITQ"))


agg_results = results %>%
  group_by(policy, run) %>%
  summarise(
    total_biomass = sum(biomass),
    total_catch = sum(catch),
    total_effort = sum(effort),
    total_profits = sum(profits),
    total_revenue = sum(revenue),
    total_cost = sum(cost)
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
  mutate(rel_value = pmin(2.5,(1e-3 + value) / (1e-3 + opt_value))) %>%
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
  mutate(rel_value = pmin(2.5,(1e-3 + value) / (1e-3 + opt_value))) %>%
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
      geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7), color = 'black') +
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

rel_bio_fig1 <-
  (
    ggplot(rel_agg_results,
           aes(run, total_biomass, fill = policy)) +
      geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7), color = 'black') +
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
      select(turf,policy,run,profits) %>%
      left_join(filter(opt_results, metric == 'profits'), by = c('turf','run')) %>%
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
