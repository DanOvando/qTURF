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
      ylab('Marginal Profits') +
      facet_grid(policy ~ .) +
      scale_fill_manual(values = c(turf.1.col, turf.2.col))
  )

ggsave(
  file = paste(run_dir, 'profit plot.pdf'),
  plot = profit_plot,
  height = fig_height,
  width = fig_width
)
