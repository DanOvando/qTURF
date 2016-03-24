
# Deal with qTURF data ----------------------------------------------------
rm(list = ls())
library(R.matlab)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)

resultdir <- 'BMS Revisions 2'

figurefolder <- paste('Results/',resultdir,'/Figures/',sep = '')

fig_height = 4

fig_width = 6

dpi_res <- 1200

# if (dir.exists(figurefolder) == F){dir.}

qturf.data <- readMat(paste('Results/',resultdir,'/RawResults/qTURF Results.mat', sep = ''))

raw.qturf.data <- readMat(paste('Results/',resultdir,'/RawResults/raw qTURF Results.mat', sep = ''))

total.profits <- (qturf.data$TotalProfits)

dimnames(total.profits) <- list(c('x'),c('M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs',
                                         'M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs',
                                         'M3 - Identical TURFs','M3 - Different TURFs'),c('No ITQ','Internal ITQ','Inter-TURF ITQ','ITQ v Derby'))

total.profits <- melt(total.profits, varnames = names(dimnames(total.profits)),
                      na.rm = FALSE, as.is = FALSE, value.name = "value")

colnames(total.profits) <- c('nothing','scenario','rights','relative.profits')

turf.effort <- (qturf.data$Effort)

dimnames(turf.effort) <- list(c('TURF1','TURF2'),c('M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs',
                                         'M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs',
                                         'M3 - Identical TURFs','M3 - Different TURFs'),c('No ITQ','Internal ITQ','Inter-TURF ITQ','ITQ v Derby','Omni'))

turf.effort <- melt(turf.effort, varnames = names(dimnames(turf.effort)),
                      na.rm = FALSE, as.is = FALSE, value.name = "value")

colnames(turf.effort) <- c('turf','scenario','rights','relative.effort')

turf.profits <- (qturf.data$Profits)

dimnames(turf.profits) <- list(c('TURF1','TURF2'),c('M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs',
                                                   'M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs',
                                                   'M3 - Identical TURFs','M3 - Different TURFs'),c('No ITQ','Internal ITQ','Inter-TURF ITQ','ITQ v Derby','Omni'))

turf.profits <- melt(turf.profits, varnames = names(dimnames(turf.profits)),
                    na.rm = FALSE, as.is = FALSE, value.name = "value")

colnames(turf.profits) <- c('turf','scenario','rights','relative.profits')


# Raw Profits -------------------------------------------------------------


raw.turf.profits <- raw.qturf.data$Profits

dimnames(raw.turf.profits) <- list(c('TURF1','TURF2'),c('M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs',
                                                    'M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs',
                                                    'M3 - Identical TURFs','M3 - Different TURFs'),c('No ITQ','Internal ITQ','Inter-TURF ITQ','ITQ v Derby','Omni'))

raw.turf.profits <- melt(raw.turf.profits, varnames = names(dimnames(raw.turf.profits)),
                     na.rm = FALSE, as.is = FALSE, value.name = "value")

colnames(raw.turf.profits) <- c('turf','scenario','rights','profits')

# Raw Effort -------------------------------------------------------------

raw.turf.effort <- raw.qturf.data$Effort

dimnames(raw.turf.effort) <- list(c('TURF1','TURF2'),c('M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs',
                                                        'M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs',
                                                        'M3 - Identical TURFs','M3 - Different TURFs'),c('No ITQ','Internal ITQ','Inter-TURF ITQ','ITQ v Derby','Omni'))

raw.turf.effort <- melt(raw.turf.effort, varnames = names(dimnames(raw.turf.effort)),
                         na.rm = FALSE, as.is = FALSE, value.name = "value")

colnames(raw.turf.effort) <- c('turf','scenario','rights','effort')


# Raw Marginal Profits -------------------------------------------------------------

raw.turf.mps <- raw.qturf.data$MarginalProfits

dimnames(raw.turf.mps) <- list(c('TURF1','TURF2'),c('M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs',
                                                       'M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs',
                                                       'M3 - Identical TURFs','M3 - Different TURFs'),c('No ITQ','Internal ITQ','Inter-TURF ITQ','ITQ v Derby','Omni'))

raw.turf.mps <- melt(raw.turf.mps, varnames = names(dimnames(raw.turf.mps)),
                        na.rm = FALSE, as.is = FALSE, value.name = "value")

colnames(raw.turf.mps) <- c('turf','scenario','rights','marginal_profit')

# Make Plots --------------------------------------------------------------



FontColor <- 'black'

Font <- 'Helvetica'

qturf.theme <- theme_light() + theme(legend.position = 'right',text = element_text(size= 10,family= Font,color= FontColor),
                     axis.text = element_text(color= FontColor),axis.text.x = element_text(angle = 35, vjust = 0.9,hjust = 0.9,color = FontColor))

turf.1.col <- 'gray81'

turf.2.col <- 'grey39'


fig_effort <- raw.turf.effort %>%
  # filter(scenario == 'M0 - Different TURFs') %>%
  ggplot(aes(scenario, effort, fill = turf)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  qturf.theme +
  facet_grid(rights ~.)
ggsave(file = paste(figurefolder,'figure effort.pdf'), plot = fig_effort, height = fig_height,width = fig_width)



fig_marginal_profits <- raw.turf.mps %>%
  # filter(scenario == 'M0 - Different TURFs') %>%
  ggplot(aes(scenario, marginal_profit, fill = turf)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  qturf.theme +
  facet_grid(rights ~.)

ggsave(file = paste(figurefolder,'figure marginal profits.pdf'), plot = fig_marginal_profits, height = fig_height,width = fig_width)


fig1 <- (ggplot(subset(total.profits, rights == 'No ITQ'), aes(scenario,relative.profits/100, fill = rights)) +
              geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7),color = 'black') +
#               geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7), color = 'black', show_guide = F) +
              scale_y_continuous(labels = percent, limits= c(0,1.2)) +
              geom_hline(yintercept = 1, linetype = 'longdash') +
              qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
              ylab('% of Optimal Profits') +
              scale_fill_brewer(palette = 'Greys'))

ggsave(file = paste(figurefolder,'figure 1.pdf'), plot = fig1, height = fig_height,width = fig_width)


fig2who <- (ggplot(subset(total.profits, rights %in% c('No ITQ','Internal ITQ')), aes(scenario,relative.profits/100, fill = rights)) +
              geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7), color = 'black') +
              scale_y_continuous(labels = percent, limits= c(0,1.2)) +
              geom_hline(yintercept = 1, linetype = 'longdash') +
              qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
              ylab('% of Optimal Profits') +
              scale_fill_brewer(palette = 'Greys'))
ggsave(file = paste(figurefolder,'figure whoknows.pdf'), plot = fig2who, height = fig_height,width = fig_width, dpi = dpi_res )



fig2 <- (ggplot(total.profits, aes(scenario,relative.profits/100, fill = rights)) +
              geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7), color = 'black') +
              scale_y_continuous(labels = percent, limits= c(0,1.2)) +
              geom_hline(yintercept = 1, linetype = 'longdash') +
              qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
              ylab('% of Optimal Profits') +
              scale_fill_brewer(palette = 'Greys'))
ggsave(file = paste(figurefolder,'figure 2.pdf'), plot = fig2, height = fig_height,width = fig_width, dpi = dpi_res )


fig3 <- (ggplot(subset(turf.effort, rights == 'Inter-TURF ITQ'), aes(scenario,relative.effort/100, fill = turf)) +
  geom_bar(stat = 'identity', width = 0.6, position = position_dodge(width = 0.7), color = 'black') +
  scale_y_continuous(labels = percent, limits= c(0,2.1)) +
  geom_hline(yintercept = 1, linetype = 'longdash') +
  qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
  ylab('% of Optimal Effort') +
  scale_fill_manual(values = c(turf.1.col,turf.2.col) ))
ggsave(file = paste(figurefolder,'figure 3.pdf'), plot = fig3, height = fig_height,width = fig_width, dpi = dpi_res )


slide_equity <- (ggplot(subset(turf.profits, rights != 'Omni'), aes(scenario,relative.profits/100, fill = turf)) +
                     geom_bar(stat = 'identity', width = 0.6, position = position_dodge(width = 0.7), color = 'black') +
                     scale_y_continuous(labels = percent, limits= c(0,2.1)) +
                     geom_hline(yintercept = 1, linetype = 'longdash') +
                     qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
                     ylab('% of Optimal Profits') +
                     scale_fill_manual(values = c(turf.1.col,turf.2.col) ) +
                   facet_grid(rights~., scales = 'fixed'))
ggsave(file = paste(figurefolder,'slide equity.pdf'), plot = slide_equity,height = fig_height,width = fig_width, dpi = dpi_res )

slide_equity2 <- (ggplot(subset(turf.profits, rights != 'Omni'), aes(scenario,relative.profits/100, fill = turf)) +
                   geom_bar(stat = 'identity', width = 0.6, position = position_dodge(width = 0.7), color = 'black') +
                   scale_y_continuous(labels = percent, limits= c(0,2.1)) +
                   geom_hline(yintercept = 1, linetype = 'longdash') +
                   qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
                   ylab('% of Optimal Profits') +
                   scale_fill_manual(values = c(turf.1.col,turf.2.col) ) +
                   facet_grid(rights~., scales = 'fixed'))
ggsave(file = paste(figurefolder,'slide 2 equity.pdf'), plot = slide_equity2, height = fig_height,width = fig_width, dpi = dpi_res )

#

opt_profits <- subset(raw.turf.profits, rights == 'Omni') %>%
  rename(opt_profits = profits) %>%
  select(turf,scenario,opt_profits)

raw.turf.profits.for.plot <- subset(raw.turf.profits, rights != 'Omni') %>%
  left_join(opt_profits, by = c('turf', 'scenario'))

fig4 <- (ggplot(raw.turf.profits.for.plot, aes(scenario,profits, fill = turf)) +
                    geom_bar(stat = 'identity', width = 0.6, position = position_dodge(width = 0.7), color = 'black') +
                      geom_point(aes(scenario,opt_profits, group = turf),
                                 position = position_dodge(width = 0.7), color = 'black', size = 2) +
qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
                    ylab('Profits') +
                    scale_fill_manual(values = c(turf.1.col,turf.2.col) ) +
                    facet_grid(rights~., scales = 'fixed'))

pdf(file = paste(figurefolder,'figure 4.pdf'), height = fig_height,width = fig_width )

print(fig4)
dev.off()

ggsave(file = paste(figurefolder,'figure 4.pdf'), plot = fig4, height = fig_height,width = fig_width, dpi = dpi_res )


