
# Deal with qTURF data ----------------------------------------------------

library(R.matlab)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)

resultdir <- 'DAMNIT'

figurefolder <- paste('Results/',resultdir,'/Figures/',sep = '')

if (dir.exists(figurefolder) == F){dir.create(figurefolder, recursive = T)}

qturf.data <- readMat(paste('Results/',resultdir,'/RawResults/qTURF Results.mat', sep = ''))

total.profits <- (qturf.data$TotalProfits)

dimnames(total.profits) <- list(c('x'),c('M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs',
                                         'M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs',
                                         'M3 - Identical TURFs','M3 - Different TURFs'),c('No ITQ','Internal ITQ','Grand ITQ'))

total.profits <- melt(total.profits, varnames = names(dimnames(total.profits)),
                      na.rm = FALSE, as.is = FALSE, value.name = "value")

colnames(total.profits) <- c('nothing','scenario','rights','relative.profits')

turf.effort <- (qturf.data$Effort)

dimnames(turf.effort) <- list(c('TURF1','TURF2'),c('M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs',
                                         'M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs',
                                         'M3 - Identical TURFs','M3 - Different TURFs'),c('No ITQ','Internal ITQ','Grand ITQ','Who Knows'))

turf.effort <- melt(turf.effort, varnames = names(dimnames(turf.effort)),
                      na.rm = FALSE, as.is = FALSE, value.name = "value")

colnames(turf.effort) <- c('turf','scenario','rights','relative.effort')


turf.profits <- (qturf.data$Profits)

dimnames(turf.profits) <- list(c('TURF1','TURF2'),c('M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs',
                                                   'M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs',
                                                   'M3 - Identical TURFs','M3 - Different TURFs'),c('No ITQ','Internal ITQ','Grand ITQ','Who Knows'))

turf.profits <- melt(turf.profits, varnames = names(dimnames(turf.profits)),
                    na.rm = FALSE, as.is = FALSE, value.name = "value")

colnames(turf.profits) <- c('turf','scenario','rights','relative.profits')

FontColor <- 'black'

Font <- 'Helvetica'

qturf.theme <- theme(legend.position = 'right',text = element_text(size= 12,family= Font,color= FontColor),
                     axis.text = element_text(color= FontColor),axis.text.x = element_text(angle = 35, vjust = 0.9,hjust = 0.9,color = FontColor))


slide_5 <- (ggplot(subset(total.profits, rights == 'No ITQ'), aes(scenario,relative.profits/100, fill = rights)) +
              geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7), color = 'black') +
              scale_y_continuous(labels = percent, limits= c(0,1.2)) +
              geom_hline(yintercept = 1, linetype = 'longdash') +
              qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
              ylab('% of Optimal Profits') +
              scale_fill_brewer(palette = 'Spectral'))

ggsave(file = paste(figurefolder,'slide 5.jpeg'), plot = slide_5, height = 6,width = 8)


slide_6 <- (ggplot(subset(total.profits, rights %in% c('No ITQ','Internal ITQ')), aes(scenario,relative.profits/100, fill = rights)) +
              geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7), color = 'black') +
              scale_y_continuous(labels = percent, limits= c(0,1.2)) +
              geom_hline(yintercept = 1, linetype = 'longdash') +
              qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
              ylab('% of Optimal Profits') +
              scale_fill_brewer(palette = 'Spectral'))
ggsave(file = paste(figurefolder,'slide 6.jpeg'), plot = slide_6, height = 6,width = 8)



slide_7 <- (ggplot(total.profits, aes(scenario,relative.profits/100, fill = rights)) +
              geom_bar(stat = 'identity', width = 0.4, position = position_dodge(width = 0.7), color = 'black') +
              scale_y_continuous(labels = percent, limits= c(0,1.2)) +
              geom_hline(yintercept = 1, linetype = 'longdash') +
              qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
              ylab('% of Optimal Profits') +
              scale_fill_brewer(palette = 'Spectral'))
ggsave(file = paste(figurefolder,'slide 7.jpeg'), plot = slide_7, height = 6,width = 8)


slide_7_effort <- (ggplot(subset(turf.effort, rights == 'Grand ITQ'), aes(scenario,relative.effort/100, fill = turf)) +
  geom_bar(stat = 'identity', width = 0.6, position = position_dodge(width = 0.7), color = 'black') +
  scale_y_continuous(labels = percent, limits= c(0,2.1)) +
  geom_hline(yintercept = 1, linetype = 'longdash') +
  qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
  ylab('% of Optimal Effort') +
  scale_fill_manual(values = c('steelblue2','forestgreen') ))
ggsave(file = paste(figurefolder,'slide 7 effort.jpeg'), plot = slide_7_effort, height = 6,width = 8)


slide_equity <- (ggplot(subset(turf.profits, rights != 'Who Knows'), aes(scenario,relative.profits/100, fill = turf)) +
                     geom_bar(stat = 'identity', width = 0.6, position = position_dodge(width = 0.7), color = 'black') +
                     scale_y_continuous(labels = percent, limits= c(0,2.1)) +
                     geom_hline(yintercept = 1, linetype = 'longdash') +
                     qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
                     ylab('% of Optimal Profits') +
                     scale_fill_manual(values = c('steelblue2','forestgreen') ) +
                   facet_grid(rights~., scales = 'fixed'))
ggsave(file = paste(figurefolder,'slide equity.jpeg'), plot = slide_equity, height = 6,width = 8)

slide_equity2 <- (ggplot(subset(turf.profits, rights != 'Who Knows'), aes(scenario,relative.profits/100, fill = turf)) +
                   geom_bar(stat = 'identity', width = 0.6, position = position_dodge(width = 0.7), color = 'black') +
                   scale_y_continuous(labels = percent, limits= c(0,2.1)) +
                   geom_hline(yintercept = 1, linetype = 'longdash') +
                   qturf.theme + theme (legend.title = element_blank(), axis.title.x = element_blank()) +
                   ylab('% of Optimal Profits') +
                   scale_fill_manual(values = c('steelblue2','forestgreen') ) +
                   facet_grid(rights~., scales = 'fixed'))
ggsave(file = paste(figurefolder,'slide equity.jpeg'), plot = slide_equity, height = 6,width = 8)


