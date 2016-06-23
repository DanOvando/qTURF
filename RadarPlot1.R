########################################################
## Radar Plot Demo Code

# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Create dummy data for parameters
params<-c('b0','f0','K','g','phi','price','cost','discount','theta','beta')
sens<-c(55,40,-30,75,50,38,-40,20,72,15)
abssens<-abs(sens)

# Create data frame
df<-data.frame(params,sens,abssens,stringsAsFactors = F)
colnames(df)<-c('Parameter','Sensitivity','AbsValue')

df$sign<-NA
df$sign[df$Sensitivity<0]<-'Negative'
df$sign[df$Sensitivity>0]<-'Positive'

df$quad<-'Over Fished & Over Fishing'

df2<-df
df2$AbsValue<-df$AbsValue*1.2
df2$quad<-'Under Fished & Over Fishing'

df3<-df
df3$AbsValue<-df$AbsValue*.75
df3$quad<-'Over Fished & Under Fishing'

df4<-df
df4$AbsValue<-df$AbsValue*1.15
df4$quad<-'Under Fished & Under Fishing'

dfinal<-rbind(df,df2,df3,df4)

# define grid for plot for 10 sensitivity parameters
scale.grid <- data.frame(
  x = rep(1:10, each = 10),
  y = rep(10, 100)
)

# define scale labels
scale.labs <- data.frame(
  x = rep(1, 10),
  y = seq(10, 100, 10),
  labels = as.character(seq(10, 100, 10))
)


radar<-ggplot(dfinal,aes(x = Parameter, y = AbsValue), width = 1) +
  geom_bar(aes(fill=sign),width = 1 ,stat = 'identity') +
  scale_fill_brewer(palette = 'Set1') +
  geom_bar(data = scale.grid, aes(x = x, y = y), width = 1 ,stat = 'identity',position='stack',color='white',fill=NA) +
  geom_text(data = scale.labs, aes(x = x, y = y, label = labels, fill = NULL), size = 4) +
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "black",size=16),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
    plot.title = element_text(lineheight = 0.8,size = 20),
    legend.position = "right",
    legend.direction = "vertical",
    strip.text.x = element_text(size = 16)
  ) +
  coord_polar() +
  labs(x = '', y = '', title = NULL) +
  facet_wrap(~quad)

# save plot
ggsave('Radar_Sensitivity_ROI.png',width = 9, height = 9)



