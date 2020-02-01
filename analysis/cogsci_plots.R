
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(hrbrthemes)
rm(list=ls())


# Prep data
values<-df.tasks%>%select(learningTaskId, hm_default, hm_reverse)%>%
  mutate(learn_id=case_when(learningTaskId=='learn01'~'L1',
                            learningTaskId=='learn02'~'L2',
                            learningTaskId=='learn03'~'L3',
                            learningTaskId=='learn04'~'L4',
                            learningTaskId=='learn05'~'L5',
                            learningTaskId=='learn06'~'L6')) %>%
  select(learn_id, hm_default, hm_reverse)
dp<-rbind(data.frame(learn_id=values$learn_id, sequence=rep('near', 90),value=values$hm_default),
          data.frame(learn_id=values$learn_id, sequence=rep('far', 90),value=values$hm_reverse))
# Plot homogeneity measure per learning scene
a<-ggplot(dp, aes(x=sequence, y=value, fill=sequence)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_light() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=11)
  ) +
  labs(x='', y='') +
  facet_wrap(~learn_id)

# Play with trials
vt<-df.tasks%>%select(trial, hm_default, hm_reverse)%>%
  select(trial, hm_default, hm_reverse)
tp<-rbind(data.frame(trial=vt$trial, sequence=rep('near', 90),value=vt$hm_default),
          data.frame(trial=vt$trial, sequence=rep('far', 90),value=vt$hm_reverse))
# Plot homogeneity measure per learning scene
ggplot(tp, aes(x=sequence, y=value, fill=sequence)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_light() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=11)
  ) +
  labs(x='', y='') +
  facet_wrap(~trial)

# Plot group existing value vs. new value
dp<-dp%>%mutate(effect_value=if_else(learn_id%in%c('L1', 'L3', 'L5'), 'existing', 'new'))

ggplot(dp, aes(x=effect_value, y=value, fill=effect_value)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_light() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=11)
  ) +
  labs(x='', y='')

# Plot group shapes vs group colors
dpf<-dp%>%filter(!(learn_id%in%c('L5', 'L6'))) %>%
  mutate(feature_change=if_else(learn_id%in%c('L1', 'L2'), 'shape', 'color'))

ggplot(dpf, aes(x=feature_change, y=value, fill=feature_change)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_light() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=11)
  ) +
  labs(x='', y='')

# Play with density
dp<-dp%>%mutate(rounded=round(value, 1))
b<-ggplot(dp, aes(x=rounded, group=effect_value, fill=effect_value)) +
  geom_density(alpha=.6) +
  theme_light() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=11)
  ) +
  labs(x='', y='') +
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))

c<-ggplot(dpf, aes(x=rounded, group=feature_change, fill=feature_change)) +
  geom_density(alpha=.6) +
  theme_light() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=11)
  ) +
  labs(x='', y='') +
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))

ggarrange(a, heights = c(2, 0.7),                                                
          ggarrange(b, c, ncol = 2, labels = c("B", "C")),
          nrow = 2, 
          labels = "A"
) 

# Play with heatmap
# Get data in the convenient format
forheatmap<-df.md%>%
  mutate(learn=case_when(learningTaskId=='learn01'~'L1',
                                           learningTaskId=='learn02'~'L2',
                                           learningTaskId=='learn03'~'L3',
                                           learningTaskId=='learn04'~'L4',
                                           learningTaskId=='learn05'~'L5',
                                           learningTaskId=='learn06'~'L6'),
         sequence=if_else(sequence=='default', 'near-transfer', 'far-transfer')) %>%
  select(learn, sequence, trial, nm_arsq, cf_arsq, cw_arsq, ft_arsq)
sub_htmp<-function(md, source=forheatmap) {
  return(source%>%select(learn, sequence, trial, value=!!as.name(paste0(md, '_arsq')))%>%mutate(model=md))
}
df.rq_plot<-rbind(sub_htmp('nm'), sub_htmp('cf'), sub_htmp('cw'), sub_htmp('ft'))
save(df.sw, df.tw, df.tasks, df.sels, df.md, df.rq_plot, file='cogsci_20200201.Rdata')

# Heatmap 
a<-ggplot(data=df.rq_plot, aes(x=model, y=trial, fill=value)) + 
  geom_tile(colour = "black") +
  geom_text(aes(label = round(value, 2)), color="grey50", size=3) +
  labs(x='', y='', fill='Adj. R^2') +
  #scale_fill_gradient(low = "white", high = "steelblue4") +
  xlim('nm', 'cf', 'cw', 'ft') +
  scale_y_reverse(labels = c('task 1', seq(2, 15)), breaks = seq(1,15)) +
  scale_fill_viridis(direction=-1) +
  theme(legend.position="bottom", 
        panel.background = element_blank()
        ) +
  facet_grid(sequence~learn)
a

# Plot individual examples
# L1, task 15
prep_data<-function(lid, tid, seq='default', df=df.sels) {
  data<-df%>%filter(learningTaskId==lid&trial==tid&sequence==seq)
  bp<-data%>%select(selection, value=freq)%>%mutate(data='participant')
  nm<-data%>%select(selection, value=nm_pp)%>%mutate(data='nm')
  cf<-data%>%select(selection, value=fc_pp)%>%mutate(data='cf')
  cw<-data%>%select(selection, value=wc_pp)%>%mutate(data='cw')
  result<-rbind(bp,nm,cf,cw)
  result$data <- factor(result$data, levels = c('nm', 'cf', 'cw', 'participant'))
  return(result)
}
d1<-prep_data('learn01', 15)
b<-ggplot(d1, aes(x=selection, y=value, fill=data)) +
  geom_bar(position="dodge", stat="identity") +
  theme_light() +
  scale_fill_viridis(discrete = T, option = "E", direction = -1) +
  labs(x='', y='frequency/probability') +
  ylim(0, 1) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        panel.border = element_blank())
b  

# L4, task 1
d2<-prep_data('learn04', 1, 'reverse')
c<-ggplot(d2, aes(x=selection, y=value, fill=data)) +
  geom_bar(position="dodge", stat="identity") +
  theme_light() +
  ylim(0, 1) +
  theme_light() +
  scale_fill_viridis(discrete = T, option = "E", direction = -1) +
  labs(x='', y='frequency/probability') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        panel.border = element_blank())
c
d<-ggplot(data.frame()) + geom_point() +
  labs(x='', y='') +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(a, widths = c(2, 1),                                                
          ggarrange(d, b, d, c, 
                    nrow=4, 
                    labels = c("B", '', "C", ''),
                    heights = c(.4, 1, .4, 1)
                    ),
          nrow = 1,
          labels = "A"
) 


# Plot regression
ggplot(data=df.sels, aes(x=freq, y=cw_pp)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() +
  facet_wrap(~learningTaskId)

plot_rg<-function(col, df=df.sels) {
  p<-ggplot(data=df, aes_string(x="freq", y=paste0(col, '_pp'))) + 
    geom_point() + labs(x='', y='') + ylim(0, 1) +
    geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
    theme_ipsum()
  return(p)
}
ggarrange(plot_rg('nm'), plot_rg('cf'), plot_rg('cw'), plot_rg('ft'),
          ncol=4, labels = c("nm","cf","cw", "ft")) 












