
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)
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


