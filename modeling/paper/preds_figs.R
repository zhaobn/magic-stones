
library(tidyverse)
load('../../behavioral_data/aggregated.Rdata')


# Plot behavioral data
ppt_near<-df.sels %>%
  filter(sequence=='default') %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), object=selection, prob=freq, mname='ppt_near') %>%
  select(condition, trial, object, prob, mname)

ppt_far<-df.sels %>%
  filter(sequence=='reverse') %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), object=selection, prob=freq, mname='ppt_far') %>%
  select(condition, trial, object, prob, mname)


# Universal model

# DPG model

# Process model

# Plot
rbind(ppt_near, ppt_far) %>%
  mutate(mname=factor(mname, levels=c('ppt_near', 'ppt_far'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='object', y='task', fill='') +
  scale_y_continuous(trans="reverse", breaks=1:15) + 
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  facet_grid(mname~condition)
  










