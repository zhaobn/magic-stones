
library(tidyverse)
load('../behavioral_data/aggregated.Rdata')
load('models.Rdata')

# Plot behavioral data
ppt_near<-df.sels %>%
  filter(sequence=='default') %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), 
         object=selection, prob=freq, mname='ppt_near',
         sequence=ifelse(sequence=='default', 'near', 'far')) %>%
  select(condition, trial, object, prob, mname, sequence)

ppt_far<-df.sels %>%
  filter(sequence=='reverse') %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), 
         object=selection, prob=freq, mname='ppt_far',
         sequence=ifelse(sequence=='default', 'near', 'far')) %>%
  select(condition, trial, object, prob, mname, sequence)

a<-rbind(ppt_near, ppt_far) %>%
  mutate(sequence=factor(sequence, levels=c('near', 'far'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='task', fill='', title='Behavioral data') +
  scale_y_continuous(trans="reverse", breaks=c(15,1)) + 
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  facet_grid(sequence~condition)
a

# Universal model
model_uni<-model.uni %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), mname='universal', sequence='') %>%
  select(condition, trial, object, prob=prob_s, mname, sequence)
  
# DPG model
model_cat<-model.cat %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), mname='DPG', sequence='') %>%
  select(condition, trial, object, prob=prob_s, mname, sequence)

b<-rbind(model_uni, model_cat) %>%
  mutate(mname=factor(mname, levels=c('universal', 'DPG'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='task', fill='', title='Computational models: fitted') +
  scale_y_continuous(trans="reverse", breaks=c(15,1)) + 
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  facet_grid(mname~condition)
b


# Process model
proc_near<-model.proc %>%
  filter(sequence=='near') %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), 
         mname='process_near') %>%
  select(condition, trial, object, prob=prob_s, mname, sequence)

proc_far<-model.proc %>%
  filter(sequence=='far') %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), mname='process_far') %>%
  select(condition, trial, object, prob=prob_s, mname, sequence)

c<-rbind(proc_near, proc_far) %>%
  mutate(sequence=factor(sequence, levels=c('near', 'far'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='task', fill='', title='Process model: fitted') +
  #scale_y_continuous(trans="reverse", breaks=1:15) + 
  scale_y_continuous(trans="reverse", breaks=c(15,1)) + 
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  facet_grid(sequence~condition)
c

f<-rbind(model_uni, model_cat, proc_near, proc_far) %>%
  mutate(mname=factor(mname, levels=c('universal', 'DPG', 'process_near', 'process_far'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='task', fill='', title='Computational models: fitted') +
  #scale_y_continuous(trans="reverse", breaks=1:15) + 
  scale_y_continuous(trans="reverse", breaks=c(15,1)) + 
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  facet_grid(mname~condition)
f

# Process model - strong order effects
order_preds<-grid_preds_01[[1]]
d<-order_preds %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), prob=sim/10000,
         sequence=factor(sequence, levels=c('near', 'far'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='task', fill='', title='Process model: alpha = 0.01') +
  #scale_y_continuous(trans="reverse", breaks=1:15) + 
  scale_y_continuous(trans="reverse", breaks=c(15,1)) + 
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  facet_grid(sequence~condition)
d  

# Process model - strong randomness
rand_preds<-grid_preds_01[[15]]
e<-rand_preds %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), prob=sim/10000,
         sequence=factor(sequence, levels=c('near', 'far'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='task', fill='', title='Process model: alpha = 8') +
  #scale_y_continuous(trans="reverse", breaks=1:15) + 
  scale_y_continuous(trans="reverse", breaks=c(15,1)) + 
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  facet_grid(sequence~condition)
e  

# Put together
library(ggpubr)

ggarrange(a,c,d,e, nrow=4,
          labels=c('A','B','C','D'), 
          common.legend=TRUE, legend='right')

ggarrange(a,f,d,e, nrow=4, heights=c(1,1.5,1,1),
          labels=c('A','B','C','D'), 
          common.legend=TRUE, legend='right')







