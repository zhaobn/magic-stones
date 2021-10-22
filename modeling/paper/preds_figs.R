
library(tidyverse)
load('behavioral_data/aggregated.Rdata')
load('modeling/models.Rdata')
textsize = 15

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

beh<-rbind(ppt_near, ppt_far) %>%
  mutate(sequence=factor(sequence, levels=c('near', 'far'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='task', fill='') + #title='Behavioral data'
  scale_y_continuous(trans="reverse", breaks=c(1,5,10,15)) +  # breaks=seq(15)
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black"),
        text = element_text(size=textsize),
        strip.text.y = element_blank()
        ) +
  facet_grid(sequence~condition)
beh


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

fit<-rbind(proc_near, proc_far) %>%
  mutate(sequence=factor(sequence, levels=c('near', 'far'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='', fill='') + # title='Process model: fitted'
  scale_y_continuous(trans="reverse", breaks=c(1,5,10,15)) +
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black"),
        text = element_text(size=textsize),
        #strip.text.x = element_blank(),
        #strip.text.y = element_blank()
        ) +
  facet_grid(sequence~condition)
fit

# c<-rbind(model_uni, model_cat, proc_near, proc_far) %>%
#   mutate(mname=factor(mname, levels=c('universal', 'DPG', 'process_near', 'process_far'))) %>%
#   ggplot(aes(x=object, y=trial, fill=prob)) +
#   geom_tile() +
#   labs(x='', y='task', fill='', title='Computational models: fitted') +
#   #scale_y_continuous(trans="reverse", breaks=1:15) + 
#   scale_y_continuous(trans="reverse", breaks=seq(15)) + # breaks=c(15,1)
#   scale_fill_gradient(low='white', high='#293352') +
#   theme_classic() +
#   theme(strip.background = element_rect(colour=NA, fill=NA),
#         panel.border = element_rect(fill = NA, color = "black")) +
#   facet_grid(mname~condition)
# c

# Process model - strong order effects
order_preds<-grid_preds_01[[1]]
small<-order_preds %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), prob=sim/10000,
         sequence=factor(sequence, levels=c('near', 'far'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='task', fill='') + #  title='Process model: alpha = 0.01'
  scale_y_continuous(trans="reverse", breaks=c(1,5,10,15)) +
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black"),
        text = element_text(size=textsize),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()
        ) +
  facet_grid(sequence~condition)
small  

# Process model - strong randomness
rand_preds<-grid_preds_01[[15]]
large<-rand_preds %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), prob=sim/10000,
         sequence=factor(sequence, levels=c('near', 'far'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='', fill='') + #title='Process model: alpha = 8'
  scale_y_continuous(trans="reverse", breaks=c(1,5,10,15)) +
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black"),
        text = element_text(size=textsize),
        strip.text.x = element_blank()) +
  facet_grid(sequence~condition)
large  

# Put together
library(ggpubr)

ggarrange(beh,small,fit,large, nrow=2, ncol=2,
          labels=c('A','B','D','C'), 
          common.legend=TRUE, legend='bottom')

ggarrange(beh,small,fit,large, nrow=2, ncol=2,
          labels=c('A','C','B','D'), 
          common.legend=TRUE, legend='bottom')

ggarrange(beh,fit,small,large, nrow=2, ncol=2,
          labels=c('A','B','C','D'), 
          common.legend=TRUE, legend='bottom')


# Plots for revision
# Aggregated ppt data
ppt_overall<-df.sels %>%
  filter(sequence=='combined') %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), 
         object=selection, prob=freq, mname='ppt_near',
         sequence='',
         mname='mturk') %>%
  select(condition, trial, object, prob, mname)

# ppt_overall %>%
#   ggplot(aes(x=object, y=trial, fill=prob)) +
#   geom_tile() +
#   labs(x='', y='task', fill='') + #title='Behavioral data'
#   scale_y_continuous(trans="reverse", breaks=c(1,5,10,15)) +  # breaks=seq(15)
#   scale_fill_gradient(low='white', high='#293352') +
#   theme_classic() +
#   theme(strip.background = element_rect(colour=NA, fill=NA),
#         panel.border = element_rect(fill = NA, color = "black"),
#         text = element_text(size=textsize),
#         strip.text.y = element_blank()
#   ) +
#   facet_grid(~condition)

# Universal model
model_uni<-model.uni %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), mname='UnCaLa (fitted)', sequence='') %>%
  select(condition, trial, object, prob=prob_s, mname)

# DPG model
model_cat<-model.cat %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), mname='LoCaLa (fitted)', sequence='') %>%
  select(condition, trial, object, prob=prob_s, mname)

# b<-rbind(model_uni, model_cat) %>%
#   mutate(mname=factor(mname, levels=c('universal', 'DPG'))) %>%
#   ggplot(aes(x=object, y=trial, fill=prob)) +
#   geom_tile() +
#   labs(x='', y='task', fill='', title='Computational models: fitted') +
#   scale_y_continuous(trans="reverse", breaks=seq(15)) + # breaks=c(15,1)
#   scale_fill_gradient(low='white', high='#293352') +
#   theme_classic() +
#   theme(strip.background = element_rect(colour=NA, fill=NA),
#         panel.border = element_rect(fill = NA, color = "black")) +
#   facet_grid(mname~condition)
# b

model_uni_nos<-model.uni %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), mname='UnCaLa (before fitting)', sequence='') %>%
  select(condition, trial, object, prob, mname)

model_cat_nos<-model.cat %>%
  mutate(condition=paste0('A',substr(learningTaskId,7,7)), mname='LoCaLa (before fitting)', sequence='') %>%
  select(condition, trial, object, prob, mname)

# c<-rbind(model_uni_nos, model_cat_nos) %>%
#   mutate(mname=factor(mname, levels=c('universal', 'DPG'))) %>%
#   ggplot(aes(x=object, y=trial, fill=prob)) +
#   geom_tile() +
#   labs(x='', y='task', fill='', title='Computational models: before fitting') +
#   scale_y_continuous(trans="reverse", breaks=seq(15)) + # breaks=c(15,1)
#   scale_fill_gradient(low='white', high='#293352') +
#   theme_classic() +
#   theme(strip.background = element_rect(colour=NA, fill=NA),
#         panel.border = element_rect(fill = NA, color = "black")) +
#   facet_grid(mname~condition)
# c

rbind(ppt_overall, model_uni, model_cat, model_uni_nos, model_cat_nos) %>%
  mutate(mname=factor(mname, levels=c('mturk', 'UnCaLa (fitted)', 'LoCaLa (fitted)', 'UnCaLa (before fitting)', 'LoCaLa (before fitting)'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='task', fill='', title='') +
  scale_y_continuous(trans="reverse", breaks=seq(15)) + # breaks=c(15,1)
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  facet_grid(mname~condition)



rbind(ppt_overall, model_uni, model_cat) %>%
  mutate(mname=factor(mname, levels=c('mturk', 'UnCaLa (fitted)', 'LoCaLa (fitted)'))) %>%
  ggplot(aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  labs(x='', y='task', fill='', title='') +
  scale_y_continuous(trans="reverse", breaks=seq(15)) + # breaks=c(15,1)
  scale_fill_gradient(low='white', high='#293352') +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  facet_grid(mname~condition)










