 
library(tidyverse)

# Consistency measure
KR21<-function(x) {
  k=sum(x)
  n=length(x)
  p=1/n
  rho<-(k/(k-1))*(1-(k*p*(1-p)/var(x)))
  rho<-min(rho, 1)
  rho<-max(rho, 0)
  return(rho)
}

objs<-c()
for (c in c('b', 'r', 'y')) {
  for (s in c('c', 'd', 's')) {
    objs<-c(objs, paste0(c,s))
  }
}
consistency<-df.sels %>%
  select(learningTaskId, sequence, trial) %>%
  distinct()

for (i in 1:nrow(consistency)) {
  counts<-df.sels %>%
    filter(learningTaskId==consistency[i,'learningTaskId'],
           sequence==consistency[i,'sequence'],
           trial==consistency[i,'trial']) %>%
    pull(n)
  consistency[i,'kr21']<-KR21(counts)
  consistency[i,'cond']<-paste0('L',substr(consistency[i,'learningTaskId'],7,7))
}


# Stats
pm<-function(vec) {
  return(paste0(round(mean(vec),2),'\\pm',round(sd(vec),2)))
}

c.data<-consistency %>% filter(sequence!='combined')
pm(c.data$kr21)
t.test(c.data$kr21, rep(0,nrow(c.data)), paired = T)

# Plot overall
c.data %>%
  ggplot(aes(x=sequence,y=kr21)) +
  geom_boxplot()

a<-c.data %>%
  mutate(sequence=ifelse(sequence=='default', 'near', 'far'),
         condition=paste0('A',substr(learningTaskId,7,7))) %>%
  mutate(sequence=factor(sequence, levels=c('near','far')),
         condition=factor(condition, levels=c('A1','A3','A5','A2','A4','A6'))) %>%
  ggplot(aes(x=sequence,y=kr21,fill=sequence)) +
  geom_boxplot() +
  geom_jitter(size=0.1) +
  facet_wrap(~condition) +
  scale_fill_brewer(palette='Paired') +
  labs(y='', x='', fill='') +
  theme_classic() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
  

# Significance
lm(kr21~sequence, data=c.data) %>% summary()
near_kr<-consistency %>% filter(sequence=='default') %>% pull(kr21)
far_kr<-consistency %>% filter(sequence=='reverse') %>% pull(kr21)
pm(near_kr)
pm(far_kr)
t.test(near_kr, far_kr, paired = T)

# Involving new
exi_kr<-c.data %>% filter(learningTaskId %in% c('learn01', 'learn03', 'learn05')) %>% pull(kr21)
new_kr<-c.data %>% filter(learningTaskId %in% c('learn02', 'learn04', 'learn06')) %>% pull(kr21)
pm(exi_kr)
pm(new_kr)
t.test(exi_kr, new_kr, paired = T)

b<-c.data %>%
  mutate(effect=ifelse(learningTaskId %in% c('learn01', 'learn03', 'learn05'), 'match', 'new')) %>%
  ggplot(aes(x=kr21, fill=effect)) +
  geom_density(alpha=0.8) +
  labs(x="Cronbach's alpha", y='', fill='') +
  scale_fill_brewer(palette='Paired') +
  theme_classic()

# Feature changes
sha_kr<-c.data %>% filter(learningTaskId %in% c('learn01', 'learn02')) %>% pull(kr21)
col_kr<-c.data %>% filter(learningTaskId %in% c('learn03', 'learn04')) %>% pull(kr21)
pm(sha_kr)
pm(col_kr)
t.test(sha_kr,col_kr,paired = T)

c<-c.data %>%
  filter(!(learningTaskId %in% c('learn05','learn06'))) %>%
  mutate(feat=ifelse(learningTaskId %in% c('learn01', 'learn02'), 'shape', 'color')) %>%
  ggplot(aes(x=kr21, fill=feat)) +
  geom_density(alpha=0.8) +
  labs(x="Cronbach's alpha", y='', fill='') +
  scale_fill_brewer(palette='Paired') +
  theme_classic()

library(ggpubr)
ggarrange(a,ggarrange(b,c,ncol=2,labels=c('B', 'C')),
          nrow=2,heights=c(2.5,1),labels=c('A',''))


# Try bar plot
consistency %>%
  filter(sequence!='combined') %>%
  mutate(sequence=ifelse(sequence=='default', 'near', 'far'),
         condition=paste0('A',substr(learningTaskId,7,7))) %>%
  mutate(sequence=factor(sequence, levels=c('near','far')),
         # condition=factor(condition, levels=c('A1','A3','A5','A2','A4','A6'))
  ) %>%
  ggplot(aes(x=sequence, y=kr21, fill=sequence)) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  geom_jitter(size=0.1) +
  facet_grid(~condition) +
  theme_classic() +
  scale_fill_brewer(palette='Paired') +
  labs(x='', fill='', y='')
  
  

agg_con<-consistency %>%
  filter(sequence!='combined') %>%
  mutate(sequence=ifelse(sequence=='default', 'near', 'far'),
         condition=paste0('A',substr(learningTaskId,7,7))) %>%
  mutate(sequence=factor(sequence, levels=c('near','far')),
         # condition=factor(condition, levels=c('A1','A3','A5','A2','A4','A6'))
         ) %>%
  select(condition, sequence, trial, kr21) %>%
  group_by(condition, sequence) %>%
  summarise(mean=mean(kr21), sd=sd(kr21)) 
ppt<-df.sels %>%
  filter(sequence!='combined') %>%
  group_by(learningTaskId, sequence) %>%
  summarise(n=sum(n)/15) %>%
  mutate(sequence=ifelse(sequence=='default', 'near', 'far'),
         condition=paste0('A',substr(learningTaskId,7,7)))  %>%
  mutate(sequence=factor(sequence, levels=c('near','far'))) %>%
  ungroup() %>%
  select(condition, sequence, n)
d<-agg_con %>%
  left_join(ppt, by=c('condition', 'sequence')) %>%
  mutate(se=sd/sqrt(n)) %>%
  ggplot(aes(x=sequence, y=mean, fill=sequence)) + 
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean, ymax=mean+se), width=0.2) +
  #geom_jitter(size=0.1) +
  facet_grid(~condition) +
  theme_classic() +
  scale_fill_brewer(palette='Paired') +
  labs(x='', fill='', y='') +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black"))


ggarrange(d,ggarrange(b,c,ncol=2,labels=c('B', 'C')),
          nrow=2,heights=c(1.5,1),labels=c('A',''))


# Fisher's exact test
tests = df.trials %>%
  select(ix, condition=learningTaskId, order, trial, result=selection) %>%
  mutate(condition=as.factor(condition), order=as.factor(order), trial=as.factor(trial), result=as.factor(result))

fisher.test(table(tests$trial, tests$result), simulate.p.value=TRUE)


# For slides
a<-df.sels %>% filter(sequence=='default') %>% select(learningTaskId, trial, selection, default=n)
b<-df.sels %>% filter(sequence=='reverse') %>% select(learningTaskId, trial, selection, reverse=n)
c<-a %>% left_join(b, by=c('learningTaskId', 'trial', 'selection'))
c<-c %>% mutate(n=default + reverse)

consistency_overall<-df.sels %>%
  select(learningTaskId, trial) %>%
  distinct()

for (i in 1:nrow(consistency_overall)) {
  counts<-c %>%
    filter(learningTaskId==consistency[i,'learningTaskId'],
           trial==consistency[i,'trial']) %>%
    pull(n)
  consistency_overall[i,'kr21']<-KR21(counts)
  consistency_overall[i,'cond']<-paste0('L',substr(consistency_overall[i,'learningTaskId'],7,7))
}


consistency %>%
  filter(sequence=='combined') %>%
  ggplot(aes(x=cond,y=kr21)) +
  geom_boxplot(fill="#293352", color='#A4A4A4') +
  coord_flip() + 
  scale_x_discrete(limits = rev) +
  ylim(0,1) +
  labs(y='', x='') +
  theme_classic() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

  
consistency %>%
  filter(sequence!='combined') %>%
  mutate(transfer=ifelse(sequence=='default','near-first','far-first')) %>%
  ggplot(aes(x=cond,y=kr21,fill=transfer)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#ef5fa7", "#27a2ff")) +
  coord_flip() + 
  scale_x_discrete(limits = rev) +
  ylim(0,1) +
  labs(y='', x='') +
  theme_classic() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="none")

# New ones

consistency$sequence = factor(consistency$sequence, levels=c('default', 'reverse', 'combined'))

consistency %>%
  filter(sequence=='combined') %>%
  ggplot(aes(x=cond,y=kr21)) +
  geom_boxplot(fill="#293352", color='#A4A4A4') +
  ylim(0,1) +
  labs(y='', x='') +
  theme_classic() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))


consistency %>%
  filter(sequence!='combined') %>%
  ggplot(aes(x=cond,y=kr21,fill=sequence)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#A4A4A4", "white")) +
  ylim(0,1) +
  labs(y='', x='') +
  theme_classic() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="none")







