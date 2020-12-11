 
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
  labs(x='Cronbach alpha', y='', fill='') +
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
  labs(x='Cronbach alpha', y='', fill='') +
  scale_fill_brewer(palette='Paired') +
  theme_classic()

library(ggpubr)
ggarrange(a,ggarrange(b,c,ncol=2,labels=c('B', 'C')),
          nrow=2,heights=c(2.5,1),labels=c('A',''))


