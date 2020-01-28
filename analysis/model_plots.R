
library(dplyr)
library(ggplot2)
rm(list=ls())

# Prep data for plot
ppt<-df.freq%>%mutate(type='default')%>%select(learningTaskId, trial, type, selection, value=freq)
rev<-rev.freq%>%mutate(type='reverse')%>%select(learningTaskId, trial, type, selection, value=freq)
nm<-df.pred%>%mutate(type='normative') %>%select(learningTaskId, trial, type, selection, value=pp)
dd<-df.sim%>%mutate(type='flat_prior') %>%select(learningTaskId, trial, type, selection, value=prob)
wg<-df.weighted%>%mutate(type='natural_weight') %>%select(learningTaskId, trial, type, selection, value=prob)
w2<-df.w2%>%mutate(type='contextualized') %>%select(learningTaskId, trial, type, selection, value=prob)
ts<-df.test%>%mutate(type='test') %>%select(learningTaskId, trial, type, selection, value=prob)

plot_all<-function(df) {
  g<-ggplot(df, aes(x=selection, y=value, fill=type)) + 
    geom_bar(position="dodge", stat="identity") +
    facet_grid(trial ~ learningTaskId) +
    labs(x='', y='') + scale_fill_brewer(palette="Paired") + 
    theme_light() +
    theme(legend.position="bottom", legend.title=element_blank())
  return(g)
}

plot_all(rbind(ppt, rev))

plot_all(rbind(ppt, nm))
plot_all(rbind(ppt, dd))
plot_all(rbind(ppt, wg))

plot_all(rbind(rev, nm))
plot_all(rbind(rev, dd))
plot_all(rbind(rev, wg))
plot_all(rbind(rev, w2))

plot_all(rbind(nm, dd))
plot_all(rbind(nm, wg))

plot_all(rbind(rev, ts))
plot_all(rbind(ppt, ts))


# Cogsci
plot_all(rbind(ppt, dd, wg))



# Aggregate per condition/trial by d-bar
ppt_norm<-ppt_norm %>% mutate(d=abs(nm-ppt))
ppt_norm_per_condition<-ppt_norm %>% group_by(learningTaskId) %>%
  summarise(dsum=sum(d), n=n()) %>% mutate(d=dsum/n)
ggplot(ppt_norm_per_condition, aes(x=learningTaskId, y=d)) + 
  geom_bar(stat="identity", fill="steelblue") + labs(x='', y='')

ppt_norm_per_trial<-ppt_norm %>% group_by(trial) %>%
  summarise(dsum=sum(d), n=n()) %>% mutate(d=dsum/n)
ggplot(ppt_norm_per_trial, aes(x=trial, y=d)) + 
  geom_bar(stat="identity", fill="steelblue") + labs(x='', y='')






