
library(dplyr)
rm(list=ls())

# Feature references
features <- c('color', 'shape')

dict <- list()
dict[['color']] <- which(match(features, 'color')==1)
dict[['shape']] <- which(match(features, 'shape')==1)

settings <- list()
settings[['color']] <- c('b', 'r', 'y')
settings[['shape']] <- c('c', 'd', 's')

# Utils
prob<-function(feature) {
  feature_list<-list()
  for (i in settings[[feature]]) feature_list[[i]] <- 0
  return(feature_list)
}
# TODO: Generalize to n-levels
all_obs<-function(opts=settings) {
  obs<-vector()
  for (i in 1:length(opts[[1]])) {
    for (j in 1:length(opts[[2]])) {
      obs<-c(obs, paste0(opts[[1]][i], opts[[2]][j]))
    }
  }
  return(obs)
} 
all_selections<-function(setup=settings) {
  df<-data.frame('selection'=all_obs(setup))
  for (i in 1:length(setup)) {
    df<-df%>%mutate(!!names(setup)[i]:=substr(selection, i, i))
  }
  return(df)
}

# Primitives
get <- function(feature, object, type) {
  feature_value<-substr(object, dict[[feature]], dict[[feature]])
  if (type=='a') {
    fd<-prob(feature); fd[[feature_value]]<-1; return(fd)
  }
  else {
    return(feature_value)
  }
}
fetch <- function(feature, values, type) {
  if (type=='a') {
    fd<-prob(feature); for (v in values) fd[[v]]<-1/length(values); return(fd)
  } else {
    return(sample(values, 1)) 
  }
}
set <- function(feature, result, val) {
  if (typeof(result)=='list' & typeof(val)=='list') {
    pcol<-paste0(feature, '_prob')
    result[,pcol] <- 0
    for (v in names(val)) result[,pcol][result[,feature]==v]<-val[[v]]
  } else {
    substr(result, dict[[feature]], dict[[feature]]) <- val
  }
  return(result)
}
# Based on primitives
exclude <- function(feature, object) {
  vals<-vector()
  if (typeof(object)=='list') {
    for (o in object) {
      vals<-c(vals, get(feature, o, ''))
    }
  } else {
    vals<-c(get(feature, object, ''))
  }
  return(setdiff(settings[[feature]], vals))
}

# Causal engine
parse_effect <- function(feature, data, type='a') {
  funcs <- list()
  if (get(feature, data[['result']], '')==get(feature, data[['target']], '')) {
    funcs[[paste0('kept_target_', feature)]] <- function(feature, data, type) get(feature, data[['target']], type)
  } else {
    funcs[[paste0('diff_target_', feature)]] <- function(feature, data, type) fetch(feature, exclude(feature, data[['target']]), type)
  }
  funcs[[paste0('fixed_', feature)]] <- function(feature, data, type) get(feature, data[['target']], type)
  funcs[[paste0('random_', feature)]] <- function (feature, data, type) fetch(feature, settings[[feature]], type)
  return(funcs)
}
check_cause <- function(feature, data, effects, type='a') {
  if (get(feature, data[['result']], '')==get(feature, data[['agent']], '')) {
    effects[[paste0('equal_agent_', feature)]] <- function(feature, data, type) get(feature, data[['agent']], type)
  } else {
    effects[[paste0('diff_agent_', feature)]] <- function(feature, data, type) fetch(feature, exclude(feature, data[['agent']]), type)
  }
  if(paste0('diff_target_',feature)%in%names(effects)&paste0('diff_agent_',feature)%in%names(effects)) {
    effects[[paste0('new_', feature)]] <- function(feature, data, type) {
      fetch(feature, exclude(feature, list(data[['agent']], data['target'])), type)
    }
  }
  return(effects)
}

# Simulations
decide <- function(feature, training, task, result, type) {
  candidateRules <- check_cause(feature, training, parse_effect(feature, training, type), type)
  if (type=='a') {
    fd<-prob(feature)
    for (i in 1:length(candidateRules)) {
      pred_dist<-candidateRules[[i]](feature, task, type)
      for (d in names(pred_dist)) {
        fd[[d]] <- fd[[d]] + (pred_dist[[d]] * 1/length(candidateRules))
      }
    }
    return(set(feature, result, fd))
  } else {
    candidate <- candidateRules[[sample(1:length(candidateRules), 1)]](feature, task, type)
    return(set(feature, result, candidate))
  }
}
get_sim <- function(training, task, type) {
  result <- if (type=='a') all_selections() else task[['target']]
  for (f in features) result <- decide(f, training, task, result, type)
  if (type=='a') {
    result <- result %>% mutate(prob=color_prob*shape_prob) %>% select(selection, prob)
  }
  return(result)
}

# Example
data <- list()
data[['agent']] <- 'rs'
data[['target']] <- 'yc'
data[['result']] <- 'ys'

task <- list()
task[['agent']] <- 'bs'
task[['target']] <- 'bc'

get_sim(data, task, '')
get_sim(data, task, 'a')

# Get trial data
# Read from normative model.R: tasks, learnings 
take_cond<-function(cond=1, list=learnings) {
  df<-data.frame(list[[cond]])
  df$learningTaskId<-paste0('learn0', cond)
  df<-df%>%select(learningTaskId, learn_agent=agent, learn_target=target, learn_result=result)
  return(df)
}
trainings<-take_cond()
for (i in 2:length(learnings)) {
  trainings<-rbind(trainings, take_cond(i))
}
df.tasks<-tasks %>% left_join(trainings, by='learningTaskId') %>%
  select(learningTaskId, trial, learn_agent, learn_target, learn_result, agent, target=recipient)
save(df.tasks, file='data_driven.Rdata')

# Try some
sim_for<-function(cond, tid, source=df.tasks) {
  dt<-source %>% filter(learningTaskId==paste0('learn0', cond)&trial==tid)
  data <- list(); task <- list()
  data[['agent']]<-as.character(dt$learn_agent)
  data[['target']]<-as.character(dt$learn_target)
  data[['result']]<-as.character(dt$learn_result)
  task[['agent']]<-as.character(dt$agent)
  task[['target']]<-as.character(dt$target)
  
  pred<-get_sim(data, task, 'a')
  pred$learningTaskId<-paste0('learn0', cond)
  pred$trial<-tid
  pred<-pred %>% select(learningTaskId, trial, selection, prob)
  return(pred)
} 
df.sim<-sim_for(1, 1)
for (i in 1:7) {
  for (j in 1:15) {
    if(!(i==1&j==1)) df.sim<-rbind(df.sim, sim_for(i, j))
  }
}
save(df.sim, df.tasks, file='data_driven.Rdata')

# Plots
df.freq$learningTaskId<-as.character(df.freq$learningTaskId)
df.sim$learningTaskId<-as.character(df.sim$learningTaskId)
ppt_dd <- df.freq %>% 
  left_join(df.sim, by=c('learningTaskId', 'trial', 'selection')) %>%
  select(learningTaskId, trial, selection, ppt=freq, dd=prob) %>%
  arrange(learningTaskId, trial, selection)

ppt<-ppt_dd %>% select(learningTaskId, trial, selection, value=ppt) %>% mutate(type='participant')
ddp<-ppt_dd %>% select(learningTaskId, trial, selection, value=dd) %>% mutate(type='data_driven')
pt_dd<-rbind(ppt, ddp) %>% select(learningTaskId, trial, selection, type, value) %>%
  arrange(learningTaskId, trial, selection)

ggplot(pt_dd, aes(x=selection, y=value, fill=type)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(trial ~ learningTaskId) +
  labs(x='', y='') + scale_fill_brewer(palette="Paired") + 
  theme(legend.position="bottom", legend.title=element_blank())






