
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
  if (type != '') {
    fd<-prob(feature); fd[[feature_value]]<-1; return(fd)
  }
  else {
    return(feature_value)
  }
}
fetch <- function(feature, values, type) {
  if (type != '') {
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
# Adjustables
get_weights<-function(rules) {
  rule_weights<-list()
  for (n in names(rules)) {
    rule_weights[[n]]<-switch (substr(n, 1, 4),
      #'kept' = 0.9, 'equa' = 0.9, 'fixe' = 0.019, 'rand' = 0.001,
      'kept' = 0.7, 'equa' = 0.7, 'fixe' = 0.1, 'rand' = 0.05,
    )
  }
  to_assign<-setdiff(names(rules), names(rule_weights))
  space<-(1-Reduce('+', rule_weights))
  for (n in to_assign) rule_weights[[n]]<-space/length(to_assign)
  return(rule_weights)
}

# Play with it, don't use it, not working
get_weights<-function(rules, ce=FALSE) {
  rule_weights<-list()
  for (n in names(rules)) {
    rule_weights[[n]]<-switch(substr(n, 1, 4),
      'kept' = 1, 
      'equa' = 1, 
      'fixe' = 1/3, 
      'rand' = 1/9,
      'diff' = 1/2,
      'new_' = 1/6)
  }
  total<-Reduce('+', rule_weights)
  for (i in 1:length(rule_weights)) rule_weights[[i]]<-rule_weights[[i]]/total
  return(rule_weights)
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
  #if(paste0('diff_target_',feature)%in%names(effects)&paste0('diff_agent_',feature)%in%names(effects)) {
   # effects[[paste0('new_', feature)]] <- function(feature, data, type) {
   #   fetch(feature, exclude(feature, list(data[['agent']], data['target'])), type)
  #  }
  #}
  return(effects)
}

# Simulations
decide <- function(feature, training, task, result, type) {
  candidateRules <- check_cause(feature, training, parse_effect(feature, training, type), type)
  if (type!='') {
    fd<-prob(feature)
    if (type=='w') weights<-get_weights(candidateRules)
    for (i in names(candidateRules)) {
      pred_dist<-candidateRules[[i]](feature, task, type)
      for (d in names(pred_dist)) {
        weight<-if (type=='w') weights[[i]] else 1/length(candidateRules)
        fd[[d]] <- fd[[d]] + (pred_dist[[d]] * weight)
      }
    }
    return(set(feature, result, fd))
  } else {
    candidate <- candidateRules[[sample(1:length(candidateRules), 1)]](feature, task, type)
    return(set(feature, result, candidate))
  }
}
get_sim <- function(training, task, type) {
  result <- if (type !='') all_selections() else task[['target']]
  for (f in features) result <- decide(f, training, task, result, type)
  if (type!='') {
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
task[['agent']] <- 'bd'
task[['target']] <- 'bc'

get_sim(data, task, '')
get_sim(data, task, 'a')
get_sim(data, task, 'w')

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

# Simulations
sim_for<-function(cond, tid, type='a', source=df.tasks) {
  dt<-source %>% filter(learningTaskId==paste0('learn0', cond)&trial==tid)
  data <- list(); task <- list()
  data[['agent']]<-as.character(dt$learn_agent)
  data[['target']]<-as.character(dt$learn_target)
  data[['result']]<-as.character(dt$learn_result)
  task[['agent']]<-as.character(dt$agent)
  task[['target']]<-as.character(dt$target)
  
  pred<-get_sim(data, task, type)
  pred$learningTaskId<-paste0('learn0', cond)
  pred$trial<-tid
  pred<-pred %>% select(learningTaskId, trial, selection, prob)
  return(pred)
} 
save_sim <- function(df, opt) {
  for (i in 1:7) {
    for (j in 1:15) {
      if(!(i==1&j==1)) df<-rbind(df, sim_for(i, j, opt))
    }
  }
  return(df)
}

df.sim<-sim_for(1, 1)
df.sim<-save_sim(df.sim, 'a')

df.weighted<-sim_for(1, 1, 'w')
df.weighted<-save_sim(df.weighted, 'w')

df.w2<-sim_for(1, 1, 'w')
df.w2<-save_sim(df.w2, 'w')

df.test<-sim_for(1, 1, 'w')
df.test<-save_sim(df.test, 'w')

save(df.tasks, df.sim, df.weighted, df.w2, file='data_driven.Rdata')













