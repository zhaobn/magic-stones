
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

get_sim(data, task, 'a')













