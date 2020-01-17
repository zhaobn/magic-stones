
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
set <- function(feature, object, val) {
  substr(object, dict[[feature]], dict[[feature]]) <- val
  return(object)
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
decide <- function(feature, training, task, obj, type) {
  candidateRules <- check_cause(feature, training, parse_effect(feature, training, type), type)
  if (type=='a') {
    fd<-prob(feature)
    for (i in 1:length(candidateRules)) {
      pred_dist<-candidateRules[[i]](feature, task, type)
      for (d in names(pred_dist)) {
        fd[[d]] <- fd[[d]] + (pred_dist[[d]] * 1/length(candidateRules))
      }
    }
    return(fd)
  } else {
    candidate <- candidateRules[[sample(1:length(candidateRules), 1)]](feature, task, type)
    return(set(feature, obj, candidate))
  }
}
get_sim <- function(training, task, type) {
  result <- task[['target']]
  for (f in features) result <- decide(f, training, task, result, type)
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

get_sim(data, task)













