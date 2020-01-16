
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

# Primitives
get <- function(feature, object) {
  return(substr(object, dict[[feature]], dict[[feature]]))
}
set <- function(feature, object, val) {
  substr(object, dict[[feature]], dict[[feature]]) <- val
  return(object)
}
fetch <- function(values) {
  return(sample(values, 1))
}
# Based on primitives
exclude <- function(feature, object) {
  vals<-vector()
  if (typeof(object)=='list') {
    for (o in object) {
      vals<-c(vals, get(feature, o))
    }
  } else {
    vals<-c(get(feature, object))
  }
  return(setdiff(settings[[feature]], vals))
}

# Causal engine
parse_effect <- function(feature, data) {
  funcs <- list()
  if (get(feature, data[['result']])==get(feature, data[['target']])) {
    funcs[[paste0('kept_target_', feature)]] <- function(feature, data) get(feature, data[['target']])
  } else {
    funcs[[paste0('diff_target_', feature)]] <- function(feature, data) fetch(exclude(feature, data[['target']]))
  }
  funcs[[paste0('fixed_', feature)]] <- function(feature, data) get(feature, data[['target']])
  funcs[[paste0('random_', feature)]] <- function (feature, data) fetch(settings[[feature]])
  return(funcs)
}
check_cause <- function(feature, data, effects) {
  if (get(feature, data[['result']]==get(feature, data[['agent']]))) {
    effects[[paste0('equal_agent_', feature)]] <- function(feature, data) get(feature, data[['agent']])
  } else {
    effects[[paste0('diff_agent_', feature)]] <- function(feature, data) fetch(exclude(feature, data[['agent']]))
  }
  if(paste0('diff_target_',feature)%in%names(effects)&paste0('diff_agent_',feature)%in%names(effects)) {
    effects[[paste0('new_', feature)]] <- function(feature, data) {
      fetch(exclude(feature, list(data[['agent']], data['target'])))
    }
  }
  return(effects)
}

# Simulations
decide <- function(feature, training, task, obj) {
  candidateRules <- check_cause(feature, training, parse_effect(feature, training))
  candidate <- candidateRules[[sample(1:length(candidateRules), 1)]](feature, task)
  return(set(feature, obj, candidate))
}
get_sim <- function(training, task) {
  result <- task[['target']]
  for (f in features) result <- decide(f, training, task, result)
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

















