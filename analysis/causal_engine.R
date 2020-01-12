
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
isEqual <- function(feature, o_1, o_2) {
  return(substr(o_1, dict[[feature]], dict[[feature]])==substr(o_2, dict[[feature]], dict[[feature]]))
}
# Based on primitives
getDiff <- function(feature, target) {
  f <- get(feature, target)
  v <- settings[[feature]]
  return(v[!v %in% f])
}
getNew <- function(feature, agent, target) {
  remove <- c(get(feature, agent), get(feature, target))
  left <- settings[[feature]] [! settings[[feature]] %in% remove]
  return(sample(left, 1))
}

# Causal engine
parse_effect <- function(feature, data) {
  funcs <- list()
  if (isEqual(feature, data[['target']], data[['result']])) {
    funcs[[paste0('kept_target_', feature)]] <- function(feature, data) get(feature, data[['target']])
  } else {
    funcs[[paste0('diff_target_', feature)]] <- function(feature, data) getDiff(feature, data[['target']])
  }
  funcs[[paste0('fixed_', feature)]] <- function(feature, data) get(feature, data[['target']])
  funcs[[paste0('random_', feature)]] <- function (feature, data) sample(settings[[feature]], 1)
  return(funcs)
}
check_cause <- function(feature, data, effects) {
  agent <- substr(data[['agent']], dict[[feature]], dict[[feature]])
  isObserved <- agent %in% c(effects[[1]](feature, data), effects[[2]](feature, data))
  if (isObserved) {
    effects[[paste0('equal_agent_', feature)]] <- function(feature, data) get(feature, data[['agent']])
  } else {
    effects[[paste0('diff_agent_', feature)]] <- function(feature, data) getDiff(feature, data[['agent']])
  }
  if(!isObserved & !isEqual(feature, data[['target']], data[['result']])) {
    effects[[paste0('new_', feature)]] <- getNew(feature, data[['agent']], data[['target']])
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

















