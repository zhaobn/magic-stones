
library(dplyr)
library(stats4)
rm(list=ls())


save(df.cf, df.cm, df.cw, df.ft, file='constructive_model.Rdata')
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
get <- function(feature, object, type='w') {
  feature_value<-substr(object, dict[[feature]], dict[[feature]])
  if (type != '') {
    fd<-prob(feature); fd[[feature_value]]<-1; return(fd)
  }
  else {
    return(feature_value)
  }
}
fetch <- function(feature, values, type='w') {
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

# Causal engine
parse_effect <- function(feature, data, type='a') {
  funcs<-list()
  if (get(feature, data[['result']], '')==get(feature, data[['target']], '')) {
    funcs[[paste0('kept_', feature)]] <- function(feature, data, type) get(feature, data[['target']], type)
  } else {
    funcs<-c(funcs, check_cause(feature, data, type))
  }
  return(funcs)
}
check_cause <- function(feature, data, type='a') {
  funcs<-list()
  if (get(feature, data[['result']], '')==get(feature, data[['agent']], '')) {
    funcs[[paste0('match_', feature)]] <- function(feature, data, type) get(feature, data[['agent']], type)
  } else {
    #funcs[[paste0('fix_', feature)]] <- function(feature, data, type) get(feature, data[['target']], type)
    funcs[[paste0('diff_', feature)]] <- function(feature, data, type) fetch(feature, exclude(feature, list(data[['agent']], data[['target']])), type)
    funcs[[paste0('random_', feature)]] <- function (feature, data, type) fetch(feature, settings[[feature]], type)
  }
  return(funcs)
}

data<-list('agent'='rc', 'target'='bs', 'result'='rs')
parse_effect('color', data, 'a')

# Default flat
# Fitting possible
get_weight<-function(rule_names) {
  rule_weights<-list()
  n_rule<-length(rule_names)
  for (r in rule_names) {
    rule_weights[[r]]<-1/n_rule
  }
  return(rule_weights)
}

softmaxList<-function(lt, base=1) {
  exps<-c()
  for (i in 1:length(lt)) exps<-c(exps, exp(base*lt[[i]]))
  total<-sum(exps)
  for (i in 1:length(lt)) lt[[i]]<-exps[i]/total
  return(lt)
}

decide <- function(feature, training, task, result, boost, type) {
  rules <- parse_effect(feature, training, type)
  fd<-prob(feature)
  weights<-get_weight(names(rules))
  for (r in names(rules)) {
    pred_dist<-rules[[r]](feature, task, 'w')
    for (d in names(pred_dist)) {
      fd[[d]] <- fd[[d]] + (pred_dist[[d]] * weights[[r]])
    }
  }
  fd<-softmaxList(fd, boost)
  return(set(feature, result, fd))
}


task<-list('agent'='rc', 'target'='rs')
decide('color', data, task, result, 2, 'w')

get_sim <- function(training, task, sequence, da, fa) {
  result <- all_selections()
  boost <- if (sequence=='default') da else fa
  for (f in features) result <- decide(f, training, task, result, boost)
  result <- result %>% mutate(prob=color_prob*shape_prob) %>% select(selection, prob)
  return(result)
}

get_sim(data, task, 'reverse', 1,1)

# Prep sequence-d data
sim_for<-function(cond, tid, seq, da, fa, source=df.tasks) {
  dt<-source %>% filter(learningTaskId==paste0('learn0', cond)&trial==tid)
  data <- list(); task <- list()
  data[['agent']]<-as.character(dt$learn_agent)
  data[['target']]<-as.character(dt$learn_target)
  data[['result']]<-as.character(dt$learn_result)
  task[['agent']]<-as.character(dt$agent)
  task[['target']]<-as.character(dt$target)
  
  pred<-get_sim(data, task, seq, da, fa)
  pred$learningTaskId<-paste0('learn0', cond)
  pred$sequence<-seq
  pred$trial<-tid
  pred<-pred %>% select(learningTaskId, sequence, trial, selection, prob)
  return(pred)
} 

sim_for(1, 1, 'default', 1, 1)
sim_for(1, 1, 'reverse', 1, 3)

# Let's do it
defaults<-sim_for(1, 1, 'default')
for (i in 1:6) {
  for (j in 1:15) if (!(i==1&j==1)) defaults<-rbind(defaults, sim_for(i,j, 'default'))
}
reverses<-sim_for(1, 1, 'reverse')
for (i in 1:6) {
  for (j in 1:15) if (!(i==1&j==1)) reverses<-rbind(reverses, sim_for(i,j, 'reverse'))
}
df.alpha<-rbind(defaults, reverses)
df.alpha$learningTaskId<-as.character(df.alpha$learningTaskId)
df.alpha$sequence<-as.character(df.alpha$sequence)
df.alpha$selection<-as.character(df.alpha$selection)
df.alpha$trial<-as.numeric(as.character(df.alpha$trial))

full_model<-function(d, f){
  likeli=c()
  for (i in 1:6) {
    for (j in 1:15) {
      for (k in c('default', 'reverse')) {
        behavorial<-df.sels%>%
          filter(learningTaskId==paste0('learn0', i)&trial==j&sequence==k) %>%
          select(selection, n)
        pred<-sim_for(i, j, k, d, f)%>% select(selection, prob)
        
        behavorial$selection<-as.character(behavorial$selection) 
        pred$selection<-as.character(pred$selection)
        
        data<-behavorial%>%left_join(pred, by='selection')
        likeli=c(likeli, sum(log(data$prob)*data$n))
      }
    }
  }
  -sum(likeli)
}

full_model(3,3)
mle(full_model, start = list(d=1,f=1)) %>% attributes()
# d        f 
# 2.558283 2.128377 
# min 2493.71

# get the sim
defaults<-sim_for(1, 1, 'default', 2.558283, 2.128377)
for (i in 1:6) {
  for (j in 1:15) if (!(i==1&j==1)) defaults<-rbind(defaults, sim_for(i,j, 'default', 2.558283, 2.128377))
}
reverses<-sim_for(1, 1, 'reverse', 2.558283, 2.128377)
for (i in 1:6) {
  for (j in 1:15) if (!(i==1&j==1)) reverses<-rbind(reverses, sim_for(i,j, 'reverse', 2.558283, 2.128377))
}
df.alpha<-rbind(defaults, reverses)
df.alpha$learningTaskId<-as.character(df.alpha$learningTaskId)
df.alpha$sequence<-as.character(df.alpha$sequence)
df.alpha$selection<-as.character(df.alpha$selection)
df.alpha$trial<-as.numeric(as.character(df.alpha$trial))


