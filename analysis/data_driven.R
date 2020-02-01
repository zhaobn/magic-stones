
library(dplyr)
library(stats4)
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
  for (i in 1:6) {
    for (j in 1:15) {
      if(!(i==1&j==1)) df<-rbind(df, sim_for(i, j, opt))
    }
  }
  return(df)
}

df.sim<-sim_for(1, 1, 'a')
df.sim<-save_sim(df.sim, 'a')

df.weighted<-sim_for(1, 1, 'w')
df.weighted<-save_sim(df.weighted, 'w')

save(df.tasks, df.sim, df.weighted, df.w2, file='data_driven.Rdata')

# Try MLE
train_weights<-function(rules, equal, fixed, rand, diff) {
  rule_weights<-list()
  for (n in names(rules)) {
    rule_weights[[n]]<-switch (substr(n, 1, 4),
                               'kept' = equal,
                               'equa' = equal,
                               'fixe' = fixed, 
                               'rand' = rand,
                               diff
    )
  }
  total<-Reduce('+', rule_weights)
  for (n in names(rule_weights)) rule_weights[[n]]<-rule_weights[[n]]/total
  return(rule_weights)
}

md_decide <- function(feature, training, task, result, equal, fixed, rand, diff) {
  candidateRules <- check_cause(feature, training, parse_effect(feature, training, type), type)
  fd<-prob(feature)
  weight_to_train<-train_weights(candidateRules, equal, fixed, rand, diff)
  for (i in names(candidateRules)) {
    pred_dist<-candidateRules[[i]](feature, task, 'w')
    for (d in names(pred_dist)) {
      weight<-weight_to_train[[i]]
      fd[[d]] <- fd[[d]] + (pred_dist[[d]] * weight)
    }
  }
  return(set(feature, result, fd))
}
get_md_sim <- function(training, task, equal, fixed, rand, diff) {
  result <- all_selections()
  for (f in features) result <- md_decide(f, training, task, result, equal, fixed, rand, diff)
  result <- result %>% mutate(prob=color_prob*shape_prob) %>% select(selection, prob)
  return(result)
}
md_sim_for<-function(cond, tid, equal, fixed, rand, diff, source=df.tasks) {
  dt<-source %>% filter(learningTaskId==paste0('learn0', cond)&trial==tid)
  data <- list(); task <- list()
  data[['agent']]<-as.character(dt$learn_agent)
  data[['target']]<-as.character(dt$learn_target)
  data[['result']]<-as.character(dt$learn_result)
  task[['agent']]<-as.character(dt$agent)
  task[['target']]<-as.character(dt$target)
  
  pred<-get_md_sim(data, task, equal, fixed, rand, diff)
  pred$learningTaskId<-paste0('learn0', cond)
  pred$trial<-tid
  pred<-pred %>% select(learningTaskId, trial, selection, prob)
  return(pred)
} 

md_sim_for(2, 4, 1, 1, 1, 1)

# Courtesy to Tia
full_model<-function(equal, fixed, rand, diff){
  likeli=c()
  for (i in 1:6) {
    for (j in 1:15) {
      behavorial<-df.sels%>%
        filter(learningTaskId==paste0('learn0', i)&trial==j&sequence=='default') %>%
        select(selection, n)
      pred<-md_sim_for(i, j, equal, fixed, rand, diff)%>% select(selection, prob)
      
      behavorial$selection<-as.character(behavorial$selection) 
      pred$selection<-as.character(pred$selection)
      
      data<-behavorial%>%left_join(pred, by='selection')
      likeli=c(likeli, sum(log(data$prob)*data$n))
    }
  }
  -sum(likeli)
}

full_model(10,1,1,1)
mle(full_model, start = list(equal=100, fixed=20, rand=140, diff=500)) %>% attributes()

#equal       fixed        rand        diff 
#2.38953600 -0.05161830  0.45439077  0.03974783 


#1059.12327  -22.43968  199.32625   18.02398 

# Then give the probabilities with fitted weights
df.ft<-md_sim_for(1, 1, 2.38953600, -0.05161830, 0.45439077, 0.03974783)
for (i in 1:6) {
  for (j in 1:15) {
    if(!(i==1&j==1)) df.ft<-rbind(df.ft, md_sim_for(i, j, 2.38953600, -0.05161830, 0.45439077, 0.03974783))
  }
}

# Do the whole thing again with additional context parameter
# to account for the reverse order sequence

train_weights<-function(rules, seq, equal, fixed, rand, diff, alpha) {
  rule_weights<-list()
  for (n in names(rules)) {
    rule_weights[[n]]<-switch (substr(n, 1, 4),
                               'kept' = if (seq=='reverse') equal * alpha else equal,
                               'equa' = if (seq=='reverse') equal * alpha else equal,
                               'fixe' = fixed, 
                               'rand' = rand,
                               diff
    )
  }
  total<-Reduce('+', rule_weights)
  for (n in names(rule_weights)) rule_weights[[n]]<-rule_weights[[n]]/total
  return(rule_weights)
}

md_decide <- function(feature, training, task, result, seq, equal, fixed, rand, diff, alpha) {
  candidateRules <- check_cause(feature, training, parse_effect(feature, training, type), type)
  fd<-prob(feature)
  weight_to_train<-train_weights(candidateRules, seq, equal, fixed, rand, diff, alpha)
  for (i in names(candidateRules)) {
    pred_dist<-candidateRules[[i]](feature, task, 'w')
    for (d in names(pred_dist)) {
      weight<-weight_to_train[[i]]
      fd[[d]] <- fd[[d]] + (pred_dist[[d]] * weight)
    }
  }
  return(set(feature, result, fd))
}
get_md_sim <- function(training, task, seq, equal, fixed, rand, diff, alpha) {
  result <- all_selections()
  for (f in features) result <- md_decide(f, training, task, result, seq, equal, fixed, rand, diff, alpha)
  result <- result %>% mutate(prob=color_prob*shape_prob) %>% select(selection, prob)
  return(result)
}
md_sim_for<-function(cond, tid, seq, equal, fixed, rand, diff, alpha, source=df.tasks) {
  dt<-source %>% filter(learningTaskId==paste0('learn0', cond)&trial==tid)
  data <- list(); task <- list()
  data[['agent']]<-as.character(dt$learn_agent)
  data[['target']]<-as.character(dt$learn_target)
  data[['result']]<-as.character(dt$learn_result)
  task[['agent']]<-as.character(dt$agent)
  task[['target']]<-as.character(dt$target)
  
  pred<-get_md_sim(data, task, seq, equal, fixed, rand, diff, alpha)
  pred$learningTaskId<-paste0('learn0', cond)
  pred$trial<-tid
  pred$sequence<-seq
  pred<-pred %>% select(learningTaskId, sequence, trial, selection, prob)
  return(pred)
} 

md_sim_for(1, 1, 'reverse', 10, 1, 1, 1, 0.8)

# Courtesy to Tia
full_model<-function(equal, fixed, rand, diff, alpha){
  likeli=c()
  for (i in 1:6) {
    for (j in 1:15) {
      for (k in c('default', 'reverse')) {
        behavorial<-df.sels%>%
          filter(learningTaskId==paste0('learn0', i)&trial==j&sequence==k) %>%
          select(selection, n)
        pred<-md_sim_for(i, j, k, equal, fixed, rand, diff, alpha)%>% select(selection, prob)
        
        behavorial$selection<-as.character(behavorial$selection) 
        pred$selection<-as.character(pred$selection)
        
        data<-behavorial%>%left_join(pred, by='selection')
        likeli=c(likeli, sum(log(data$prob)*data$n))
      }
    }
  }
  -sum(likeli)
}

full_model(10,1,1,1,0.1)
mle(full_model, start = list(equal=1, fixed=1, rand=1, diff=1, alpha=1)) %>% attributes()

2803.385 * 2 + 4 * log(1800)

#equal       fixed        rand        diff       alpha 
# 2.51091067 -0.05068758  0.46434372  0.05399570  0.52024053


# Then give the probabilities with fitted weights
df.ft<-md_sim_for(1, 1, 'default', 2.51091067, -0.05068758, 0.46434372, 0.05399570, 0.52024053)
df.ft<-rbind(df.ft, md_sim_for(1, 1, 'reverse', 2.51091067, -0.05068758, 0.46434372, 0.05399570, 0.52024053))
for (i in 1:6) {
  for (j in 1:15) {
    for (k in c('default', 'reverse')) {
      if(!(i==1&j==1)) df.ft<-rbind(df.ft, md_sim_for(i, j, k, 2.51091067, -0.05068758, 0.46434372, 0.05399570, 0.52024053))
    }
  }
}

save(df.ft, df.weighted, )

save(df.sw, df.tw, df.tasks, df.sels, df.mod, df.rq_plot, df.ft,
     file = 'cogsci_20200201.Rdata')







