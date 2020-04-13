
library(dplyr)
library(stats4)
rm(list=ls())


# save(df.cf, df.cm, df.cw, df.ft, file='constructive_model.Rdata')
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

fetch <- function(feature, values) {
  fd<-prob(feature); for (v in values) fd[[v]]<-1/length(values)
  return(fd)
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
      vals<-c(vals, get(feature, o))
    }
  } else {
    vals<-c(get(feature, object))
  }
  return(setdiff(settings[[feature]], vals))
}

# Causal engine
parse_effect <- function(feature, data) {
  funcs<-list()
  if (get(feature, data[['result']], '')==get(feature, data[['target']], '')) {
    funcs[[paste0('kept_', feature)]] <- function(feature, data) get(feature, data[['target']])
  } else {
    funcs<-c(funcs, check_cause(feature, data))
  }
  return(funcs)
}
check_cause <- function(feature, data) {
  funcs<-list()
  if (get(feature, data[['result']], '')==get(feature, data[['agent']], '')) {
    funcs[[paste0('match_', feature)]] <- function(feature, data) get(feature, data[['agent']])
  } else {
    #funcs[[paste0('fix_', feature)]] <- function(feature, data) get(feature, data[['target']])
    funcs[[paste0('diff_', feature)]] <- function(feature, data) fetch(feature, exclude(feature, list(data[['agent']], data[['target']])))
    funcs[[paste0('random_', feature)]] <- function (feature, data) fetch(feature, settings[[feature]])
  }
  return(funcs)
}
data<-list('agent'='rc', 'target'='bs', 'result'='rs')
parse_effect('color', data)

get_weight<-function(rule_names, ext) {
  rule_weights<-list()
  for (r in rule_names) {
    if ((substr(r, 1, 3) == 'fix')||(substr(r, 1, 3) == 'ran')) {
      rule_weights[[r]]<-ext
    } else {
      rule_weights[[r]]<-1
    }
  }
  total <- Reduce("+", rule_weights)
  for (r in names(rule_weights)) rule_weights[[r]] <- rule_weights[[r]]/total
  return(rule_weights)
}

softmaxList<-function(lt, base) {
  exps<-c()
  for (i in 1:length(lt)) exps<-c(exps, exp(base*lt[[i]]))
  total<-sum(exps)
  for (i in 1:length(lt)) lt[[i]]<-exps[i]/total
  return(lt)
}

decide <- function(feature, training, task, result, seq, conf = 3, ext = 0.5) {
  rules <- parse_effect(feature, training)
  # If far-first, include more uncertain rules
  if (seq=='reverse') {
    rules[[paste0('fix_', feature)]] <- function(feature, data=training) get(feature, data[['agent']])
    rules[[paste0('random_', feature)]] <- function (feature, data) fetch(feature, settings[[feature]])
  }
  fd<-prob(feature)
  weights<-get_weight(names(rules), ext)
  for (r in names(rules)) {
    pred_dist<-rules[[r]](feature, task)
    for (d in names(pred_dist)) {
      fd[[d]] <- fd[[d]] + (pred_dist[[d]] * weights[[r]])
    }
  }
  fd<-softmaxList(fd, conf)
  return(set(feature, result, fd))
}


task<-list('agent'='bs', 'target'='bs')
result <- all_selections()
decide('color', data, task, result, 'reverse')

# Combine features
get_sim <- function(training, task, seq, conf=3, ext=0.5) {
  result <- all_selections()
  for (f in features) result <- decide(f, training, task, result, seq, conf, ext)
  result <- result %>% mutate(prob=color_prob*shape_prob) %>% select(selection, prob)
  return(result)
}

get_sim(data, task, 'default')

sim_for<-function(cond, tid, seq, conf = 3, ext = 0.5, source=df.tasks) {
  dt<-source %>% filter(learningTaskId==paste0('learn0', cond)&trial==tid)
  data <- list(); task <- list()
  data[['agent']]<-as.character(dt$learn_agent)
  data[['target']]<-as.character(dt$learn_target)
  data[['result']]<-as.character(dt$learn_result)
  task[['agent']]<-as.character(dt$agent)
  task[['target']]<-as.character(dt$target)
  
  pred<-get_sim(data, task, seq, conf, ext)
  pred$learningTaskId<-paste0('learn0', cond)
  pred$sequence<-seq
  pred$trial<-tid
  pred<-pred %>% select(learningTaskId, sequence, trial, selection, prob)
  return(pred)
} 

sim_for(1, 1, 'default')
sim_for(1, 1, 'reverse')

full_model<-function(conf,ext){
  likeli=c()
  for (i in 1:6) {
    for (j in 1:15) {
      for (k in c('default', 'reverse')) {
        behavorial<-df.sels%>%
          filter(learningTaskId==paste0('learn0', i)&trial==j&sequence==k) %>%
          select(selection, n)
        pred<-sim_for(i, j, k, conf, ext)%>% select(selection, prob)
        
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
mle(full_model, start = list(conf=3,ext=0.5)) #%>% attributes()


defaults<-sim_for(1, 1, 'default', 2.460499570, 0.005155454)
for (i in 1:6) {
  for (j in 1:15) if (!(i==1&j==1)) defaults<-rbind(defaults, sim_for(i,j, 'default', 2.460499570, 0.005155454))
}
reverses<-sim_for(1, 1, 'reverse', 2.460499570, 0.005155454)
for (i in 1:6) {
  for (j in 1:15) if (!(i==1&j==1)) reverses<-rbind(reverses, sim_for(i,j, 'reverse', 2.460499570, 0.005155454))
}
df.order<-rbind(defaults, reverses)
df.order$learningTaskId<-as.character(df.order$learningTaskId)
df.order$sequence<-as.character(df.order$sequence)
df.order$selection<-as.character(df.order$selection)
df.order$trial<-as.numeric(as.character(df.order$trial))

save(df.order, file='order_model.Rdata')









