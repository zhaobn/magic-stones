
# Libs ####
options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
library(viridis)

eps<-.Machine$double.eps

# Task configs ####
features<-list()
features[['color']]<-c('b', 'r', 'y') # blue, red, yellow
features[['shape']]<-c('c', 'd', 's') # circle, diamond, square

feat_dict<-c(features[[1]],features[[2]])

obj_sep=''

read_f<-function(feature, obj) {
  f_idx<-if (feature=='color') 1 else 2
  return(substr(obj, f_idx, f_idx))
}

# Helper functions ####
get_trials<-function(data) {
  diff_fval<-function(feature, v1, v2) {
    return(setdiff(features[[feature]], c(v1, v2)))
  }
  af<-read_f(names(features)[1], data[['agent']])
  ag<-read_f(names(features)[2], data[['agent']])
  rf<-read_f(names(features)[1], data[['recipient']])
  rg<-read_f(names(features)[2], data[['recipient']])
  df<-sample(diff_fval(names(features)[1], af, rf), 1)
  dg<-sample(diff_fval(names(features)[2], ag, rg), 1)
  
  ta_f<-c(rep(af,3),rep(df,4),rep(af,4),rep(df,4))
  ta_g<-c(rep(ag,3),rep(ag,4),rep(dg,4),rep(dg,4))
  tr_f<-c(df, rep(c(rf,df),7))
  tr_g<-c(rg,dg,dg,rep(c(rg,rg,dg,dg),3))
  
  df<-data.frame(trial=seq(15), ta_f, ta_g, tr_f, tr_g)
  df<-df%>%
    mutate(agent=paste0(ta_f,obj_sep,ta_g), recipient=paste0(tr_f,obj_sep,tr_g))%>%
    mutate(task=paste0(agent, ',', recipient))%>%
    select(trial, task, agent, recipient)
  return(df)
}
read_task<-function(task_str) {
  task<-list()
  task[['agent']]<-strsplit(task_str,',')[[1]][1]
  task[['recipient']]<-strsplit(task_str,',')[[1]][2]
  return(task)
}

# Core functions ####
# return::vector
init_cat<-function(alpha) {
  return(rep(alpha, length(features[[1]])+length(features[[2]])))
}

## args: obs::list, count_type::'A' - agent only, 'AR' - agent and recipient
## return: feats::vector, reference with feat_dict in task config
count_feats<-function(obs, count_type) {
  count_obj_feats<-function(obj) {
    feats<-rep(0,length(feat_dict))
    obj_feats<-strsplit(obj,'')[[1]]
    for (i in 1:length(feat_dict)) {
      if (feat_dict[[i]]%in%obj_feats) feats[i]<-feats[i]+1
    }
    return(feats)
  }
  if (count_type=='A') {
    return(count_obj_feats(obs[['agent']]))
  } else if (count_type=='AR') {
    return(count_obj_feats(obs[['agent']]) + count_obj_feats(obs[['recipient']]))
  }
}

## args: obs::list, cat::vector, count_type::"A", "AR"
## return: prob::float
cat_prob<-function(obs, cat, count_type) {
  cat_prob_per_feat<-function(obs_feat, cat_feat) {
    presented<-cat_feat[which(obs_feat==1)]
    return(sum(presented)/sum(cat_feat))
  }
  obs_feats<-count_feats(obs, count_type)
  c_prob<-cat_prob_per_feat(obs_feats[0:length(features[[1]])],cat[0:length(features[[1]])])
  s_prob<-cat_prob_per_feat(obs_feats[(length(features[[1]])+1):length(obs_feats)], cat[(length(features[[1]])+1):length(cat)])
  return(c_prob*s_prob)
}

## return: cats::list of vectors
update_or_new<-function(obs, cat_index, cats, alpha, count_type) {
  cat<-cats[[cat_index]]
  if (runif(1)<cat_prob(obs, cat, count_type)) { # update
    cats[[cat_index]]<-cat+count_feats(obs, count_type)
  } else { # new
    add_to_end<-length(cats)+1
    cats[[add_to_end]]<-init_cat(alpha)+count_feats(obs, count_type)
  }
  return(cats)
}

## return: cats::list of vectors
assign_cats<-function(obs, cats, alpha, count_type) {
  current_cats<-seq(length(cats))
  updated<-FALSE
  while(length(current_cats)>1) {
    check_cat<-sample(current_cats, 1)
    if (runif(1)<cat_prob(obs, cats[[check_cat]], count_type)) { # add to this cat
      cats[[check_cat]]<-cats[[check_cat]]+count_feats(obs, count_type)
      updated<-TRUE
      break
    } else { # check for next cat
      current_cats<-setdiff(current_cats, check_cat)
    }
  }
  if (!updated) {
    left_index<-current_cats[1]
    cats<-update_or_new(obs, left_index, cats, alpha, count_type)
  }
  return(cats)
}

## return: tasks::vector
get_tasks<-function(ld, seq, count_type) {
  tasks<-get_trials(ld)$task
  if (count_type=="pair"|count_type=='AR') {
    if (seq=='near') return(tasks) else return(rev(tasks))
  } else {
    agents<-c()
    for (t in tasks) agents<-c(agents, strsplit(t, ',')[[1]][1])
    agents<-unique(agents)
    if (seq=='near') return(agents) else return(rev(agents))
  }
}

## return: n cats: int
sim_feat_cat<-function(ld, tasks, feat_alpha, count_type) {
  # Assign ld to first cat
  cats<-list()
  cats[[1]]<-init_cat(feat_alpha)+count_feats(ld, count_type)
  
  # Greedily assign categories according to feature similarity
  for (i in 1:length(tasks)) {
    td<-read_task(tasks[i])
    cats<-assign_cats(td, cats, feat_alpha, count_type)
  }
  
  # For now: count distinct categories
  return(length(cats))
  #return(cats)
}

sim_feat_cat(ld, get_tasks(ld, 'near', 'AR'), 0.01, 'AR')

# Simulation results ####
## Get data
ld<-as.list(df.learn_tasks[1,c(2:4)])
# ld<-list("agent"="rs", "recipient"="yc", "result"="ys")

get_avg_cats<-function(n, seq, type, alpha) {
  total<-0; n_run<-n;
  count_type<-if (type=='pair') 'AR' else 'A'
  tasks<-get_tasks(ld, seq, count_type)
  while (n>0) {
    total<-total+sim_feat_cat(ld, tasks, alpha, count_type)
    n<-n-1
  }
  return(round(total/n_run,2))
}

get_avg_cats(500, 'near', 'agent_only', 0.1)

## Run sims
n<-c(10,100,500)
task_type<-c('near', 'far')
count_type<-c('agent_only', 'pair')
feature_alpha<-c(0.01, 0.1, 0.2, 1)

df.sim<-expand.grid(n=n, condition=task_type, grouping=count_type, feature_alpha=feature_alpha)
df.sim$avg_cats<-mapply(get_avg_cats, df.sim$n, df.sim$condition, df.sim$grouping, df.sim$feature_alpha)

save(df.sim, file='greedy_feat.Rdata')

# Plots ####
ggplot(df.sim, aes(x=grouping, y=avg_cats, fill=condition))+
  geom_bar(position="dodge", stat="identity")+
  facet_grid(n~feature_alpha)

# Tests ####
df<-expand.grid(n=500, condition=task_type, grouping='agent_only', feature_alpha=feature_alpha)
df$avg_cat<-mapply(get_avg_cats, df$n, df$condition, df$grouping, df$feature_alpha)










