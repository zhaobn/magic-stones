
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
relations<-c('=', '~')

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
  total<-cat+count_feats(obs, count_type)
  # Separate features
  colors<-total[0:length(features[['color']])]
  shapes<-total[(length(features[['color']])+1):length(total)]
  # Keep non-zeros
  colors<-colors[colors!=0]
  shapes<-shapes[shapes!=0]
  # Probabilities
  c_prob = s_prob = 1
  c_total<-sum(colors); s_total<-sum(shapes)
  for (c in colors) c_prob<-c_prob*(c/c_total)
  for (s in shapes) s_prob<-s_prob*(s/s_total)
  
  return(c_prob*s_prob)
}



## args: ld::list, tasks::vector of comma-sep string, count_type::"A", "AR
sim_cat<-function(ld, tasks, count_type) {
  cats<-list()
  ## 1. assign learning data-point to cat 1
  cats[['1']]<-count_feats(ld, count_type)
  ## 2. greedily assign cats
  for (i in 1:length(tasks)) {
    # start with a random cat
    cat_num<-sample(names(cats),1)
    # belong to this cat?
    prob<-cat_prob(read_task(tasks[i]), cats[[cat_num]], count_type)
    if (prob>runif(1)) {
      # add observation feature values to this cat
      cats[[cat_num]]<-cats[[cat_num]]+count_feats(read_task(tasks[i]), count_type)
    } else {
      # check for the next cat or create new cat
     if (length(cats)==1) {
       cats[['2']]<-count_feats(read_task(tasks[i]), count_type)
     } else {
       
     }
    }
  }
  ## 3. for now: count distinct cats
}


# Simulation results ####
## Get data
ld<-as.list(df.learn_tasks[1,c(2:4)])
near_first<-get_trials(ld)
far_first<-near_first%>%arrange(desc(row_number()))

# Plots ####






