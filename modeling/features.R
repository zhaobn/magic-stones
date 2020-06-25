
# Libs ####
options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
library(viridis)

eps<-.Machine$double.eps

#######################################################################
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

#######################################################################
# Helper functions ####
# Returns a vector of tasks, ordered by near/far condition
#   @ld {list} learning data-point
#   @seq {string} "near": near-first transfer sequence, "far": far-first
all_tasks<-function(ld, seq) {
  # Reused from earlier code
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
  
  tasks<-get_trials(ld)$task
  if (seq=='near') return(tasks) else return(rev(tasks))
}
# Returns the list version of a task
#   @task_str {string} string version of a task, separated by comma
read_task<-function(task_str) {
  task<-list()
  task[['agent']]<-strsplit(task_str,',')[[1]][1]
  task[['recipient']]<-strsplit(task_str,',')[[1]][2]
  return(task)
}
# Returns a normalized vector
normalize<-function(vec) {
  sum<-sum(vec); norm<-vec/sum; return(norm)
}

#######################################################################
# Core functions ####
# Returns a vector of category summarize intialized with feature alpha
#   @alpha {numeric}
init_cat<-function(alpha) {
  return(rep(alpha, length(features[[1]])+length(features[[2]])))
}

# Returns a vector of feature counts
#   @obs {list} observed stones
#   @count_type {string} 'A': agent-only, 'AR': both agent and recipient
count_feats<-function(obs, count_type) {
  count_obj_feats<-function(obj) {
    feats<-rep(0,length(feat_dict))
    obj_feats<-strsplit(obj,'')[[1]]
    for (i in 1:length(feat_dict)) if (feat_dict[[i]] %in% obj_feats) feats[i]<-feats[i]+1
    return(feats)
  }
  if (count_type=='A') {
    return(count_obj_feats(obs[['agent']]))
  } else if (count_type=='AR') {
    return(count_obj_feats(obs[['agent']]) + count_obj_feats(obs[['recipient']]))
  }
}

# Returns the number of members of cat(s)
#   @cat {vector or list} category summary
#   @alpha {numeric} **feature** alpha
#   @count_type {string}, see above
get_size<-function(cat, alpha, count_type) {
  # flatten multiple cat summaries
  if (typeof(cat)=='list') {
    cat_summary<-vector()
    for (c in cat) cat_summary<-c(cat_summary, c)
  } else {
    cat_summary<-cat
  }
  n_cat<-if (typeof(cat)=='list') length(cat) else 1
  n_els<-(sum(cat_summary)-n_cat*sum(init_cat(alpha)))
  n_feats<-length(features)
  if (count_type=='A') return(n_els/n_feats) else return((n_els/n_feats)/2) # account for double-counting
}

# Returns the probability of belonging to a category
# according to the Chinese restaurant process
# see https://www.cs.princeton.edu/courses/archive/fall07/cos597C/scribe/20070921.pdf
#   @cat {vector} a category summary
#   @cats {list} all existing categories
#   @is_new {boolean} whether belong to a new category
#   @feat_alpha, @crp_alpha, @count_type, see above
cat_prior<-function(cat, cats, feat_alpha, crp_alpha, count_type, is_new=F) {
  denom<-get_size(cats, feat_alpha, count_type)+1-1+crp_alpha
  num<-if (is_new) crp_alpha else get_size(cat, feat_alpha, count_type)
  return(num/denom)
}

# Returns p(stone|cat_summary)
#   @obs {list} observed stones
#   @cat {vector} a cat summary
#   @count_type {string} see above
stone_likeli<-function(obs, cat, count_type) {
  likeli_per_feat<-function(obs_feat, cat_feat) {
    observed<-sum(obs_feat*cat_feat)
    total<-sum(replace(obs_feat, obs_feat==0, 1)*cat_feat)
    return(observed/total)
  } 
  
  obs_feats<-count_feats(obs, count_type)
  
  n_colors<-length(features[["color"]])
  n_els<-n_colors+length(features[["shape"]])
  
  c_prob<-likeli_per_feat(obs_feats[0:n_colors], cat[0:n_colors])
  s_prob<-likeli_per_feat(obs_feats[(n_colors+1):n_els], cat[(n_colors+1):n_els])
  
  return(c_prob*s_prob)
}

# Returns simulated category assignment
#   @ld {list}, @tasks {vector of string}, 
#   @feat_alpha {numeric}, @crp_alpha {numeric}
#   @count_type {string}: "A", "AR"
sim_feat_cat<-function(ld, tasks, feat_alpha, crp_alpha, count_type) {
  # Assign ld to first cat
  cats<-list()
  cats[[1]]<-init_cat(feat_alpha)+count_feats(ld, count_type)
  
  # Greedily assign categories
  for (i in 1:length(tasks)) {
    td<-read_task(tasks[i])
    # Check the probability of belonging to each existing category
    unnorm_probs<-vector()
    for (ci in (1:length(cats))) {
      p_cat<-stone_likeli(td, cats[[ci]], count_type)*
             cat_prior(cats[[ci]], cats, feat_alpha, crp_alpha, count_type, F)
      unnorm_probs<-c(unnorm_probs, p_cat)
    }
    # Or creating a new category
    p_new<-stone_likeli(td, init_cat(feat_alpha), count_type)*
           cat_prior(c(), cats, feat_alpha, crp_alpha, count_type, T)
    unnorm_probs<-c(unnorm_probs, p_new)
    # Normalize  
    probs<-normalize(unnorm_probs)
    # Assign cat accordinly
    cat_indexes<-seq(length(cats)+1)
    assigned_ci<-sample(cat_indexes, 1, prob=probs)
    if (assigned_ci>length(cats)) {
      cats[[assigned_ci]]<-init_cat(feat_alpha)+count_feats(td, count_type)
    } else {
      cats[[assigned_ci]]<-cats[[assigned_ci]]+count_feats(td, count_type)
    }
  }
  
  return(length(cats))
  #return(cats)
}

sim_feat_cat(ld, all_tasks(ld, 'near'), 0.2, 0.5, 'AR')

#######################################################################
# Simulation results ####
# Returns average categories per n simulations
#   @n {integer} n runs
#   @seq {string} "near", "far"
#   @type {string} "agent-only" or "agent, "pair"
#   @feat_alpha {numeric} larger => more categories
#   @crp_alpha {numeric} larger => more categories
get_avg_cats<-function(n, seq, type, feat_alpha, crp_alpha) {
  total<-0; n_run<-n;
  count_type<-if (type=='pair') 'AR' else 'A'
  tasks<-all_tasks(ld, seq)
  while (n>0) {
    total<-total+sim_feat_cat(ld, tasks, feat_alpha, crp_alpha, count_type)
    n<-n-1
  }
  return(round(total/n_run,2))
}

get_avg_cats(100, 'near', 'pair', 0.1, 0.1)

## Run sims
#ld<-as.list(df.learn_tasks[1,c(2:4)])
ld<-list("agent"="rs", "recipient"="yc", "result"="ys")

n<-c(10,100,1000)
task_type<-c('near', 'far')
count_type<-c('agent_only', 'pair')
feature_alpha<-c(0.01, 0.1, 0.2, 1)

df.sim<-expand.grid(n=n, condition=task_type, grouping=count_type, feature_alpha=feature_alpha)
df.sim$avg_cats<-mapply(get_avg_cats, df.sim$n, df.sim$condition, df.sim$grouping, df.sim$feature_alpha)

save(df.sim, file='data/greedy_feat.Rdata')

# Plots ####
ggplot(df.sim, aes(x=grouping, y=avg_cats, fill=condition))+
  geom_bar(position="dodge", stat="identity")+
  facet_grid(n~feature_alpha)









