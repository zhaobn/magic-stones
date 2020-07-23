
#######################################################################
####  Libs ####
options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
library(viridis)

eps<-.Machine$double.eps

#######################################################################
####  Task configs ####
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

#######################################################################
####  Helper functions ####
# Returns a dict, key is abbreviated feature, value is full f. name
#   @features {list} key is full f. name
abbr_feature<-function(features) {
  f_dict<-list()
  for (n in names(features)) {
    abbr<-substr(n, 1, 1)
    f_dict[[abbr]]<-n
  }
  return(f_dict)
}
# Returns a vector of all possible objects
#   @features {list}
get_all_objs<-function(features) {
  objs<-c()
  for (f in features[[1]]) {
    for (g in features[[2]]) {
      objs<-c(objs, paste0(f, obj_sep, g))
    }
  }
  return(objs)
}
# Configure all possible objects
all_objs<-get_all_objs(features)

# Read tasks from a dataframe
tasks_from_df<-function(lid, seq='near', src=df.gen_trials) {
 taskdf<-src%>%filter(learningTaskId==paste0('learn0',lid))%>%
   mutate(task=paste0(agent,',',recipient))
 if (seq=='near') return(taskdf$task) else return(rev(taskdf$task))
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

# Returns a softmaxed vector
#   @base {int} reverse softmax temperature parameter, higher the tighter
softmax<-function(vec, base=0, type='') {
  if (type!='log') {
    v_exp<-exp(vec*base); sum<-sum(v_exp)
  } else {
    v_exp<-exp(log(vec)*base); sum<-sum(v_exp)
  }
  return(v_exp/sum)
}
# Returns a softmaxed vector of trials
softmax_trials<-function(vec, base, type) {
  t<-c()
  for (i in seq(1,length(vec),length(all_objs))) {
    to_softmax<-vec[c(i:(i+length(all_objs)-1))]
    results<-softmax(to_softmax, base, type)
    t<-c(t, results)
  }
  return(t)
}
# Stringify a data-point list
flatten<-function(list, sep=',') {
  str=c()
  for (i in 1:length(list)) str<-c(str, list[[i]])
  return(paste(str, collapse=sep))
}
# Turn a string version of task into list
to_list<-function(str, sep=',') {
  vecs<-strsplit(str, sep)[[1]]
  data<-list()
  data[['agent']]<-vecs[1]
  data[['recipient']]<-vecs[2]
  if (length(vecs) > 2) data[['result']]<-vecs[3]
  return(data)
}


#######################################################################
####  Core functions: Categorization ####
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


####  Core functions: PCFGs ####
# Returns all (universal) hypotheses
#   @features {list} see task config
get_all_hypos<-function(features) {
  per_feature<-function(feature) {
    hypos<-c()
    f<-substr(feature, 1, 1)
    obs<-paste0(f, c('(A)', '(R)')) # relative references
    obs<-c(obs, features[[feature]]) # exact references
    
    for (r in relations) {
      for (o in obs) {
        hypos<-c(hypos, paste0(f, '(T)', r, o))
      }
    }
    return(hypos)
  }
  hypos<-features
  for (f in names(features)) hypos[[f]]<-per_feature(f)
  
  hypo<-c()
  # First, add all single hypos
  hypo<-c(hypo, hypos[[1]])
  hypo<-c(hypo, hypos[[2]])
  # Then, add complex hypos
  for (f in hypos[[1]]) {
    for (g in hypos[[2]]) {
      hypo<-c(hypo, paste0(f,',',g))
    }
  }
  return(hypo)
}

# Returns normative-defined prior for a hypothesis, unnormalized
#   @hypo {string} a comma-separated hypothesis
#   @beta {numeric} size of virtual feature values
get_hypo_prior<-function(hypo, beta=10) {
  ib<-1
  descs<-strsplit(hypo, ',')[[1]]
  for (d in descs) {
    ib<-ib/(length(features)*length(relations))
    to_draw<-if (nchar(d)>6) 2 else beta # relative vs. absolute values
    ib<-ib/to_draw
  }
  return(ib)
}

# Returns P(data | hypothesis)
#   @data {list or string} a full data-point
#   @hypo {string}
#   @beta {numeric} same as above
data_given_hypo<-function(data, hypo, beta=10) {
  li<-1 # li: short for likelihood
  if (typeof(data)!='list') data<-to_list(data)
  
  get_li_per_feature<-function(feature, f_hypo) {
    relation<-substr(f_hypo,5,5)
    target<-if (nchar(f_hypo)<9) substr(f_hypo,6,6) else {
      obj<-if (substr(f_hypo,8,8)=='A') 'agent' else 'recipient'
      read_f(feature, data[[obj]])
    }
    # switch below 0's to eps if needed
    if (relation=='=') {
      li<-if (read_f(feature, data[['result']])==target) 1 else 0
    } else {
      li<-if (read_f(feature, data[['result']])!=target) 1/(beta-1) else 0
    }
    return(li)
  }
  
  # If a hypothesis do not spec all features, default the missing one to unchange
  descs<-strsplit(hypo, ',')[[1]]
  if (length(descs)<length(features)) {
    missed<-if (substr(descs[1],1,1)=='c') 's' else 'c'
    no_change<-paste0(missed, '(T)=', missed, '(R)')
    descs<-c(descs, no_change)
  }
  # Get likelihood
  for (h in descs) {
    f<-if (substr(h, 1, 1)=='c') 'color' else 'shape'
    li<-li*get_li_per_feature(f, h)
  }
  return(li)
}

# Returns predicted outcome
#   @td {list} a partial data-point, with only agent & recipient
#   @hypo {string} a hypothesis
get_hypo_preds<-function(td, hypo) {
  predicted<-vector()
  for (o in all_objs) {
    d<-td
    d[['result']]<-o
    if (data_given_hypo(d, hypo) > 0) predicted<-c(predicted, o)
  }
  return(predicted)
}

# Returns a dataframe with all hypotheses, prior and posterior
#   @ld {list} learning data point
prep_hypos<-function(ld, beta=10, temp=0, type='') {
  all_hypo<-get_all_hypos(features)
  
  df<-data.frame(hypo=all_hypo)%>%mutate(hypo=as.character(hypo))
  n<-nrow(df)
  
  df$prior<-normalize(mapply(get_hypo_prior, df$hypo, rep(beta,n)))
  df$posterior<-(df$prior*mapply(data_given_hypo, rep(flatten(ld),n), df$hypo, rep(beta,n)))
  
  if (type=='log') {
    df$posterior<-softmax(df$posterior, temp, 'log')
  } else {
    df$posterior<-if (temp==0) normalize(df$posterior) else softmax(df$posterior, temp, '')
  }
  return(df)
}

#### Re: simulation scripts ####
# Returns a dataframe to hold simulation results
#   @seq {string} "near", "far"
#   @n_tasks {integer} length(tasks)
init_results<-function(seq='near', n_tasks=15) {
  df<-expand.grid(trial=seq(n_tasks), condition=seq, pred=all_objs, n=0)%>%
    arrange(trial, pred)
}
init_all<-function(seq='near', n_tasks=15) {
  df<-expand.grid(learningTaskId=paste0('learn0', seq(6)),
    trial=seq(n_tasks), condition=seq, pred=all_objs, n=0)%>%
    arrange(learningTaskId, trial, pred)
}

#### Shared stats functions ####
# Note that this required a systematic input data formt
# This function prepares ppt data
fmt_ppt<-function(src=df.sels, type='seq') {
  if (type=='seq') {
    ppt_near<-src%>%filter(sequence=='default')%>%
      mutate(condition='near', learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%
      select(learn_cond, condition, trial, selection, n)
    ppt_far<-src%>%filter(sequence=='reverse')%>%
      mutate(condition='far', learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%
      select(learn_cond, condition, trial, selection, n)
    return(rbind(ppt_near, ppt_far))
  } else {
    ppt<-src%>%filter(sequence=='combined')%>%
      mutate(learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%
      select(learn_cond, trial, selection, n)
    return(ppt)
  }
}
# This function formats model_predictions
fmt_results<-function(df, type='seq') {
  if (type=='seq') {
    df<-df%>%mutate(learn_cond=paste0('L', substr(learningTaskId,7,7)),
                    pred=as.character(pred),
                    condition=as.character(condition))%>%
      select(learn_cond, condition, trial, selection=pred, prob)
  } else {
    df<-df%>%mutate(learn_cond=paste0('L', substr(learningTaskId,7,7)),
                    pred=as.character(pred))%>%
      select(learn_cond, trial, selection=pred, prob)
  }
  return(df)
}
#######################################################################









