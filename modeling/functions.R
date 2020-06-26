
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
# Returns a softmaxed vector
#   @base {int} reverse softmax temperature parameter, higher the tighter
softmax<-function(vec, base=1) {
  v_exp<-exp(vec*base); sum<-sum(v_exp)
  return(v_exp/sum)
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


#######################################################################









