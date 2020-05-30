
options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
library(viridis)

eps<-.Machine$double.eps

features<-list()
features[['color']]<-c('b', 'r', 'y') # blue, red, yellow
features[['shape']]<-c('c', 'd', 's') # circle, diamond, square

obj_sep=''
relations<-c('=', '~')

read_f<-function(feature, obj) {
  f_idx<-if (feature=='color') 1 else 2
  return(substr(obj, f_idx, f_idx))
}


# Helper functions
abbr_feature<-function(features) {
  f_dict<-list()
  for (n in names(features)) {
    abbr<-substr(n, 1, 1)
    f_dict[[abbr]]<-n
  }
  return(f_dict)
}
get_all_objs<-function(features) {
  objs<-c()
  for (f in features[[1]]) {
    for (g in features[[2]]) {
      objs<-c(objs, paste0(f, obj_sep, g))
    }
  }
  return(objs)
}

all_objs<-get_all_objs(features)

normalize<-function(vec) {
  sum<-sum(vec); norm<-vec/sum; return(norm)
}
softmax<-function(vec, base=1) {
  v_exp<-exp(vec*base); sum<-sum(v_exp)
  return(v_exp/sum)
}
flatten<-function(list, sep=',') {
  str=c()
  for (i in 1:length(list)) str<-c(str, list[[i]])
  return(paste(str, collapse=sep))
}
to_list<-function(str, sep=',') {
  vecs<-strsplit(str, sep)[[1]]
  data<-list()
  data[['agent']]<-vecs[1]
  data[['recipient']]<-vecs[2]
  if (length(vecs) > 2) data[['result']]<-vecs[3]
  return(data)
}

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
    select(trial, task)
  return(df)
}

get_pred_per_hypo<-function(task, hypo, t=6) {
  get_result_val<-function(feature, desc, task) {
    relation<-substr(desc, 5, 5)
    if (nchar(desc) < 8) {
      t_val<-substr(desc, 6, 6)
    } else {
      t_idx<-if (substr(desc, 8, 8)=='A') 1 else 2
      t_val<-read_f(feature, strsplit(task, ',')[[1]][t_idx])
    }
    if (relation=='=') return(t_val) else {
      return(setdiff(features[[feature]], t_val))
    }
  }
  
  f_dict<-abbr_feature(features)
  f_pred<-list()
  for (d in strsplit(hypo, ',')[[1]]) {
    f<-substr(d,1,1)
    f_pred[[f]]<-get_result_val(f_dict[[f]], d, task)
  }
  
  pred<-c()
  for (f in f_pred[[1]]) {
    for (g in f_pred[[2]]) {
      pred<-c(pred, paste0(f, obj_sep, g))
    }
  }
  
  df<-data.frame(obj=all_objs); df$obj<-as.character(df$obj)
  df$yes<-as.numeric(df$obj%in%pred)
  
  if (t==F) df$pp<-df$yes else df$pp<-softmax(df$yes, t)
  return(df[,c('obj', 'pp')])
}


















