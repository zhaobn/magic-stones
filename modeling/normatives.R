
source('funcs/preempt.R')

# Define normative model
get_all_hypos<-function(features, is_relative=F) {
  per_feature<-function(feature) {
    hypos<-c()
    f<-substr(feature, 1, 1)
    obs<-paste0(f, c('(A)', '(R)'))
    if (!is_relative) obs<-c(obs, features[[feature]])
    
    for (r in relations) {
      for (o in obs) {
        hypos<-c(hypos, paste0(f, '(T)', r, o))
      }
    }
    return(hypos)
  }
  hypos<-features
  for (f in names(features)) {
    hypos[[f]]<-per_feature(f)
  }
  hypo<-c()
  for (f in hypos[[1]]) {
    for (g in hypos[[2]]) {
      hypo<-c(hypo, paste0(f,',',g))
    }
  }
  return(hypo)
}
all_hypos<-get_all_hypos(features, F)
rel_hypos<-get_all_hypos(features, T)

get_learned_prior<-function(hypo, data) {
  task<-paste(strsplit(data, ',')[[1]][c(1,2)], collapse=',')
  result<-strsplit(data, ',')[[1]][3]
  post<-get_pred_per_hypo(task, hypo, 9)
  return(post[post$obj==result,'pp'])
}
get_norm_preds<-function(hypos, cid, t, noise=FALSE, learn_src=df.learn_tasks) {
  data<-learn_src[cid, c(2:4)]
  trials<-get_trials(data)
  
  dh<-data.frame(hypo=hypos)%>%mutate(hypo=as.character(hypo))
  dh$prior<-mapply(get_learned_prior, dh$hypo, rep(flatten(data), length(dh$hypo)))
  dh$prior<-normalize(dh$prior)
  
  get_norm_pred_per_task<-function(data, task, t) {
    df<-data.frame(obj=all_objs)%>%mutate(obj=as.character(obj))
    for (i in 1:length(dh$hypo)) {
      hcol<-paste0('h_', i)
      x<-get_pred_per_hypo(task, dh$hypo[i], t)
      x[,hcol]=x$pp*dh[i,'prior']
      df<-df%>%left_join((x[,c('obj', hcol)]), by='obj')
    }
    df<-df%>%mutate(task=task, pred=obj,
                    sum = rowSums(.[2:ncol(df)]))%>%select(task, pred, sum)
    df$prob<-normalize(df$sum)
    return(df[,c('task', 'pred', 'prob')])
  }
  
  df<-data.frame(task=character(0), pred=character(0), prob=numeric(0))
  for (tk in trials$task) {
    df<-rbind(df, get_norm_pred_per_task(data, tk, t))
  }
  
  df<-df%>%mutate(condition=paste0('L', cid), trial=rep(seq(15), each=9))
  dfcols<-c('condition', 'trial', 'task', 'pred', 'prob')
  
  if (noise==FALSE) return(df[,dfcols]) else {
    df$noise<-mapply(rnorm, 1, rep(0.01, length(df$pred)), rep(0.01, length(df$pred)))
    df$prob<-df$prob+df$noise
    return(df[,dfcols])
  }
}

x<-get_norm_preds(all_hypos, 1, 3.19, F)
y<-get_norm_preds(rel_hypos, 1, 3.19, F)

# Fit parameters
full_model<-function(t) {
  ppt<-df.sels%>%filter(sequence=='combined')%>%select(learningTaskId, trial, selection, n)
  pred<-data.frame(learningTaskId=character(0), trial=numeric(0), selection=character(0), prob=numeric(0))
  
  for (i in 1:6) {
    ld<-as.list(df.learn_tasks[i,c(2:4)])
    df<-get_norm_preds(get_all_hypos(features, T), ld, t, F)%>%
      mutate(learningTaskId=paste0('learn0', i), trial=rep(seq(15),each=9))%>%
      select(learningTaskId, trial, selection=pred, prob)
    pred<-rbind(pred, df)
  }
  
  df<-ppt%>%left_join(pred,by=c('learningTaskId', 'trial', 'selection'))
  likeli<-sum(log(df$prob)*df$n)
  
  -likeli
}
library(stats4)
mle(full_model, start=list(t=10))%>%attributes() # 3.19

# Introduce inductive prior

# Stats and plots

# Debugs

