
source('funcs/preempt.R')

# Define normative model
get_all_hypos<-function(features) {
  per_feature<-function(feature) {
    hypos<-c()
    f<-substr(feature, 1, 1)
    obs<-paste0(f, c('(A)', '(R)'))
    obs<-c(obs, features[[feature]])
    
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
all_hypos<-get_all_hypos(features)

# Introduce inductive bias
get_inductive_bias<-function(hypo, bias=3) {
  ib<-1
  descs<-strsplit(hypo, ',')[[1]]
  for (d in descs) {
    penalty<-if (nchar(d)>6) 1 else 1/bias
    ib<-ib*penalty
  }
  return(ib)
}

# Update with learning task
get_learned_prior<-function(hypo, data, prior=0) {
  task<-paste(strsplit(data, ',')[[1]][c(1,2)], collapse=',')
  result<-strsplit(data, ',')[[1]][3]
  post<-get_pred_per_hypo(task, hypo, 9)
  post_pred<-post[post$obj==result,'pp']
  if (prior<=0) return(post_pred) else {
    pr<-get_inductive_bias(hypo, prior)
    return(post_pred*pr)
  }
}

# Generalization posterior predictive
get_norm_preds<-function(hypos, cid, t, bias, noise=FALSE, learn_src=df.learn_tasks) {
  data<-learn_src[cid, c(2:4)]
  trials<-get_trials(data)
  
  dh<-data.frame(hypo=hypos)%>%mutate(hypo=as.character(hypo))
  n<-length(dh$hypo)
  dh$prior<-mapply(get_learned_prior, dh$hypo, rep(flatten(data),n), rep(bias,n))
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
#x<-get_norm_preds(all_hypos, 1, 3.19, 0, F)
#y<-get_norm_preds(rel_hypos, 1, 3.19, 3, F)

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

# Plotting
plot_pred_hm<-function(data) {
  g<-ggplot(data, aes(pred, task, fill=prob)) + 
    geom_tile() + 
    scale_fill_gradient(low="white", high="black")
  return(g)
}
#plot_pred_hm(y)

# Checks and stats
df.norm<-get_norm_preds(all_hypos, 1, 3.19, 0, F)
for (i in 2:6) df.norm<-rbind(df.norm, get_norm_preds(all_hypos, i, 3.19, 0, F))
df.norm$model<-'normative'
save(df.norm, file='normative.Rdata')

relative<-get_norm_preds(all_hypos, 1, 3.19, 3, F)
for (i in 2:6) relative<-rbind(relative, get_norm_preds(all_hypos, i, 3.19, 3, F))
relative$model<-'bias_3'

big_diff<-get_norm_preds(all_hypos, 1, 3.19, 100, F)
for (i in 2:6) big_diff<-rbind(big_diff, get_norm_preds(all_hypos, i, 3.19, 100, F))
big_diff$model<-'bias_100'

x<-rbind(df.norm, relative, big_diff)
x$model<-factor(x$model, levels=c('normative', 'bias_3', 'bias_100'))
ggplot(x, aes(pred, trial, fill=prob)) + geom_tile() + 
  scale_fill_viridis(option="E", direction=-1) +
  scale_y_continuous(tran ="reverse", breaks = unique(x$trial)) +
  facet_grid(model~condition)

# Likelihood
ppt<-df.sels%>%filter(sequence=='combined')%>%
  mutate(condition=paste0('L', substr(learningTaskId,7,7)))%>%
  select(condition, trial, pred=selection, n)
total_likeli<-function(model_name, model_src=df.norm, ppt_src=ppt) {
  dm<-model_src%>%filter(model==model_name)%>%select(condition, trial, pred, prob)
  df<-ppt_src%>%left_join(dm, by=c('condition', 'trial', 'pred'))
  likeli<-sum(df$n*log(df$prob))
  return(likeli)
}
total_likeli('normative', x)
total_likeli('bias_3', x)
total_likeli('bias_100', x)

save(df.norm, file='normatives.Rdata')

# Plot with ppt data
names(df.norm)
x<-df.norm%>%select(condition, trial, pred, prob, model)
ppt<-df.sels%>%filter(sequence=='combined')%>%
  mutate(condition=paste0('L', substr(learningTaskId,7,7)), model='mturk_combined')%>%
  select(condition, trial, pred=selection, prob=freq, model)
x<-rbind(x, ppt)























