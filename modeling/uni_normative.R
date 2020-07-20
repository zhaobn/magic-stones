
# Load library and functions
source("./functions.R")
load("../behavioral_data/tasks.Rdata")
load("../behavioral_data/aggregated.Rdata")

#######################################################################
# Get normative predictions ####
get_cond_pred<-function(lid, par=3, temp=0) {
  get_trial_pred<-function(tid, par, temp) {
    prediction<-list(); for(obj in all_objs) prediction[[obj]]<-0
    td<-as.list(tasks[tid, c(3:4)])
    for (i in 1:nrow(dh)) {
      hp<-dh$hypo[i]; posterior<-dh$posterior[i]
      predicted<-get_hypo_preds(td, hp)
      for (pt in predicted) {
        size<-if (length(predicted)>1) par-1 else 1
        prediction[[pt]]<-prediction[[pt]]+posterior/size
      }
    }
    
    p<-data.frame(matrix(unlist(prediction), nrow=length(prediction), byrow=T))
    colnames(p)<-c('prob')
    p$prob<-if (temp==0) normalize(p$prob) else softmax(p$prob, temp)
    
    return(cbind(data.frame(learningTaskId=paste0('learn0',lid), trial=tid, pred=all_objs), p))
  }
  # Prep data
  ld<-as.list(df.learn_tasks[lid,c(2:4)])
  tasks<-df.gen_trials%>%filter(learningTaskId==paste0('learn0',lid))
  dh<-prep_hypos(ld, par)
  
  # Make prediction
  df<-get_trial_pred(1, par, temp)  
  for (i in 2:15) df<-rbind(df, get_trial_pred(i, par, temp))
  return(df)
}

# Take a look ####
df<-get_cond_pred(1, 3, 0)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, 3, 0))

ggplot(df, aes(x=pred, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(~learningTaskId)

# Likelihood & fitting ####
ppt_data<-fmt_ppt(df.sels)

# Fit feature_size only
fit_fsize<-function(par, data) {
  df<-get_cond_pred(1, par, 0)
  for (i in 2:6) df<-rbind(df, get_cond_pred(i, par, 0))
  df<-fmt_results(rbind(df%>%mutate(condition='near'), df%>%mutate(condition='far')))
  return(-sum(log(df$prob)*data$n))
}
#fit_fsize(3, ppt_data)
out=optim(par=3, fn=fit_fsize, data=ppt_data, method='Brent', lower=3, upper=100)
# par = 3.83, l = 2598.68

# fit softmax
df<-get_cond_pred(1, 3, 0)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, 3, 0))
df<-fmt_results(rbind(df%>%mutate(condition='near'), df%>%mutate(condition='far')))

fit_softmax<-function(par, df, ppt) {
  # apply softmax per trial
  probs<-df$prob
  t<-c()
  for (i in seq(1,nrow(df),length(all_objs))) {
    to_softmax<-probs[c(i:(i+length(all_objs)-1))]
    results<-softmax(to_softmax, par)
    t<-c(t, results)
  }
  # get total likelihood
  return(-sum(log(t)*ppt$n))
}
#fit_softmax(5, df, ppt_data)
out=optim(par=5, fn=fit_softmax, df=df, ppt=ppt_data, method='Brent', lower=1, upper=100)
# par=6.37, l=2754

df<-get_cond_pred(1, 3.83, 0)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, 3.83, 0))
df<-fmt_results(rbind(df%>%mutate(condition='near'), df%>%mutate(condition='far')))
out=optim(par=5, fn=fit_softmax, df=df, ppt=ppt_data, method='Brent', lower=1, upper=100)
# par=5.17, l=2726 ???

# have a look at softmaxed data
x<-df
x$soft_maxed<-t
ggplot(df, aes(x=selection, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(~learn_cond)
# seems fine
x<-x%>%left_join(ppt_data, by=c('learn_cond', 'condition', 'trial', 'selection'))
sum(log(x$prob)*x$n) # 2598
sum(log(x$soft_maxed)*x$n) # 3296
# Is it because of a condition?
ll_per_cond<-function(src, lid) {
  cond<-src%>%filter(condition=='near'&learn_cond==paste0('L',lid))
  p_ll<-sum(log(cond$prob)*cond$n)
  s_ll<-sum(log(cond$soft_maxed)*cond$n)
  return(data.frame(learn_cond=paste0('L',lid), p_ll=p_ll, s_ll=s_ll))
}
ll_per_cond(x,1)

cond_nonzero<-cond%>%filter(n>0)
cond_nonzero$p_log<-log(cond_nonzero$prob)
cond_nonzero$s_log<-log(cond_nonzero$soft_maxed)
cond_nonzero$diff<-cond_nonzero$s_log-cond_nonzero$p_log
sum(cond_nonzero$n*cond_nonzero$diff)








