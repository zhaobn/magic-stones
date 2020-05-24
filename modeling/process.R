
# Resuse functions from simulations.R
options("scipen" = 10)
options()$scipen

library(dplyr)

# Demo data
ld<-list("agent"="rs", "recipient"="yc", "result"="ys")
trials<-get_trials(ld)
rev_trials<-data.frame(trial=seq(15),task=rev(trials$task)); rev_trials$task<-as.character(rev_trials$task)

# Theories
li_prob<-post_cat(ld[['result']], 1)

# Priors
priors<-data.frame(trial=seq(15), prob=c(0.5, rep(0,14)), causal=c(0.5, rep(0,14)))
preds<-data.frame(trial=numeric(0), obj=character(0), prob=numeric(0))

# Iterations
for (tid in 1:15) {
  #task<-trials$task[tid]
  task<-rev_trials$task[tid]
  
  df<-data.frame(trial=rep(tid, length(all_objs)), obj=all_objs)%>%mutate(obj=as.character(obj))%>%
    left_join(li_prob, by='obj')%>%rename(prob=post_pred)
  causal<-get_pred_per_task(ld, task, 3)%>%select(obj=pred, causal=prob); causal$obj<-as.character(causal$obj)
  df<-df%>%left_join(causal, by='obj')
  
  prob_prior<-(priors%>%filter(trial==tid)%>%select(prob))[[1]]
  causal_prior<-(priors%>%filter(trial==tid)%>%select(causal))[[1]]
  
  df<-df%>%
    mutate(prob_pr=prob_prior, causal_pr=causal_prior)%>%
    mutate(post=prob*prob_pr+causal*causal_pr)%>%
    mutate(prob_post=post*prob*prob_pr, causal_post=post*causal*causal_pr)
  
  # Store selections
  preds<-rbind(preds, (df%>%select(trial, obj, prob=post)))
  
  # Update prior
  prob_raw<-sum(df$prob_post)
  causal_raw<-sum(df$causal_post)
  #prob_raw<-sum(df$prob_post)
  #causal_raw<-sum(df$causal_post)
  
  if (tid<15) {
    priors[tid+1,'prob']<-prob_raw/(prob_raw+causal_raw)
    priors[tid+1,'causal']<-causal_raw/(prob_raw+causal_raw)
  }
}










