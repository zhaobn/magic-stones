
# Resuse functions from simulations.R
options("scipen" = 10)
options()$scipen

library(dplyr)

# Inference model to be debugged
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


##########################################################################################
## Mixture model
context_hypo<-function(data, seq) {
  break_hypos<-function(hypo, hc) {
    hc[[1]]<-unique(c(hc[[1]], strsplit(hypo, ',')[[1]][1]))
    hc[[2]]<-unique(c(hc[[2]], strsplit(hypo, ',')[[1]][2]))
    return(hc)
  }
  contrast_hypo<-function(hypos, radical) {
    eq<-c()
    for (h in hypos) if (grepl('\\=',h)) eq<-c(eq, h)
    if (length(eq)>0) {
      obj<-substr(eq, 8, 8); 
      if (radical|(obj=='R')) {
        new<-setdiff(c('A', 'R'), obj)
        con<-gsub(obj, new, eq); con<-gsub('\\=', '~', con)
        hypos<-unique(c(hypos, con))
      }
    }
    return(hypos)
  }
  
  hypos<-compose_hypo(data)
  
  hypo_components<-list('color'=c(), 'shape'=c())
  for (h in hypos) hypo_components<-break_hypos(h, hypo_components)
  
  # Near transfer: recipient varies by one feature
  # Far transfer: both agent and recipient are completely different
  radical_change<-if (seq=='near') F else T
  for (i in 1:length(hypo_components)) {
    hypo_components[[i]]<-contrast_hypo(hypo_components[[i]], radical_change)
  }
  
  hps<-c()
  for (f in hypo_components[[1]]) {
    for (g in hypo_components[[2]]) {
      hps<-c(hps, paste0(f, ',', g))
    }
  }
  
  return(hps)
}
context_hypo(as.list(df.learn_tasks[1,c(2:4)]),'far')

get_context_preds<-function(cid, seq, t, noise=FALSE) {
  data<-as.list(df.learn_tasks[cid,c(2:4)])
  get_pred_per_task<-function(data, seq, task, t) {
    hypo<-context_hypo(data, seq)
    df<-data.frame(obj=all_objs); df$obj<-as.character(df$obj)
    for (i in 1:length(hypo)) {
      hcol<-paste0('h_', i)
      x<-get_pred_per_hypo(task, hypo[i], t)
      df<-df%>%left_join(x, by='obj')%>%rename(!!hcol:=pp)
    }
    df<-df%>%mutate(task=task, pred=obj, sequence=seq, condition=paste0('L', cid),
                    sum = rowSums(.[2:ncol(df)]))
    df$prob<-normalize(df$sum)
    return(df[,c('sequence', 'condition', 'task', 'pred', 'prob')])
  }
  
  df<-data.frame(task=character(0), obj=character(0), prob=numeric(0))
  tasks<-get_trials(data)$task
  for (tk in tasks) {
    df<-rbind(df, get_pred_per_task(data, seq, tk, t))
  }
  df$trial<-rep(seq(15), each=9)
  
  if (noise==TRUE) {
    df$noise<-mapply(rnorm, 1, rep(0.01, length(df$pred)), rep(0.01, length(df$pred)))
    df$prob<-df$prob+df$noise
  }
  return(df[,c('sequence', 'condition', 'trial', 'task', 'pred', 'prob')])
}


df.process<-get_context_preds(1,'near',3.19, F)
for (i in 1:6) {
  for (s in c('near', 'far')) {
    if (!(i==1&s=='near')) df.process<-rbind(df.process, get_context_preds(i,s,3.19, F))
  }
}
df.process$trial<-factor(df.process$trial, levels=rev(seq(15)))
x<-df.process%>%filter(condition=='L1')
ggplot(df.process, aes(pred, trial, fill=prob)) + geom_tile() + 
  scale_fill_viridis(option="E", direction=-1) +
  #scale_fill_gradient(low="white", high="black") +
  facet_grid(condition~sequence)

## Add prior likelihoods





































