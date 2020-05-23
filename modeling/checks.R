
# Note: reuse functions from simulations.R

# Task configs
features<-list()
features[['color']]<-c('b', 'r', 'y') # blue, red, yellow
features[['shape']]<-c('c', 'd', 's') # circle, diamond, square

obj_sep=''
all_objs<-get_all_objs(features)

## Override custom functions
read_f<-function(feature, obj) {
  f_idx<-if (feature=='color') 1 else 2
  return(substr(obj, f_idx, f_idx))
}


# Plot normative predictions vs. mturk results
fmt_task_axis<-function(func, data, param, noise) {
  df<-func(data, param, noise)
  df$task<-factor(df$task, levels=rev(get_trials(data)$task))
  return(df)
}
fmt_ppt_data<-function(cid, seq, ld, src=df.sels) {
  trials<-get_trials(ld)
  df<-df.sels%>%filter(learningTaskId==cid&sequence==seq)%>%select(trial, pred=selection, prob=freq)
  df<-df%>%left_join(trials, by='trial')%>%select(task, pred, prob)
  df$task<-factor(df$task, levels=rev(trials$task))
  return(df)
}

# Plot per condition
get_cond_data<-function(cid, alpha=1, temperature=3, src=df.learn_tasks) {
  ld<-as.list(src%>%filter(learningTaskId==cid)%>%select(agent, recipient, result))
  
  prob<-get_cat_preds(ld, alpha, T)%>%mutate(type='probablistic')
  causal<-get_causal_preds(ld, temperature, T)%>%mutate(type='causal')
  default<-fmt_ppt_data(cid, 'default', ld)%>%mutate(type='near')
  reverse<-fmt_ppt_data(cid, 'reverse', ld)%>%mutate(type='far')
  
  df<-rbind(prob, causal, default, reverse)
  df$condition<-cid
  df$type<-factor(df$type, levels=c('probablistic', 'causal', 'near', 'far'))
  df$task<-factor(df$task, levels=rev(get_trials(ld)$task))
  return(df)
}

ggplot(get_cond_data('learn02'), aes(pred, task, fill=prob)) + geom_tile() + 
  #scale_fill_viridis(option="E", direction=-1, end=0.7) +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(~type)

# Overall plots
df<-get_cond_data('learn01')
for (i in 2:6) {
  df<-rbind(df, get_cond_data(paste0('learn0', i)))
}








