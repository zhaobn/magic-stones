
# Note: reuse functions from simulations.R
# Task configs
features<-list()
features[['color']]<-c('b', 'r', 'y') # blue, red, yellow
features[['shape']]<-c('c', 'd', 's') # circle, diamond, square

obj_sep=''
all_objs<-get_all_objs(features)

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
get_cond_data<-function(cid, alpha=1, temperature=3, noise=T, src=df.learn_tasks) {
  ld<-as.list(src%>%filter(learningTaskId==cid)%>%select(agent, recipient, result))
  
  prob<-get_cat_preds(ld, alpha, noise)%>%mutate(type='probablistic')
  causal<-get_causal_preds(ld, temperature, noise)%>%mutate(type='causal')
  default<-fmt_ppt_data(cid, 'default', ld)%>%mutate(type='near')
  reverse<-fmt_ppt_data(cid, 'reverse', ld)%>%mutate(type='far')
  
  df<-rbind(prob, causal, default, reverse)
  df$condition<-paste0('L', substr(cid, nchar(cid), nchar(cid)))
  df$trial<-rep(rep(seq(15), each=9),4)
  
  df$type<-factor(df$type, levels=c('probablistic', 'causal', 'near', 'far'))
  df$task<-factor(df$task, levels=rev(get_trials(ld)$task))
  df$trial<-factor(df$trial, levels=rev(seq(15)))
  return(df)
}

ggplot(get_cond_data('learn01'), aes(pred, trial, fill=prob)) + geom_tile() + 
  #scale_fill_viridis(option="E", direction=-1, end=0.7) +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(~type)

# Overall plots
df.plot<-get_cond_data('learn01')
for (i in 2:6) df.plot<-rbind(df.plot, get_cond_data(paste0('learn0', i)))

df<-df.plot%>%filter(!(type=='probablistic'))
ggplot(df.plot, aes(pred, trial, fill=prob)) + geom_tile() + 
  scale_fill_viridis(option="E", direction=-1) +
  #scale_fill_gradient(low="white", high="black") +
  facet_grid(type~condition)

## Add normative preds
norm<-data.frame(task=character(0),pred=character(0),prob=numeric(0))
for (i in 1:6) {
  data<-as.list(df.learn_tasks%>%filter(learningTaskId==paste0('learn0',i))%>%select(agent, recipient, result))
  norm<-rbind(norm, get_norm_preds(data, 6, T))
}
norm$type<-'normative'
norm$condition<-rep(paste0('L', rep(seq(6))),each=9*15)
norm$trial<-rep(rep(seq(15),each=9),6)
norm<-norm%>%select(names(df.plot))
df.plot<-rbind(df.plot, norm)


# Uniformaty
## Use very strong causal predictions
df.strict<-get_cond_data('learn01', 1, 6, F)
for (i in 2:6) df.strict<-rbind(df.strict, get_cond_data(paste0('learn0', i), 1, 6, F))

get_hty<-function(cid, tid, mod, src=df.strict) {
  cond<-paste0('L', substr(cid, nchar(cid), nchar(cid)))
  probs<-(src%>%filter(condition==cond&trial==tid&type==mod)%>%select(prob))[[1]]
  hty<-var(probs)/(1/length(probs))
  return(data.frame(condition=cond, trial=tid, type=mod, hm=hty))
}
df.hm<-get_hty('learn01', 1, 'probablistic')
for (i in 1:6) {
  for (t in 1:15) {
    for (m in c('probablistic', 'causal', 'near', 'far')) {
      if (!(i==1&t==1&m=='probablistic')) {
        df.hm<-rbind(df.hm, get_hty(paste0('learn0', i), t, m))
      }
    }
  }
}
save(df.plot, df.strict, df.hm, file='plot.Rdata')

df.hm$condition<-factor(df.hm$condition, levels=c('L1', 'L3', 'L5', 'L2', 'L4', 'L6'))
df.hm$trial<-factor(df.hm$trial, levels=seq(15))
ggplot(df.hm, aes(x=trial, y=hm, group=type, color=type)) + 
  geom_line() + facet_wrap(~condition) +
  labs(x='trial', y='homogeneity') +
  theme_light() +
  scale_color_manual(values = c("#FFDB6D", "#D16103", "#4E84C4", "#C3D7A4"))

df<-df.hm%>%filter(type%in%c('near', 'far'))
ggplot(df, aes(x=type, y=hm, fill=type)) +
  geom_boxplot() + facet_wrap(~condition) 
  
# % match
get_match_rate<-function(cid, seq, ppt_src=df.sels, prob_src=df.strict) {
  df<-ppt_src%>%filter(learningTaskId==cid&sequence==seq)%>%select(trial, selection, n)
  cond<-paste0('L', substr(cid, nchar(cid), nchar(cid)))
  
  prob<-prob_src%>%filter(condition==cond&type=='probablistic')%>%select(trial, selection=pred, prob)
  prob$trial<-as.numeric(as.character(prob$trial))
  prob$selection<-as.character(prob$selection)
  df<-df%>%left_join(prob, by=c('trial', 'selection'))
  df$np<-df$n*df$prob
  pmatch<-sum(df$np)/sum(df$n)
  
  causal<-prob_src%>%filter(condition==cond&type=='causal')%>%select(trial, selection=pred, causal=prob)
  causal$trial<-as.numeric(as.character(causal$trial))
  causal$selection<-as.character(causal$selection)
  df<-df%>%left_join(causal, by=c('trial', 'selection'))
  df$nc<-df$n*df$causal
  cmatch<-sum(df$nc)/sum(df$n)
  
  df$mix<-mapply(max, df$prob, df$causal)
  df$nm<-df$n*df$mix
  mmatch<-sum(df$nm)/sum(df$n)
  
  seqname<-if (seq=='default') 'near' else 'far'
  return(data.frame(condition=rep(cond, 3), sequence=rep(seqname, 3), 
             type=c('probablistic', 'causal', 'mix'),
             value=c(pmatch, cmatch, mmatch)))
}
df.match<-get_match_rate('learn01', 'default')
for (i in 1:6) {
  for (s in c('default', 'reverse')) {
    if (!(i==1&s=='default')) df.match<-rbind(df.match, get_match_rate(paste0('learn0', i), s))
  }
}
df.match$type<-factor(df.match$type, levels=c('probablistic', 'causal', 'mix'))
df.match$condition<-factor(df.match$condition, levels=c('L1', 'L3', 'L5', 'L2', 'L4', 'L6'))

ggplot(df.match, aes(fill=type, y=value, x=sequence)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, direction = -1) +
  labs(y='% match') +
  geom_text(aes(label=round(value,2)), position=position_dodge(0.9), vjust=-.5) +
  facet_wrap(~condition)

get_mix_match_per_trial<-function(cid, seq, tid, ppt_src=df.sels, prob_src=df.strict) {
  cond<-paste0('L', substr(cid, nchar(cid), nchar(cid)))
  df<-ppt_src%>%filter(learningTaskId==cid&sequence==seq&trial==tid)%>%select(trial, selection, n)
  
  prob<-prob_src%>%filter(condition==cond&type=='probablistic'&trial==tid)%>%select(trial, selection=pred, prob)
  prob$trial<-as.numeric(as.character(prob$trial))
  prob$selection<-as.character(prob$selection)
  df<-df%>%left_join(prob, by=c('trial', 'selection'))
  
  causal<-prob_src%>%filter(condition==cond&type=='causal'&trial==tid)%>%select(trial, selection=pred, causal=prob)
  causal$trial<-as.numeric(as.character(causal$trial))
  causal$selection<-as.character(causal$selection)
  df<-df%>%left_join(causal, by=c('trial', 'selection'))

  df$mix<-mapply(max, df$prob, df$causal)
  df$nm<-df$n*df$mix
  
  seqname<-if (seq=='default') 'near' else 'far'
  return(data.frame(condition=cond, sequence=seqname, trial=tid, match=sum(df$nm)/sum(df$n)))
}

df.mix<-get_mix_match_per_trial('learn01', 'default', 1)  
for (i in 1:6) {
  for (t in 1:15) {
    for (s in c('default', 'reverse')) {
      if (!(i==1&t==1&s=='default')) df.mix<-rbind(df.mix, get_mix_match_per_trial(paste0('learn0',i), s, t)  )
    }
  }
}  
df.mix$trial<-factor(df.mix$trial, levels=seq(15))
df.mix$condition<-factor(df.mix$condition, levels=c('L1', 'L3', 'L5', 'L2', 'L4', 'L6'))
save(df.plot, df.strict, df.hm, df.match, df.mix, file='plot.Rdata')  

ggplot(df.mix, aes(x=trial, y=match, group=sequence, color=sequence)) + 
  geom_line(size=.8) +
  labs(y='% matched by prob+causal') +
  theme_light() + facet_wrap(~condition) +
  geom_smooth(method = "lm", se = FALSE, linetype="dashed", size=.5)

# Take a look at causal match
get_causal_match_per_trial<-function(cid, seq, tid, ppt_src=df.sels, prob_src=df.strict) {
  cond<-paste0('L', substr(cid, nchar(cid), nchar(cid)))
  df<-ppt_src%>%filter(learningTaskId==cid&sequence==seq&trial==tid)%>%select(trial, selection, n)
  
  causal<-prob_src%>%filter(condition==cond&type=='causal'&trial==tid)%>%select(trial, selection=pred, causal=prob)
  causal$trial<-as.numeric(as.character(causal$trial))
  causal$selection<-as.character(causal$selection)
  df<-df%>%left_join(causal, by=c('trial', 'selection'))
  df$nc<-df$n*df$causal
  
  seqname<-if (seq=='default') 'near' else 'far'
  return(data.frame(condition=cond, sequence=seqname, trial=tid, causal=sum(df$nc)/sum(df$n)))
}
df.cm<-get_causal_match_per_trial('learn01', 'default', 1)
for (i in 1:6) {
  for (t in 1:15) {
    for (s in c('default', 'reverse')) {
      if (!(i==1&t==1&s=='default')) df.cm<-rbind(df.cm, get_causal_match_per_trial(paste0('learn0',i), s, t))
    }
  }
}  
df.cm$trial<-factor(df.cm$trial, levels=seq(15))
df.cm$condition<-factor(df.cm$condition, levels=c('L1', 'L3', 'L5', 'L2', 'L4', 'L6'))
ggplot(df.cm, aes(x=trial, y=causal, group=sequence, color=sequence)) + 
  geom_line() +
  labs(y='% matched by causal') +
  theme_light() + facet_wrap(~condition) +
  geom_smooth(method = "lm", se = FALSE, linetype="dashed", size=.5)


df.plot$type<-factor(df.plot$type, levels=c('probablistic', 'normative', 'causal', 'near', 'far'))
save(df.plot, df.strict, df.hm, df.match, df.mix, file='plot.Rdata')







  
  
  
