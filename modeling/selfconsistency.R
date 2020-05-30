
source('funcs/preempt.R')

# % match by minimal
## 1. get minimal preds
minimal_hypo<-function(data) {
  if (typeof(data)=='character') data<-to_list(data)
  min_hypo_per_feature<-function(feature, data) {
    hypos<-c()
    f<-substr(feature, 1, 1)
    af<-read_f(feature, data[['agent']])
    rf<-read_f(feature, data[['recipient']])
    tf<-read_f(feature, data[['result']])
    if (tf==rf) hypos<-c(hypos, paste0(f, '(T)', '=', f, '(R)')) else
      if (tf==af) hypos<-c(hypos, paste0(f, '(T)', '=', f, '(A)')) else
        hypos<-c(hypos, paste0(f, '(T)', '~', f, '(R)'),
                        paste0(f, '(T)', '~', f, '(A)')) 
    return(hypos)
  }
  
  hypo_comp<-list()
  for (n in names(features)) hypo_comp[[n]]<-min_hypo_per_feature(n, data)
  
  min_hypos<-c()
  for (f in hypo_comp[[1]]) {
    for (g in hypo_comp[[2]]) {
      min_hypos<-c(min_hypos, paste0(f, ',', g))
    }
  }
  return(min_hypos)
}
get_minimal_preds<-function(cid, learn_src=df.learn_tasks) {
  data<-learn_src[cid, c(2:4)]
  hypos<-minimal_hypo(data)
  trials<-get_trials(data)
  
  get_pred_per_task<-function(task) {
    df<-get_pred_per_hypo(task, hypos[1], F)
    if (length(hypos)>1) {
      for (i in 2:length(hypos)) {
        coln<-paste0('h',i)
        dx<-get_pred_per_hypo(task, hypos[i], F)%>%select(obj, !!coln:=pp)
        df<-df%>%left_join(dx,by='obj')
      }
    }
    df<-df%>%mutate(selected=rowSums(.[2:ncol(df)]))%>%
      mutate(yes=if_else(selected>0, 1, 0), task=task)%>%
      select(obj, yes, task)
    return(df)
  }
  
  df<-get_pred_per_task(trials$task[1])
  for (i in 2:nrow(trials)) df<-rbind(df, get_pred_per_task(trials$task[i]))
  
  df<-df%>%mutate(condition=paste0('L',cid), trial=rep(seq(15),each=9))%>%
    select(condition, trial, pred=obj, yes)
  
  return(df)
}

## 2. % match
ppt_match_minimal<-function(seq, cid, learn_src=df.learn_tasks, ppt_src=df.sels) {
  seqname<-if (seq=='near') 'default' else 'reverse'
  condname<-paste0('learn0', cid)
  ppt<-ppt_src%>%filter(sequence==seqname&learningTaskId==condname)%>%
    select(trial, pred=selection, n)
  minpred<-get_minimal_preds(cid, learn_src)%>%select(trial, pred, yes)%>%
    left_join(ppt,by=c('trial','pred'))%>%mutate(is_in=yes*n)
  minpred<-minpred%>%group_by(trial)%>%summarise(total=sum(n), match=sum(is_in))
  minpred$perc<-minpred$match/minpred$total
  minpred<-minpred%>%mutate(condition=paste0('L',cid), sequence=seq)%>%
    select(condition, sequence, trial, total, match, perc)
}
df.match<-ppt_match_minimal('near', 1)
for (i in 1:6) {
  for (s in c('near', 'far')) {
    if (!(i==1&s=='near')) df.match<-rbind(df.match, ppt_match_minimal(s, i))
  }
}

ggplot(df.match, aes(x=trial, y=perc, group=sequence, color=sequence)) + 
  geom_line(size=.8) +
  labs(y='% matched by minimal') +
  theme_light() + facet_wrap(~condition) +
  geom_smooth(method = "lm", se = FALSE, linetype="dashed", size=.5)

## Result: not really

# Self-consistency update






