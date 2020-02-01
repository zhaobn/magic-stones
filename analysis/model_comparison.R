
options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

# Base dataframe
d1<-df.tasks%>%select(learningTaskId, trial)%>%mutate(sequence='default')
d2<-df.tasks%>%select(learningTaskId, trial)%>%mutate(sequence='reverse')
df.mod<-rbind(d1, d2)

save(df.sw, df.tw, df.tasks, df.sels, df.mod, df.rq_plot, file='cogsci_20200127.Rdata')

# Match-count
match_by<-function(lid, tid, seq, model, data=df.sels) {
  dist<-paste0(model, '_pp')
  data<-data%>%filter(learningTaskId==lid&trial==tid&sequence==seq)
  ppt_tops<-data%>%filter(freq==max(freq)); ppt_tops<-ppt_tops$selection
  pred_tops<-data%>%filter(!!as.name(dist)==max(!!as.name(dist))); 
  if (length(pred_tops$selection) > 5) {
    return(as.numeric(0))
  } else {
    pred_tops<-pred_tops$selection
    return(as.numeric(sum(ppt_tops %in% pred_tops)>0))
  }
  
}
match_by('learn01', 1, 'default', 'nm')

df.md<-df.mod%>%select(learningTaskId, sequence, trial)
totals<-df.tw%>%count(learningTaskId, order, trial)%>%select(learningTaskId, sequence=order, trial, n)
df.md<-df.md%>%left_join(totals, by=c('learningTaskId', 'sequence', 'trial'))

for (md in c('nm', 'cf', 'cw', 'ft')) {
  df.md[, paste0(md, '_match')]<-
    mapply(match_by, df.md[, 'learningTaskId'], df.md[, 'trial'], 
           df.md[, 'sequence'], rep(md, length(df.md[,1])))
}
sum(df.md$nm_match)/length(df.mod[,1])
sum(df.md$cf_match)/length(df.mod[,1])
sum(df.md$cw_match)/length(df.mod[,1])
sum(df.md$ft_match)/length(df.mod[,1])

# Likelihood
test<-df.sels
for (md in c('nm', 'cf', 'cw', 'ft')) {
  test[,paste0(md, '_ll')]<-log(test[,paste0(md, '_pp')])*test$n
}
test[,'rand_ll']<-log(1/9)*test$n
x<-test%>%group_by(learningTaskId, sequence, trial)%>%
  summarise(rand_ll=sum(rand_ll), nm_ll=sum(nm_ll), cf_ll=sum(cf_ll), cw_ll=sum(cw_ll), ft_ll=sum(ft_ll))
df.md<-df.md%>%left_join(x, by=c('learningTaskId', 'sequence', 'trial'))

sum(df.md$rand_ll)
sum(df.md$nm_ll)
sum(df.md$cf_ll)
sum(df.md$cw_ll)
sum(df.md$ft_ll)

# BIC
total<-length(df.tw[,1])
sum(df.md$rand_ll) * (-2) + 0 * log(total)
sum(df.md$nm_ll) * (-2) + 0 * log(total)
sum(df.md$cf_ll) * (-2) + 0 * log(total)
sum(df.md$cw_ll) * (-2) + 4 * log(total)
sum(df.md$ft_ll) * (-2) + 4 * log(total)


# Regressions
nm_rsq<-function(lid, tid, seq, data=df.sels) {
  dt<-data%>%filter(learningTaskId==lid&sequence==seq&trial==tid)
  return(summary(lm(data=dt, freq~nm_pp))$adj.r.squared)
}
cf_rsq<-function(lid, tid, seq, data=df.sels) {
  dt<-data%>%filter(learningTaskId==lid&sequence==seq&trial==tid)
  return(summary(lm(data=dt, freq~cf_pp))$adj.r.squared)
}
cw_rsq<-function(lid, tid, seq, data=df.sels) {
  dt<-data%>%filter(learningTaskId==lid&sequence==seq&trial==tid)
  return(summary(lm(data=dt, freq~cw_pp))$adj.r.squared)
}
ft_rsq<-function(lid, tid, seq, data=df.sels) {
  dt<-data%>%filter(learningTaskId==lid&sequence==seq&trial==tid)
  return(summary(lm(data=dt, freq~ft_pp))$adj.r.squared)
}

df.md$nm_arsq<-mapply(nm_rsq, df.md$learningTaskId, df.md$trial, df.md$sequence)
df.md$cf_arsq<-mapply(cf_rsq, df.md$learningTaskId, df.md$trial, df.md$sequence)
df.md$cw_arsq<-mapply(cw_rsq, df.md$learningTaskId, df.md$trial, df.md$sequence)
df.md$ft_arsq<-mapply(ft_rsq, df.md$learningTaskId, df.md$trial, df.md$sequence)

summary(lm(data=df.sels, freq~cw_pp))$adj.r.squared
summary(lm(data=df.sels, freq~ft_pp))$adj.r.squared

save(df.sw, df.tw, df.tasks, df.sels, df.md, file='cogsci_20200201.Rdata')


