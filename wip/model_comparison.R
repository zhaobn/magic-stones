
options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

save(df.nm, df.nm.inf, df.alpha, df.order, df.tasks, df.sels, df.mods, file='models.Rdata')

# Combine model results
df.sels<-df.sels%>%
  left_join(df.order, by=c('learningTaskId', 'sequence', 'trial', 'selection'))%>%
  rename(od_pp=prob)

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
match_by('learn02', 1, 'default', 'od')

df.mods_backup<-df.mods

df.mods<-df.mods%>%select(learningTaskId, sequence, trial)
totals<-df.trials%>%count(learningTaskId, order, trial)%>%select(learningTaskId, sequence=order, trial, n)
df.mods<-df.mods%>%left_join(totals, by=c('learningTaskId', 'sequence', 'trial'))
for (md in c('nm', 'cs', 'od')) {
  df.mods[, paste0(md, '_match')]<-
    mapply(match_by, df.mods[, 'learningTaskId'], df.mods[, 'trial'], 
           df.mods[, 'sequence'], rep(md, length(df.mods[,1])))
}
nr<-length(df.mods[,1])
sum(df.mods$nm_match)/nr
sum(df.mods$cs_match)/nr
sum(df.mods$od_match)/nr


# Likelihood
test<-df.sels
for (md in c('rd', 'nm', 'cs', 'od')) {
  test[,paste0(md, '_ll')]<-log(test[,paste0(md, '_pp')])*test$n
}
x<-test%>%group_by(learningTaskId, sequence, trial)%>%
  summarise(rd_ll=sum(rd_ll), nm_ll=sum(nm_ll), cs_ll=sum(cs_ll), od_ll=sum(od_ll))
df.mods<-df.mods%>%left_join(x, by=c('learningTaskId', 'sequence', 'trial'))

sum(df.mods$rd_ll)
sum(df.mods$nm_ll)
sum(df.mods$cs_ll)
sum(df.mods$od_ll)

# BIC
total<-length(df.trials[,1])
sum(df.mods$rd_ll) * (-2) + 0 * log(total)
sum(df.mods$nm_ll) * (-2) + 0 * log(total)
sum(df.mods$cs_ll) * (-2) + 2 * log(total)
sum(df.mods$od_ll) * (-2) + 2 * log(total)


# Regressions
rd_rsq<-function(lid, tid, seq, data=df.sels) {
  dt<-data%>%filter(learningTaskId==lid&sequence==seq&trial==tid)
  return(summary(lm(data=dt, freq~rd_pp))$adj.r.squared)
}
nm_rsq<-function(lid, tid, seq, data=df.sels) {
  dt<-data%>%filter(learningTaskId==lid&sequence==seq&trial==tid)
  return(summary(lm(data=dt, freq~nm_pp))$adj.r.squared)
}
cs_rsq<-function(lid, tid, seq, data=df.sels) {
  dt<-data%>%filter(learningTaskId==lid&sequence==seq&trial==tid)
  return(summary(lm(data=dt, freq~cs_pp))$adj.r.squared)
}
od_rsq<-function(lid, tid, seq, data=df.sels) {
  dt<-data%>%filter(learningTaskId==lid&sequence==seq&trial==tid)
  return(summary(lm(data=dt, freq~od_pp))$adj.r.squared)
}

df.mods$rd_arsq<-mapply(rd_rsq, df.mods$learningTaskId, df.mods$trial, df.mods$sequence)
df.mods$nm_arsq<-mapply(nm_rsq, df.mods$learningTaskId, df.mods$trial, df.mods$sequence)
df.mods$cs_arsq<-mapply(cs_rsq, df.mods$learningTaskId, df.mods$trial, df.mods$sequence)
df.mods$od_arsq<-mapply(od_rsq, df.mods$learningTaskId, df.mods$trial, df.mods$sequence)

summary(lm(data=df.sels, freq~nm_pp))$adj.r.squared
summary(lm(data=df.sels, freq~cs_pp))$adj.r.squared
summary(lm(data=df.sels, freq~od_pp))$adj.r.squared

length(df.mods$cs_arsq[df.mods$cs_arsq>0.9])/length(df.mods$cs_arsq)
length(df.mods$cs_arsq[df.mods$cs_arsq>0.8])/length(df.mods$cs_arsq)


