
# Aim: an automated script that loops 9 theories on the 6 conditions
t<-theories[1]
# Compare with normative results
agree_with_norm<-function(result, cond, tid) {
  sel_norm<-norm %>% filter(learningTaskId==cond&trial==tid)
  sel_norm<-as.character(sel_norm$norm)
  
  sel_pred<-result %>% filter(condition==cond&trial==tid) %>% arrange(desc(prob)) %>% slice(1)
  sel_pred<-as.character(sel_pred$selection)
  # Deal with uncertain normative results
  if (length(sel_norm)>1) {
    return(sel_pred %in% sel_norm)
  } else {
    return(sel_norm==sel_pred)
  }
}
check_for_cond<-function(pred, cond) {
  checks<-rep(0, len=15)
  for (i in 1:15) {
    checks[i]<-agree_with_norm(pred, cond, i)
  }
  return(sum(checks)/15)
}
norm_agree<-function(theory) {
  sims<-get_theory_sim(theory)
  norm_agrees<-rep(0,6)
  for (i in 1:6) {
    norm_agrees[i]<-check_for_cond(sims, paste0('learn0', i))
  }
  dt<-as.data.frame(norm_agrees)
  names(dt)<-theory
  return(dt)
}

conditions<-rep('', 6)
for (i in 1:6) {
  conditions[i]<-paste0('learn0', i)
}
norm_comp<-as.data.frame(conditions)
for (i in 1:length(theories)) {
  norm_comp<-cbind(norm_comp, norm_agree(theories[i]))
}

# Plot heatmap
library(ggplot2)
norm_agree_for_plot<-function(theory) {
  sims<-get_theory_sim(theory)
  norm_agrees<-rep(0,6)
  for (i in 1:6) {
    norm_agrees[i]<-check_for_cond(sims, paste0('learn0', i))
  }
  dt<-as.data.frame(norm_agrees)
  dt$condition<-conditions
  dt$theory<-rep(theory,6)
  dt<-dt%>%select(theory, condition, agrees=norm_agrees)
  return(dt)
}
norm_comp_p<-norm_agree_for_plot(theories[1])
for (i in 2:length(theories)) {
  norm_comp_p<-rbind(norm_comp_p, norm_agree_for_plot(theories[i]))
}

ggplot(norm_comp_p, aes(x=theory, y=reorder(condition, desc(condition)), fill = agrees)) + 
  geom_raster() +
  geom_text(aes(label = round(agrees, 2)), data=subset(norm_comp_p, agrees>0.001)) +
  scale_fill_gradient(low = "gray90", high = "steelblue4") + 
  labs(x='', y='')

# Compare with participant data
pdata<-df.tw %>% select(ix, learningTaskId, trial, selection)
ppt_agree<-function(data, cond, tid) {
  sel_ppt<-pdata %>% filter(learningTaskId==cond&trial==tid) %>% 
    group_by(selection) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
  sel_ppt$selection<-as.character(sel_ppt$selection)
  sel_pred<-data %>% filter(condition==cond&trial==tid) %>% select(selection, prob)
  sel_pred$selection<-as.character(sel_pred$selection)
  sels<-sel_pred%>%left_join(sel_ppt, by="selection")%>%select(selection, prob, freq)%>%
    mutate(freq=ifelse(is.na(freq), 0, freq))%>%mutate(diff=prob-freq)
  return(sum(abs(sels$diff))/15)
}

ppt_agree_for_plot <- function(th) {
  data<-get_theory_sim(th)
  cond<-'learn01'
  theory<-rep(th, 6)
  checks<-rep(0,6)
  for (i in 1:6) {
    checks[i]<-ppt_agree(data, cond, i)
  }
  return(data.frame(theory, conditions, checks))
}
ppt_comp_p<-ppt_agree_for_plot(theories[1])
for (i in 2:length(theories)) {
  ppt_comp_p<-rbind(ppt_comp_p, ppt_agree_for_plot(theories[i]))
}

ggplot(ppt_comp_p, aes(x=theory, y=reorder(conditions, desc(conditions)), fill = checks)) + 
  geom_raster() +
  geom_text(aes(label = round(checks, 2))) +
  scale_fill_gradient(low = "gray90", high = "lightsteelblue3") + 
  labs(x='', y='')






