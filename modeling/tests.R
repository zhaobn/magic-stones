source("functions.R")


plot_cols<-c("learn_cond", "condition", "trial", "selection", "prob", "data")
# Participant data - ppt ####
ppt_near<-df.sels%>%filter(sequence=='default')%>%
  mutate(data='ppt', condition='near', learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%
  select(learn_cond, condition, trial, selection, prob=freq, n, data)
ppt_far<-df.sels%>%filter(sequence=='reverse')%>%
  mutate(data='ppt', condition='far', learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%
  select(learn_cond, condition, trial, selection, prob=freq, n, data)
ppt<-rbind(ppt_near, ppt_far)

#####
# Random baseline - rand ####
rand<-ppt%>%mutate(prob=1/9, data='random')%>%select(plot_cols)
#####
# Universal Model 1 - uni_1 (fsize=3.83) ####
dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 3.83))
df<-get_cond_pred(1, dh, 3.83, 0)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, dh, 3.83, 0))

dnear<-df%>%mutate(data='uni_fsize', condition='near', selection=pred,learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%select(plot_cols)
dfar<-df%>%mutate(data='uni_fsize', condition='far', selection=pred,learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%select(plot_cols)
uni_1<-rbind(dnear, dfar)
#####
# Universal Model 2 - uni_2 (temp=25.7) ####
dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 3))
df<-get_cond_pred(1, dh, 3, 25.7)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, dh, 3, 25.7))

dnear<-df%>%mutate(data='uni_softmx', condition='near', selection=pred,learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%select(plot_cols)
dfar<-df%>%mutate(data='uni_softmx', condition='far', selection=pred,learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%select(plot_cols)
uni_2<-rbind(dnear, dfar)
#####
# Universal Model 3 - uni_3 (fsize=2.5, temp=35) ####
dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 2.5))
df<-get_cond_pred(1, dh, 2.5, 35)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, dh, 2.5, 35))

dnear<-df%>%mutate(data='uni_both', condition='near', selection=pred,learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%select(plot_cols)
dfar<-df%>%mutate(data='uni_both', condition='far', selection=pred,learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%select(plot_cols)
uni_3<-rbind(dnear, dfar)
#####
# Cat Normative
df<-get_crp_norm_pred(192831, 1.78, 3.83, 0)
cat_10a<-df%>%mutate(data='cat_fitted', selection=pred,learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%select(plot_cols)

df<-get_crp_norm_pred(0.1, 0.5, 3.83, 0)
cat_10b<-df%>%mutate(data='cat_neutral', selection=pred,learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%select(plot_cols)

df<-get_crp_norm_pred(0.2, 0.08, 3.83, 0)
cat_10c<-df%>%mutate(data='cat_try', selection=pred,learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%select(plot_cols)

calc_likeli(cat_10b)
calc_rsq(cat_10c)

# Cat greedy
df<-get_sim_df(c(3.83, 0, 0.2, 0.08))
greedy<-df%>%mutate(data='greedy', selection=pred, prob=if_else(prob==0, eps, prob),learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%select(plot_cols)

calc_likeli(greedy)
calc_rsq(greedy)

############################################################
# Likelihoods and BIC
calc_likeli<-function(df) {
  p<-ppt%>%select(learn_cond, condition, trial, selection, n)
  df<-df%>%mutate(selection=as.character(selection))%>%
    left_join(p, by=c("learn_cond", "condition", "trial", "selection"))
  return(sum(log(df$prob)*df$n))
}
# Lm
calc_rsq<-function(df) {
  p<-ppt%>%select(learn_cond, condition, trial, selection, freq=prob)
  df<-df%>%mutate(selection=as.character(selection))%>%
    left_join(p, by=c("learn_cond", "condition", "trial", "selection"))
  return(summary(lm(data=df, freq~prob))$r.squared)
}
# Plots
df<-rbind(ppt%>%select(plot_cols), uni_1, uni_2, uni_3)
df<-rbind(ppt%>%select(plot_cols), cat_10b, cat_10c)
df<-rbind(ppt%>%select(plot_cols), uni_1, cat_10b, greedy)
df<-rbind(uni_1, cat_10c)
df<-rbind(cat_10b, cat_10c)

ggplot(df, aes(x=selection, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(data~learn_cond)

ggplot(cat_10b, aes(x=selection, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(~learn_cond)










