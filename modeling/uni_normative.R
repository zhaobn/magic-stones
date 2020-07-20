
# Load library and functions
source("./functions.R")
load("../behavioral_data/tasks.Rdata")
load("../behavioral_data/aggregated.Rdata")

#######################################################################
# Get normative predictions ####
get_cond_pred<-function(lid, par=3, temp=1) {
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

dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 3))

df<-get_cond_pred(1, 3, 0)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, dh, 3, 0))

ggplot(df, aes(x=pred, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(~learningTaskId)

ppt_data<-df.sels%>%filter(sequence=='combined')%>%select(learningTaskId, trial, pred=selection, n)
likeli<-function(par, data) {
  hp<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
  hp$prior<-normalize(mapply(get_hypo_prior, hp$hypo, par[1]))
  
  df<-get_cond_pred(1, hp, par[1], par[2])
  for (i in 2:6) df<-rbind(df, get_cond_pred(i, hp, par[1], par[2]))
  
  df<-df%>%mutate(learningTaskId=as.character(learningTaskId), pred=as.character(pred))
  df<-df%>%left_join(data, by=c('learningTaskId', 'trial', 'pred'))
  
  #return(df)
  return(-sum(log(df$prob)*df$n))
}
#x<-likeli(c(3,0), ppt_data); x

# Fit feature_size only
fit_fsize<-function(x, data) return(likeli(c(x,0),data))
out_fsize=optim(par=3, fn=fit_fsize, data=ppt_data, method='Brent', lower=3, upper=50)

# Fit softmax only
fit_temp<-function(x, data) return(likeli(c(3,x),data))
out_temp=optim(par=1, fn=fit_temp, data=ppt_data, method='Brent', lower=1, upper=100)

# Fit both
out_both=optim(par=c(3.8, 25), fn=likeli, data=ppt_data)

# Test posterior dist. with fitted parameters
dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 3))
dh$orig_post<-normalize(dh$prior*mapply(data_given_hypo, flatten(ld), dh$hypo, 3))

dh$feat_prior<-normalize(mapply(get_hypo_prior, dh$hypo, 3.8))
dh$feat_post<-normalize(dh$feat_prior*mapply(data_given_hypo, flatten(ld), dh$hypo, 3.8))

dh$soft_post<-softmax(dh$orig_post, 25.7)

dh$comb_prior<-normalize(mapply(get_hypo_prior, dh$hypo, 2.5))
dh$comb_post<-normalize(dh$comb_prior*mapply(data_given_hypo, flatten(ld), dh$hypo, 2.5))
dh$comb_post<-softmax(dh$comb_post, 35)

# Save normative sims with fitted parameter ####
# Feature_size only ####
dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 3.83))

df<-get_cond_pred(1, dh, 3.83, 0)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, dh, 3.83, 0))
df.norm.fsize<-df

# Softmax_only ####
dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 3))

df<-get_cond_pred(1, dh, 3, 25.7)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, dh, 3, 25.7))
df.norm.softmax<-df

# Both ####
dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 2.5))

df<-get_cond_pred(1, dh, 2.5, 35)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, dh, 3, 35))

df.norm.both<-df
save(df.norm.fsize, df.norm.both, df.norm.softmax, file='normative.Rdata')

# Check plots ####
a<-df.norm.fsize
b<-df.norm.softmax
c<-df.norm.both

a$type<-'fsize'
b$type<-'softmax'
c$type<-'both'
df<-rbind(a,b,c)

ggplot(df, aes(x=pred, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(type~learningTaskId)

df.norm<-df%>%mutate(learningTaskId=as.character(learningTaskId),
                     pred=as.character(pred))
save(df.norm, file='normative.Rdata')

#######################################################################

# Get likelihood per type ####
get_likeli<-function(model, lid, ppt=ppt_data){
  mdata<-df.norm%>%filter(learningTaskId==paste0('learn0',lid)&type==model)
  ppt<-ppt%>%filter(learningTaskId==paste0('learn0',lid))
  df<-mdata%>%left_join(ppt, by=c('learningTaskId', 'trial', 'pred'))
  likeli<-sum(log(df$prob)*df$n)
  return(data.frame(model=model, learningTaskId=paste0('learn0',lid), likelihood=likeli))
}
#get_likeli('fsize',1)
df<-data.frame(model=character(0), learningTaskId=character(0), likelihood=numeric(0))
for (m in c('fsize', 'softmax', 'both')) {
  for (i in 1:6) {
    df<-rbind(df,get_likeli(m, i))
  }
}
ggplot(df, aes(x=learningTaskId, y=-likelihood, fill=model)) +
  geom_bar(position = 'dodge', stat = 'identity')

# Fit per condition ####
ppt_near<-df.sels%>%filter(sequence=='default')%>%select(learningTaskId, trial, pred=selection, n)
ppt_far<-df.sels%>%filter(sequence=='reverse')%>%select(learningTaskId, trial, pred=selection, n)

near_fsize=optim(par=3, fn=fit_fsize, data=ppt_near, method='Brent', lower=3, upper=50)
far_fsize=optim(par=3, fn=fit_fsize, data=ppt_far, method='Brent', lower=3, upper=50)

near_temp=optim(par=1, fn=fit_temp, data=ppt_near, method='Brent', lower=1, upper=100)
far_temp=optim(par=1, fn=fit_temp, data=ppt_far, method='Brent', lower=1, upper=100)

near_both=optim(par=c(3, 25), fn=likeli, data=ppt_near)
far_both=optim(par=c(3, 25), fn=likeli, data=ppt_far)
#######################################################################

# have a look at L5
ld<-as.list(df.learn_tasks[5,c(2:4)])

dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 3.83))

dh$prior_2<-normalize(mapply(get_hypo_prior, dh$hypo, 3))
dh$post_2<-normalize(dh$prior_2*mapply(data_given_hypo, flatten(ld), dh$hypo, 3))
dh$post_2<-softmax(dh$post_2, 25.7)
# alright it gets softened












