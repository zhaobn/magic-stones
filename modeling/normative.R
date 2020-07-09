
# Load library and functions
source("./functions.R")

#######################################################################
# Get normative predictions ####
get_cond_pred<-function(lid, dh, par=3) {
  get_trial_pred<-function(tid) {
    prediction<-list(); for(obj in all_objs) prediction[[obj]]<-0
    td<-as.list(tasks[tid, c(3:4)])
    for (i in 1:nrow(dh)) {
      hp<-dh$hypo[i]; posterior<-dh$post[i]
      predicted<-get_hypo_preds(td, hp)
      for (pt in predicted) {
        size<-if (length(predicted)>1) par-1 else 1
        prediction[[pt]]<-prediction[[pt]]+posterior/size
      }
    }
    
    p<-data.frame(matrix(unlist(prediction), nrow=length(prediction), byrow=T))
    colnames(p)<-c('prob')
    p$prob<-normalize(p$prob)
    
    return(cbind(data.frame(learningTaskId=paste0('learn0',lid), trial=tid, pred=all_objs), p))
  }
  # Prep data
  ld<-as.list(df.learn_tasks[lid,c(2:4)])
  tasks<-df.gen_trials%>%filter(learningTaskId==paste0('learn0',lid))
  # Calc posterior
  #dh$post<-normalize(dh$prior*mapply(data_given_hypo, flatten(ld), dh$hypo, par))
  dh$post<-normalize(dh$prior*mapply(data_given_hypo, flatten(ld), dh$hypo, par))
  # Make prediction
  df<-get_trial_pred(1)  
  for (i in 2:15) df<-rbind(df, get_trial_pred(i))
  return(df)
}

dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 3))

df<-get_cond_pred(1, dh, 3)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, dh, 3))

df.norm<-df
save(df.norm, file='data/normative.Rdata')

# Plot results ####
ggplot(df, aes(x=pred, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(~learningTaskId)

# Fit parameters ####
ppt_data<-df.sels%>%filter(sequence=='combined')%>%select(learningTaskId, trial, pred=selection, n)

fit_me<-function(par, data) {
  hp<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
  hp$prior<-normalize(mapply(get_hypo_prior, hp$hypo, par))
  
  df<-get_cond_pred(1, hp, par)
  for (i in 2:6) df<-rbind(df, get_cond_pred(i, hp, par))
  
  df<-df%>%mutate(learningTaskId=as.character(learningTaskId), pred=as.character(pred))
  df<-df%>%left_join(data, by=c('learningTaskId', 'trial', 'pred'))
  
  #return(df)
  return(-sum(log(df$prob)*df$n))
}
#x<-fit_me(3, ppt_data); x

out=optim(par=10, fn=fit_me, data=ppt_data, method='Brent', lower=1, upper=50)
# out$par = 3.828173
# out$value = 2598.681

# Save normative sims with fitted parameter ####
dh<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
dh$prior<-normalize(mapply(get_hypo_prior, dh$hypo, 3.8))

df<-get_cond_pred(1, dh, 3.8)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, dh, 3.8))

df.norm<-df
save(df.norm, file='data/normative.Rdata')

#######################################################################

# % match ####
task_match<-function(lid, tid) {
  ppt<-df.sels%>%
    filter(sequence=='combined'&learningTaskId==paste0('learn0',lid)&trial==tid)
  ppt_most<-(ppt%>%filter(n==max(n)))$selection
          
  mod<-df.norm%>%mutate(pred=as.character(pred))%>%
    filter(learningTaskId==paste0('learn0',lid)&trial==tid)%>%arrange(-prob)
  mod_most<-(mod%>%filter(prob==max(prob)))$pred
  
  predicted<-FALSE
  for (p in ppt_most) {
    predicted<-predicted|(p %in% mod_most)
  }
  return(predicted)
}
matched<-0
for (i in 1:6) {
  for (j in 1:15) {
    matched<-matched+task_match(i,j)
  }
}
66/90

df<-data.frame(learningTaskId=paste0('learn0',1),trial=1,matched=1)
for (i in 1:6) {
  for (j in 1:15) {
    if (!(i==1&j==1)) df<-rbind(df,
                                data.frame(learningTaskId=paste0('learn0',i),trial=j,
                                           matched=task_match(i,j)))
  }
}


#######################################################################
















