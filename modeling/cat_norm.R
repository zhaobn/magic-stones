
# Load library and functions
source("./functions.R")
load("../behavioral_data/tasks.Rdata")
load("../behavioral_data/aggregated.Rdata")

#######################################################################
# get predictions per condition
get_crp_norm_cond_preds<-function(lid, seq, mu=0.1, alpha=0.1, beta=10, temp=0) {
  # prep data
  ld<-as.list(df.learn_tasks[lid,c(2:4)]) # read learning data point
  dh<-prep_hypos(ld, beta) # hypotheses prior and posterior
  cat<-init_cat(mu)+count_feats(ld, 'A') # learning's category
  cats<-list(); cats[[1]]<-cat
  tasks<-tasks_from_df(lid) # all tasks for this learning condition
  
  # calculate stone posterior per hypo
  calc_stone_post<-function(td, dh, type='post', beta) {
    prediction<-list(); for(obj in all_objs) prediction[[obj]]<-0
    for (i in 1:nrow(dh)) {
      hp<-dh$hypo[i]
      posterior<-if (type=='post') dh$posterior[i] else dh$prior[i]
      predicted<-get_hypo_preds(td, hp)
      for (pt in predicted) {
        size<-if (length(predicted)>1) beta-1 else 1
        prediction[[pt]]<-prediction[[pt]]+posterior/size
      }
    }
    p<-data.frame(matrix(unlist(prediction), nrow=length(prediction), byrow=T))
    colnames(p)<-c('prob')
    return(cbind(data.frame(pred=all_objs), p))
  }
  
  get_trial_pred<-function(tid, seq, mu, alpha, beta, temp) {
    # Preps
    tid<-if (seq=='near') tid else length(tasks)+1-tid
    td<-read_task(tasks[tid])
    # Decide if task stones belong to the same category as learning stones
    p_cat<-stone_likeli(td, cat, 'A')*cat_prior(cat, cats, mu, alpha, 'A', F)
    p_new<-stone_likeli(td, init_cat(mu), 'A')*cat_prior(init_cat(mu), cats, mu, alpha, 'A', T)
    p_yes<-p_cat/(p_cat+p_new)
    # If so, sample from posterior
    yes<-calc_stone_post(td, dh, 'post', beta)
    # If not, sample from prior
    new<-calc_stone_post(td, dh, 'prior', beta)
    # Combine to get the mixed posterior
    pd<-(yes%>%left_join(new, by='pred'))%>%
      mutate(mixed=p_yes*prob.x+(1-p_yes)*prob.y,
             learningTaskId=paste0('learn0', lid), trial=tid, condition=seq)%>%
      select(learningTaskId, condition, trial, pred, prob=mixed)
    pd$prob<-if (temp==0) normalize(pd$prob) else softmax(pd$prob, temp)
    return(pd)
  }
  
  df<-get_trial_pred(1, seq, mu, alpha, beta, temp)
  for (i in 2:length(tasks)) df<-rbind(df, get_trial_pred(i, seq, mu, alpha, beta, temp))
  df<-df%>%arrange(learningTaskId, condition, trial, pred)
  return(df)
}

# get all predictions
get_crp_norm_pred<-function(mu=0.1, alpha=0.1, beta=10, temp=0) {
  df<-get_crp_norm_cond_preds(1, 'near', mu, alpha, beta, temp)
  for (i in 1:6) {
    for (s in c('near', 'far')) {
      if (!(i==1&seq=='near')) {
        df<-rbind(df, get_crp_norm_cond_preds(i, s, mu, alpha, beta, temp))
      } 
    }
  }
  return(df)
}

# get cro-normative model results
df.crp<-get_crp_norm_pred(0.2, 0.08, 3.8, 0)%>%mutate(pred=as.character(pred))
save(df.crp, file='data/sim.Rdata')

# Calculate likelihoods
ppt_data<-df.sels%>%filter(!(sequence=='combined'))%>%
  mutate(condition=if_else(sequence=='default', 'near', 'far'))%>%
  select(learningTaskId, condition, trial, pred=selection, n, freq)

get_likeli<-function(model, data) {
  m<-model%>%select(learningTaskId, condition, trial, pred, prob)
  p<-data%>%select(learningTaskId, condition, trial, pred, n)
  d<-p%>%left_join(m, by=c('learningTaskId', 'condition', 'trial', 'pred'))
  return(sum(log(d$prob)*d$n))
}

get_likeli(df.sim.10k, ppt_data) # -4955.807
get_likeli(df.sim.analytical, ppt_data) # -4951.343

# Double-check uni-normative results
norm<-rbind(df.norm%>%filter(type=='fsize')%>%mutate(condition='near'),
            df.norm%>%filter(type=='fsize')%>%mutate(condition='far'))%>%
  select(learningTaskId, condition, trial, pred, norm=prob)
d<-d%>%left_join(norm, by=c('learningTaskId', 'condition', 'trial', 'pred'))
sum(log(d$prob)*d$n) #-4951.343
sum(log(d$norm)*d$n) #-2598.681


# Fit parameters
try_params<-function(par, data) {
  preds<-get_crp_norm_pred(par[1], par[2], 3.83, 0)%>%mutate(pred=as.character(pred))
  likeli<-get_likeli(preds, data)
  return(-likeli)
}
#try_params(c(0.1,0.1), ppt)
out=optim(par=c(0.1, 0.1), fn=try_params, data=ppt)
save(out, file=read_file('out_10.Rdata'))





