
# Load library and functions
source("./functions.R")
load("../behavioral_data/tasks.Rdata")
load("../behavioral_data/aggregated.Rdata")

#######################################################################
beta=3.83

# 1 get prior and posterior for all trials
prs<-prep_hypos(as.list(df.learn_tasks[1,c(2:4)]), beta)
prs<-prs%>%rename(prior_1=prior, posterior_1=posterior)
for (i in 2:6) {
  x<-prep_hypos(as.list(df.learn_tasks[i,c(2:4)]), beta)
  col_1<-paste0('prior_',i); col_2<-paste0('posterior_',i)
  x<-x%>%rename(!!col_1:=prior, !!col_2:=posterior)
  prs<-prs%>%left_join(x, by='hypo')
}

# ... and get all tasks for convinience
tasks<-tasks_from_df(1)
for (i in 2:6) tasks<-c(tasks, tasks_from_df(i))
all_trials<-data.frame(learn=rep(1:6, each=15), trial=rep(seq(15),6), task=tasks)
all_trials$task<-as.character(all_trials$task)

# ... and participant data for likelihood calculation
ppt_data<-df.sels%>%filter(sequence=='combined')%>%select(learningTaskId, trial, pred=selection, n)

# 2 calculate p_yes for each trial
get_p_yes<-function(lid, tid, mu, alpha) {
  ld<-as.list(df.learn_tasks[lid,c(2:4)])
  td<-read_task((all_trials%>%filter(learn==lid&trial==tid))$task)
  # ld to existing cat
  cat<-init_cat(mu)+count_feats(ld, 'A'); cats<-list(); cats[[1]]<-cat
  # weigh td
  p_cat<-stone_likeli(td, cat, 'A')*cat_prior(cat, cats, mu, alpha, 'A', F)
  p_new<-stone_likeli(td, init_cat(mu), 'A')*cat_prior(init_cat(mu), cats, mu, alpha, 'A', T)
  p_yes<-p_cat/(p_cat+p_new)
  return(p_yes)
}

# 3 get the final prediction
generate_pred<-function(mu, alpha) {
  get_trial_preds<-function(lid, tid, mu, alpha) {
    prediction<-list(); for(obj in all_objs) prediction[[obj]]<-0
    dh<-prs[,c('hypo', paste0('prior_',lid), paste0('posterior_',lid))]
    for (i in 1:nrow(dh)) {
      hp<-dh[i,1]; prior<-dh[i,2]; posterior<-dh[i,3]
      td<-read_task((all_trials%>%filter(learn==lid&trial==tid))$task)
      predicted<-get_hypo_preds(td, hp)
      p_yes<-get_p_yes(lid, tid, mu, alpha)
      for (pt in predicted) {
        size<-if (length(predicted)>1) beta-1 else 1
        # core
        prediction[[pt]]<-prediction[[pt]]+(p_yes*posterior/size+(1-p_yes)*prior/size)
      }
    }
    # format results
    p<-data.frame(matrix(unlist(prediction), nrow=length(prediction), byrow=T))
    colnames(p)<-c('prob')
    df<-cbind(data.frame(pred=all_objs), p)
    df$prob<-normalize(df$prob)
    df<-df%>%mutate(learningTaskId=paste0('learn0',lid), trial=tid)%>%select(learningTaskId, trial, pred, prob)
    return(df)
  }
  df<-get_trial_preds(1,1,mu,alpha)
  for (i in 1:6) {
    for (j in 1:15) {
      if (!(i==1&j==1)) df<-rbind(df, get_trial_preds(i,j,mu,alpha))
    }
  }
  return(df)
}

# 4 santity checks √
# 5 fit with grid search
fits<-data.frame(mu=numeric(0), alpha=numeric(0), likeli=numeric(0))
#mus=alphas=seq(0.01,1,by=0.02)
mus=alphas=seq(0.1,1,by=0.5)
for (m in mus) {
  for (a in alphas) {
    df<-generate_pred(m, a)
    likeli<-sum(log(df$prob)*ppt_data$n)
    fits<-rbind(fits, data.frame(mu=m,alpha=a,likeli=likeli))
  }
}
save(fits, file='out.Rdata')




# Fit current optimal with log-softmax















