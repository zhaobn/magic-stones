
# Load library and functions
source("./functions.R")
load("data/tasks.Rdata")
load("data/aggregated.Rdata")

#######################################################################
# Do it in the sampling way ####
get_sim<-function(n=100, mu=0.1, alpha=0.1, beta=3.8, temp=0) {
  # Get simulation per condition
  sim_cond<-function(n, lid, seq) {
    ld<-as.list(df.learn_tasks[lid,c(2:4)]) # read learning data point
    dh<-prep_hypos(ld, beta, temp) # hypotheses prior and posterior
    cat<-init_cat(mu)+count_feats(ld, 'A') # learning's category
    cats<-list(); cats[[1]]<-cat
    tasks<-tasks_from_df(lid) # all tasks for this learning condition
    df<-init_results(seq, length(tasks)) # prep results dataframe
    # Get simulation results per trial
    sim_trial<-function(results, tid){
      # Preps
      task<-if (seq=='near') tasks[tid] else tasks[length(tasks)+1-tid]
      td<-read_task(tasks)
      # Decide if task stones belong to the same category as learning stones
      p_cat<-stone_likeli(td, cat, 'A')*cat_prior(cat, cats, mu, alpha, 'A', F)
      p_new<-stone_likeli(td, init_cat(mu), 'A')*cat_prior(init_cat(mu), cats, mu, alpha, 'A', T)
      p_yes<-p_cat/(p_cat+p_new)
      # If so, sample from posterior
      # If not, sample from prior
      if (runif(1)<p_yes) {
        func<-sample(dh$hypo, 1, prob=dh$posterior)
      } else {
        func<-sample(dh$hypo, 1, prob=dh$prior)
      }
      predicted<-sample(get_hypo_preds(td, func), 1)
      results<-results%>%mutate(n=if_else(trial==tid&pred==predicted, n+1, n))
      return(results)
    }
    # Run simulation
    iter=n;
    while (iter>0) {
      for (i in 1:length(tasks)) df<-sim_trial(df, i)
      iter<-iter-1
    }
    df<-df%>%mutate(learningTaskId=paste0('learn0',lid))%>%select(learningTaskId, condition, trial, pred, n)
    return(df)
  }
  # Run simulations
  df<-data.frame(learningTaskId=character(0), condition=character(0), 
                 trial=numeric(0), pred=character(0), n=numeric(0))
  for (i in 1:6) {
    for (s in c('near', 'far')) {
      df<-rbind(df, sim_cond(n, i, s))
    }
  }
  return(df)
}

df<-get_sim(10000, 0.2, 0.08, 3.8, 0)
df$prob<-df$n/10000
df.sim.10k<-df
df.sim.10k<-df.sim.10k%>%mutate(condition=as.character(condition), pred=as.character(pred))
#save(df.sim.10k, file='data/sim.Rdata')
#####

# Do it analytically ####
get_crp_norm_pred<-function(mu=0.1, alpha=0.1, beta=3.8, temp=0) {
  # calculate stone posterior per hypo
  calc_stone_post<-function(td, dh, type='post') {
    calc_hypo_stone_post<-function(hypo, post, td, stone) {
      preds<-get_hypo_preds(td, hypo)
      is_predicted<-as.numeric(stone%in%preds)
      return((is_predicted/length(preds))*post)
    }
    stones<-all_objs
    probs<-c()
    for (s in stones) {
      p<-0
      for (i in 1:nrow(dh)) {
        hypo<-dh$hypo[i]
        post<-if (type=='post') dh$posterior[i] else dh$prior[i] 
        p<-p+calc_hypo_stone_post(hypo, post, td, s)
      }
      probs<-c(probs, p)
    }
    result<-data.frame(pred=stones, prob=probs)
    result$prob<-normalize(result$prob)
    return(result)
  }
  # get calculations per trial
  get_cond_pred<-function(lid, seq) {
    ld<-as.list(df.learn_tasks[lid,c(2:4)]) # read learning data point
    dh<-prep_hypos(ld, beta, temp) # hypotheses prior and posterior
    cat<-init_cat(mu)+count_feats(ld, 'A') # learning's category
    cats<-list(); cats[[1]]<-cat
    tasks<-tasks_from_df(lid) # all tasks for this learning condition

    get_trial_pred<-function(tid) {
      # Preps
      tid<-if (seq=='near') tid else length(tasks)+1-tid
      td<-read_task(tasks[tid])
      # Decide if task stones belong to the same category as learning stones
      p_cat<-stone_likeli(td, cat, 'A')*cat_prior(cat, cats, mu, alpha, 'A', F)
      p_new<-stone_likeli(td, init_cat(mu), 'A')*cat_prior(init_cat(mu), cats, mu, alpha, 'A', T)
      p_yes<-p_cat/(p_cat+p_new)
      # If so, sample from posterior
      yes<-calc_stone_post(td, dh, 'post')
      # If not, sample from prior
      new<-calc_stone_post(td, dh, 'prior')
      # Combine to get the mixed posterior
      pd<-(yes%>%left_join(new, by='pred'))%>%
        mutate(mixed=p_yes*prob.x+(1-p_yes)*prob.y,
               learningTaskId=paste0('learn0', lid), trial=tid, condition=seq)%>%
        select(learningTaskId, condition, trial, pred, prob=mixed)
      return(pd)
    }
    df<-get_trial_pred(1)
    for (i in 2:length(tasks)) df<-rbind(df, get_trial_pred(i))
    df<-df%>%arrange(learningTaskId, condition, trial, pred)
    return(df)
  }
  # get all predictions
  df<-data.frame(learningTaskId=character(0), condition=character(0), 
                 trial=numeric(0), pred=character(0), prob=numeric(0))
  for (i in 1:6) {
    for (s in c('near', 'far')) {
      df<-rbind(df, get_cond_pred(i, s))
    }
  }
  return(df)
}

df.sim.analytical<-get_crp_norm_pred(0.2, 0.08, 3.8, 0)
df.sim.analytical<-df.sim.analytical%>%mutate(pred=as.character(pred))
save(df.sim.10k, df.sim.analytical, file='data/sim.Rdata')

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




