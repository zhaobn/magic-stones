
# Load library and functions
source("./functions.R")
load("../behavioral_data/tasks.Rdata")
load("../behavioral_data/aggregated.Rdata")

# Prep ppt data #######################################################
ppt_data<-rbind(
  (df.sels%>%filter(sequence=='default')%>%mutate(condition='near')%>%select(learningTaskId, condition, trial, selection, n)),
  (df.sels%>%filter(sequence=='reverse')%>%mutate(condition='far')%>%select(learningTaskId, condition, trial, selection, n))
)

# Get greedy simulations ##############################################
sim_preds<-function(preds, ld, seq, tasks, hypos, feat_alpha, crp_alpha, count_type) {
  cats<-list(); cat_funcs<-list()
  
  # Assign ld to first cat
  cats[[1]]<-init_cat(feat_alpha)+count_feats(ld, count_type)
  # Sample a function for it from posterior
  cat_funcs[[1]]<-sample(hypos$hypo, 1, prob=hypos$posterior)
  hypos<-hypos%>%filter(!(hypo==cat_funcs[[1]]))
  
  # Greedily assign categories
  for (i in 1:length(tasks)) {
    td<-read_task(tasks[i])
    # Check the probability of belonging to each existing category
    unnorm_probs<-vector()
    for (ci in (1:length(cats))) {
      p_cat<-stone_likeli(td, cats[[ci]], count_type)*
        cat_prior(cats[[ci]], cats, feat_alpha, crp_alpha, count_type, F)
      unnorm_probs<-c(unnorm_probs, p_cat)
    }
    # Or creating a new category
    cat_new<-init_cat(feat_alpha)
    #cat_new<-init_cat(feat_alpha)+count_feats(td, count_type)
    p_new<-stone_likeli(td, cat_new, count_type)*
      cat_prior(c(), cats, feat_alpha, crp_alpha, count_type, T)
    unnorm_probs<-c(unnorm_probs, p_new)
    # Normalize  
    probs<-normalize(unnorm_probs)
    # Assign cat accordinly
    cat_indexes<-seq(length(cats)+1)
    assigned_ci<-sample(cat_indexes, 1, prob=probs)
    if (assigned_ci>length(cats)) {
      cats[[assigned_ci]]<-init_cat(feat_alpha)+count_feats(td, count_type)
      # For newly-created cat, sample a function for it
      func<-sample(hypos$hypo, 1, prob=hypos$prior)
      cat_funcs[[assigned_ci]]<-func
      hypos<-hypos%>%filter(!(hypo==func))
      # Get predictions
      predicted<-sample(get_hypo_preds(td, func), 1)
    } else {
      cats[[assigned_ci]]<-cats[[assigned_ci]]+count_feats(td, count_type)
      # Get prediction
      predicted<-sample(get_hypo_preds(td, cat_funcs[[assigned_ci]]), 1)
    }
    # Add prediction to results
    trial_id<-if (seq=='near') i else (length(tasks)+1-i)
    preds<-preds%>%mutate(n=if_else(trial==trial_id&pred==predicted, n+1, n))
  }
  return(preds)
}
# Run sim_preds() n times
get_sim<-function(lid, seq, n=1000, beta=3.8,
                  feat_alpha=0.1, crp_alpha=0.1, grouping='A', 
                  ld_src=df.learn_tasks) {
  ld<-as.list(ld_src[lid,c(2:4)])
  
  tasks<-tasks_from_df(lid, seq)
  df<-prep_hypos(ld, beta)
  
  results<-init_results(seq)%>%mutate(learningTaskId=paste0('learn0', lid))%>%
    select(learningTaskId, condition, trial, pred, n)
  
  n_runs<-n
  while (n>0) {
    results<-sim_preds(results, ld, seq, tasks, df, feat_alpha, crp_alpha, grouping)
    n<-n-1
  }
  results$prob<-results$n/n_runs
  return(results)
} 
# Get sim results for all learning conditions
sim_results<-function(n=200, beta=5, alpha=0.1, mu=10) {
  set.seed(123)
  df<-get_sim(1, 'near', n, beta, mu, alpha)
  df<-rbind(df, get_sim(1, 'far', n, beta, mu, alpha))
  for (i in 2:6) {
    for (s in c('near', 'far')) {
      df<-rbind(df, get_sim(i, s, n, beta, mu, alpha))
    }
  }
  df<-df%>%mutate(condition=as.character(condition), selection=as.character(pred))%>%
    select(learningTaskId, condition, trial, selection, prob)
  return(df)
}

# Take a look at log-likelihood
df<-sim_results(5000, 7, 0.01, 0.05)
x<-df%>%left_join(ppt_data, by=c('learningTaskId', 'condition', 'trial', 'selection'))%>%filter(n>0)
sum(log(x$prob)*x$n) #-2548.891
y<-x%>%filter(n>0&prob==0); View(y)
save(df, file='sim.Rdata')
ggplot(df, aes(x=selection, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(condition~learningTaskId)

##########
# Fit a log-softmax
fit<-function(par, data, ppt) {
  data$x<-softmax_trials(data$prob, par, 'log')
  data<-data%>%
    left_join(ppt, by=c('learningTaskId', 'condition', 'trial','selection'))%>%
    filter(n>0)
  likeli<-sum(log(data$x)*data$n)
  return(-likeli)
}
fit(1, df, ppt_data)
out=optim(par=1, fn=fit, data=df, ppt=ppt_data,  method='Brent', lower=0.1, upper=10)
# par = 0.8907976, l=2542.592








