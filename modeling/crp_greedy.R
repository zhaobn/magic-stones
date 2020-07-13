
# Load library and functions
source("./functions.R")
load("data/tasks.Rdata")
load("data/aggregated.Rdata")

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
  #return(cat_funcs)
  #return(length(cats))
  #return(cats)
}
get_sim<-function(lid, seq, n=1000, beta=3.8, temp=0,
                  feat_alpha=0.1, crp_alpha=0.1, grouping='A', 
                  ld_src=df.learn_tasks) {
  ld<-as.list(ld_src[lid,c(2:4)])
  
  #tasks<-all_tasks(ld, seq)
  tasks<-tasks_from_df(lid, seq)
  df<-prep_hypos(ld, beta, temp)
  
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

#####
# df<-data.frame(learningTaskId=character(0), condition=character(0), trial=integer(0),
#                pred=character(0), n=integer(0), prob=numeric(0))
# for (i in 1:6) {
#   for (s in c('near', 'far')) {
#     df<-rbind(df, get_sim(i, s, 100))
#   }
# }
# 
# ggplot(df, aes(x=pred, y=trial, fill=prob)) + geom_tile() +
#   scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
#   scale_fill_viridis(option="E", direction=-1) +
#   facet_grid(condition~learningTaskId)

# df.sim<-df
# save(df.sim, file='sim.Rdata')
#####

# Calculate likelihoods, prep model fitting
ppt_data<-df.sels%>%filter(!(sequence=='combined'))%>%
  mutate(condition=if_else(sequence=='default', 'near', 'far'))%>%
  select(learningTaskId, condition, trial, pred=selection, n, freq)

get_likeli<-function(model, data) {
  m<-model%>%select(learningTaskId, condition, trial, pred, prob)%>%
    mutate(condition=as.character(condition), pred=as.character(pred),
           prob=if_else(prob==0,eps,prob))
  p<-data%>%select(learningTaskId, condition, trial, pred, n)
  d<-p%>%left_join(m, by=c('learningTaskId', 'condition', 'trial', 'pred'))
  return(sum(log(d$prob)*d$n))
}
# get_likeli(df, ppt_data)


# Fit parameters
get_sim_df<-function(par) {
  df<-data.frame(learningTaskId=character(0), condition=character(0), trial=integer(0),
                 pred=character(0), n=integer(0), prob=numeric(0))
  for (i in 1:6) {
    for (s in c('near', 'far')) {
      # beta, temp, mu, alpha
      df<-rbind(df, get_sim(i, s, 200, par[1], par[2], par[3], par[4], 'A'))
    }
  }
  return(df)
}

try_params<-function(par, data) {
  set.seed(123)
  df<-get_sim_df(par)
  likeli<-get_likeli(df, data)
  return(-likeli)
}

#try_params(c(3.8,0, 0.1, 0.1), ppt_data)












