
# Load library and functions
source("./functions.R")
load("../behavioral_data/tasks.Rdata")
load("../behavioral_data/aggregated.Rdata")

# Preps ###############################################################
# Prep ppt data 
ppt_data<-rbind(
  (df.sels%>%filter(sequence=='default')%>%mutate(condition='near')%>%select(learningTaskId, condition, trial, selection, n)),
  (df.sels%>%filter(sequence=='reverse')%>%mutate(condition='far')%>%select(learningTaskId, condition, trial, selection, n))
)

# Optimize: prep hypo prediction data
all_preds<-data.frame(hypo=get_all_hypos(features))%>%mutate(hypo=as.character(hypo))
append_preds<-function(lid, tid, df=preds) {
  td<-as.list(df.gen_trials%>%filter(learningTaskId==paste0('learn0',lid)&trial==tid)%>%select(agent, recipient))
  pds<-c(); for (h in df$hypo) pds<-c(pds, paste0(get_hypo_preds(td, h), collapse=','))
  pcol<-paste0(lid,'-',tid); df[,pcol]<-pds
  return(df)
}
for (i in 1:6) {
  for (j in 1:15) {
    all_preds<-append_preds(i,j,all_preds)
  }
}

# Get greedy simulations 
sim_preds<-function(preds, lid, seq, tasks, hypos, feat_alpha, crp_alpha, count_type) {
  cats<-list(); cat_funcs<-list()
  ld<-as.list(df.learn_tasks[lid,c(2:4)])
  l_preds<-all_preds%>%select(hypo, starts_with(as.character(lid)))
  # Assign ld to first cat
  cats[[1]]<-init_cat(feat_alpha)+count_feats(ld, count_type)
  # Sample a function for it from posterior
  cat_funcs[[1]]<-sample(hypos$hypo, 1, prob=hypos$posterior)
  #hypos<-hypos%>%filter(!(hypo==cat_funcs[[1]]))
  
  # Greedily assign categories
  for (i in 1:length(tasks)) {
    td<-read_task(tasks[i])
    trial_id<-if (seq=='near') i else (length(tasks)+1-i)
    t_preds<-l_preds[,c(1,trial_id+1)]
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
      #hypos<-hypos%>%filter(!(hypo==func))
      # Get predictions
      options<-(t_preds%>%filter(hypo==cat_funcs[[assigned_ci]]))[,2]
      predicted<-sample(strsplit(options,',')[[1]],1)
      #predicted<-sample(get_hypo_preds(td, func), 1)
    } else {
      cats[[assigned_ci]]<-cats[[assigned_ci]]+count_feats(td, count_type)
      # Get prediction
      options<-(t_preds%>%filter(hypo==cat_funcs[[assigned_ci]]))[,2]
      predicted<-sample(strsplit(options,',')[[1]],1)
      #predicted<-sample(get_hypo_preds(td, ), 1)
    }
    # Add prediction to results
    if (sum(duplicated(unlist(cat_funcs)))==0) {
      preds<-preds%>%mutate(n=if_else(
        learningTaskId==paste0('learn0',lid)&
        condition==seq&
        trial==trial_id&
        pred==predicted, n+1, n))
    }
  }
  # runtime: 0.002
  return(preds)
}
# For all conditions
results<-rbind(init_all('near'), init_all('far'))
sim_task<-function(results, beta, feat_alpha, crp_alpha) {
  for (i in 1:6) {
    st=proc.time()[[3]]
    tasks=tasks_from_df(lid, seq)
    ld<-as.list(ld_src[lid,c(2:4)])
    df<-prep_hypos(ld, beta)
    for (s in c('near', 'far')) {
      results<-sim_preds(results, i, s, tasks, df, feat_alpha, crp_alpha, 'A')
    }
    et=proc.time()[[3]]
    print(paste0('cond ',i,': ', et-st))
  }
  return(results)
}
#x<-sim_task(results,7,0.1,0.1)
# n times
run_sim<-function(n, beta, alpha, mu) {
  results<-rbind(init_all('near'), init_all('far'))
  n_runs<-n
  while (n>0) {
    results<-sim_task(results,beta,mu,alpha)
    n<-n-1
  }
  results<-results%>%group_by(learningTaskId, trial, condition,pred)%>%
    summarise(n=sum(n))%>%mutate(prob=n/sum(n))
  return(results)
}
x<-run_sim(1, 7, 0.1, 0.1)

# Get sim results for all learning conditions
sim_results<-function(n=200, beta=5, alpha=0.1, mu=10) {
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

#####

# Take a look at log-likelihood
df<-sim_results(5000, 7, 0.06, 0.02)
x<-df%>%left_join(ppt_data, by=c('learningTaskId', 'condition', 'trial', 'selection'))%>%filter(n>0)
sum(log(x$prob)*x$n) #-2548.891
y<-x%>%filter(n>0&prob==0); View(y)
save(df, file='sim.Rdata')
ggplot(df, aes(x=selection, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(condition~learningTaskId)

##########
# Fits par = beta, alpha, mu, temp
set.seed(123)
model<-function(par, ppt) {
  restricted_feature_par=exp(par[1])+1
  restricted_alpha=exp(par[2])
  restricted_mu=exp(par[3])
  df<-sim_results(5000, restricted_feature_par, restricted_alpha, restricted_mu)
  df$z<-softmax_trials(df$prob, par[4], 'log')
  x<-df%>%left_join(ppt, by=c('learningTaskId', 'condition', 'trial', 'selection'))%>%filter(n>0)
  return(-sum(log(x$z)*x$n))
}

model(c(1, 3, 1, 1), ppt_data)

out=optim(par=c(1, 3, 1, 1), fn=model, ppt=ppt_data)


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




