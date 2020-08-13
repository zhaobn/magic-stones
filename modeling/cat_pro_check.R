
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
#####

# Check cats - without replacement
sim_preds<-function(lid, seq, tasks, hypos, feat_alpha, crp_alpha, replace=F, count_type = 'A') {
  cats<-list(); cat_funcs<-list()
  ld<-as.list(df.learn_tasks[lid,c(2:4)])
  # tasks<-tasks_from_df(lid, seq)
  # hypos<-prep_hypos(ld, beta)
  # Assign ld to first cat
  cats[[1]]<-init_cat(feat_alpha)+count_feats(ld, count_type)
  # Sample a function for it from posterior
  cat_funcs[[1]]<-sample(hypos$hypo, 1, prob=hypos$posterior)
  if (!replace) hypos<-hypos%>%filter(!(hypo==cat_funcs[[1]]))
  
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
      if (!replace) hypos<-hypos%>%filter(!(hypo==func))
    } else {
      cats[[assigned_ci]]<-cats[[assigned_ci]]+count_feats(td, count_type)
    }
  }
  return(cat_funcs)
}
sim_preds(1,'near',7,0.1, 0.5)
sim_preds(1,'near',7,0.1, 0.5, T)

# Overlap per 1k simulations
check_dupes<-function(n, beta, mu, alpha, seq) {
  dupes=0
  lid=1
  
  ld<-as.list(df.learn_tasks[lid,c(2:4)])
  tasks<-tasks_from_df(lid, seq)
  hypos<-prep_hypos(ld, beta)
  
  i=n
  while (i>0) {
    funcs<-sim_preds(lid, seq, tasks, hypos, mu, alpha, T)
    if (sum(duplicated(unlist(funcs))) > 0) dupes<-dupes+1 
    i=i-1
  }
  
  return(dupes/n)
}

check_dupes(1000,4,10,0.05,'near')





