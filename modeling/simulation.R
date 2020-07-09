
# Load library and functions
source("./functions.R")

#######################################################################
#### Helper functions #################################################
# Returns hypothesis stats
#   @ld {list}, @beta {numeric}
prep_hypos<-function(ld, beta=3.8) {
  all_hypo<-get_all_hypos(features)
  
  df<-data.frame(hypo=all_hypo)%>%mutate(hypo=as.character(hypo))
  n<-nrow(df)
  
  df$prior<-normalize(mapply(get_hypo_prior, df$hypo, rep(beta,n)))
  df$posterior<-normalize(
    df$prior * mapply(data_given_hypo, rep(flatten(ld),n), df$hypo, rep(beta,n))
  )
  return(df)
}

# Returns simulated predictions
#   @ld {list}, @tasks {vector of string},
#   @hypos {dataframe} hypo|prior|posterior
#   @feat_alpha {numeric}, @crp_alpha {numeric}
#   @count_type {string}: "A", "AR"
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

# Returns a dataframe to hold simulation results
#   @seq {string} "near", "far"
#   @n_tasks {integer} length(tasks)
init_results<-function(seq='near', n_tasks=15) {
  df<-expand.grid(trial=seq(n_tasks), condition=seq, pred=all_objs, n=0)%>%
    arrange(trial, pred)
}

# Returns simulation results
#   @lid {integer} learning task id, 1-6
#   @seq {string} "near", "far"
#   @n {integer} simulation runs
#   @beta, feat_alpha, crp_alpha {numeric} see functions.R
#   @grouping {string} "A", "AR", see functions.R
#   @ld_src {dataframe} use df.learn_src from './data/tasks.Rdata'
get_sim<-function(lid, seq, n=1000, beta=3.8,
                  feat_alpha=0.1, crp_alpha=0.1, grouping='A', 
                  ld_src=df.learn_tasks) {
  ld<-as.list(ld_src[lid,c(2:4)])
  
  #tasks<-all_tasks(ld, seq)
  tasks<-tasks_from_df(lid, seq)
  df<-prep_hypos(ld, beta)
  
  results<-init_results(seq)%>%mutate(learningTaskId=paste0('learn0', lid))%>%
    select(learningTaskId, condition, trial, pred, n)
  
  n_runs<-n
  while (n>0) {
    results<-sim_preds(results, ld, seq, tasks, df, feat_alpha, crp_alpha, grouping)
    n<-n-1
  }
  results$freq<-results$n/n_runs
  return(results)
} 

#### Run sims #########################################################
df<-data.frame(learningTaskId=character(0), condition=character(0), trial=integer(0),
               pred=character(0), n=integer(0), freq=numeric(0))
for (i in 1:6) {
  for (s in c('near', 'far')) {
    df<-rbind(df, get_sim(i, s, 1000))
  }
}

ggplot(df, aes(x=pred, y=trial, fill=freq)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(condition~learningTaskId)

df.sim<-df
save(df.sim, file='sim.Rdata')

# x<-get_sim(1, 'near', 100)
# ggplot(x, aes(x=pred, y=trial, fill=freq)) + geom_tile() +
#   scale_y_continuous(trans="reverse", breaks=unique(x$trial)) +
#   scale_fill_viridis(option="E", direction=-1)

#### Fit parameters #####################################################
ppt_data<-df.sels%>%filter(!(sequence=='combined'))%>%
  mutate(condition=if_else(sequence=='default', 'near', 'far'))%>%
  select(learningTaskId, condition, trial, pred=selection, ppt=n)

sim<-function(par, data) {
  set.seed(6)
  df<-data.frame(learningTaskId=character(0), condition=character(0), trial=integer(0),
                 pred=character(0), n=integer(0), freq=numeric(0))
  for (i in 1:6) {
    for (s in c('near', 'far')) {
      df<-rbind(df, get_sim(i, s, 100, 3.8, par[1], par[2]))
    }
  }
  df<-df%>%mutate(condition=as.character(condition), pred=as.character(pred))%>%
    left_join(data, by=c('learningTaskId', 'condition', 'trial', 'pred'))%>%
    mutate(prob=if_else(freq==0, eps, freq))
  
  return(-sum(log(df$prob)*df$ppt))
} 

#x<-sim(c(0.1,0.1), ppt_data); x
#out=optim(par=c(0.1, 0.1), fn=sim, data=ppt_data)
# out$par 0.08905396 0.10702393
# out$value 3146.072

#refit=optim(par=c(1, 1), fn=sim, data=ppt_data)
# refit$par 1.107895 1.055760
# refit$value 2955.638
# sim(c(1.1,1), ppt_data) 3184.891 => refit result makes no sense

# Grid search result is better
sim(c(0.2,0.08), ppt_data) # 3002.539
sim(c(eps,eps), ppt_data) # 3885.358

# Plot >1 params?
df<-data.frame(learningTaskId=character(0), condition=character(0), trial=integer(0),
               pred=character(0), n=integer(0), freq=numeric(0))
for (i in 1:6) {
  for (s in c('near', 'far')) {
    df<-rbind(df, get_sim(i, s, 100, 3.8, 1.11, 1.06))
  }
}

ggplot(df, aes(x=pred, y=trial, fill=freq)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(condition~learningTaskId)

df.sim.loose<-df

df<-data.frame(learningTaskId=character(0), condition=character(0), trial=integer(0),
               pred=character(0), n=integer(0), freq=numeric(0))
for (i in 1:6) {
  for (s in c('near', 'far')) {
    df<-rbind(df, get_sim(i, s, 100, 10, 0.1, 0.1))
  }
}

ggplot(df, aes(x=pred, y=trial, fill=freq)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(condition~learningTaskId)

df.sim.loose<-df
df.sim.tight<-df
df.sim.me<-df
save(df.sim, df.sim.tight, df.sim.loose, df.sim.me, file='./data/sim.Rdata')


#### Analyse Eddie fitting results #####################################
# Run on Eddie ####
x<-c(seq(0.01,0.1,by=0.01), seq(0.2,1,by=0.1))
y<-c(seq(0.01,0.1,by=0.01), seq(0.2,1,by=0.1))
m<-matrix(nrow=length(x), ncol=length(y))
for (xi in 1:length(x)) {
  for (yi in 1:length(y)) {
    m[xi,yi]<-sim(c(x[xi],y[yi]), ppt_data)
  }
}

write.table(m, file=paste0(path,'output.txt'))

# Plot sensitivity ####
library(plotly)
library(htmlwidgets)

m<-matrix(unlist(read.table('data/output.txt')), ncol=length(y))
p<-plot_ly(x=y, y=x, z=m, type = "surface")%>% 
  layout(
    title = "Sensitivity analysis",
    scene = list(
      xaxis = list(title = "crp_concentration"),
      yaxis = list(title = "feature_prior"),
      zaxis = list(title = "Loglikelihood")
    ))
p
saveWidget(p, file="sensitivity_analysis.html")

library(GA)
par(mfrow=c(1,2))
persp3D(x, y, m, theta=-30, phi=15, expand=1, col.palette=topo.colors,
        xlab="feature_prior", ylab="crp_concentration", zlab="")
persp3D(x, y, m, theta=120, phi=15, expand=1, col.palette=topo.colors,
        xlab="feature_prior", ylab="crp_concentration", zlab="")


# persp(x2,y2,m2, shade=0.4,col="cyan",
#         theta=-40, phi=-0,
#         nticks=10, ticktype="detailed",
#         xlab="feature_prior", ylab="crp_concentration", zlab="log_likelihood", main="Sensitivity analysis")
# 
# persp(x2,y2,m2, shade=0.4, col="cyan",
#       theta=140, phi=-0,
#       nticks=10, ticktype="detailed",
#       xlab="feature_prior", ylab="crp_concentration", zlab="log_likelihood", main="Sensitivity analysis")


# Run with fitted parameters ####
df<-data.frame(learningTaskId=character(0), condition=character(0), trial=integer(0),
               pred=character(0), n=integer(0), freq=numeric(0))
for (i in 1:6) {
  for (s in c('near', 'far')) {
    df<-rbind(df, get_sim(i, s, 1000, 3.8, 0.2, 0.08))
  }
}
df.sim<-df
save(df.sim, file='./data/sim.Rdata')

# Try new values
df<-data.frame(learningTaskId=character(0), condition=character(0), trial=integer(0),
               pred=character(0), n=integer(0), freq=numeric(0))
for (i in 1:6) {
  for (s in c('near', 'far')) {
    df<-rbind(df, get_sim(i, s, 1000, 3.8, 0.06, 0.08))
  }
}
df.sim<-df
save(df.sim, file='./data/sim.Rdata')


#### Not used ##########################################################
# Returns average categories per n simulations
#   @n {integer} n runs
#   @seq {string} "near", "far"
#   @type {string} "agent-only" or "agent, "pair"
#   @feat_alpha {numeric} larger => more categories
#   @crp_alpha {numeric} larger => more categories
get_avg_cats<-function(n, seq, type, feat_alpha, crp_alpha) {
  total<-0; n_run<-n;
  count_type<-if (type=='pair') 'AR' else 'A'
  tasks<-all_tasks(ld, seq)
  while (n>0) {
    # !!! If you want to run this, 
    # !!! set sim_feat_cat() to return number of cats
    total<-total+sim_feat_cat(ld, tasks, feat_alpha, crp_alpha, count_type)
    n<-n-1
  }
  return(round(total/n_run,2))
}
#get_avg_cats(100, 'near', 'pair', 0.1, 0.1)

ld<-list("agent"="rs", "recipient"="yc", "result"="ys")

task_type<-c('near', 'far')
count_type<-c('agent_only', 'pair')
feature_alpha<-c(0.01, 0.1, 0.2, 1)
crp_alpha<-c(0.1, 0.5, 0.8)

df.sim<-expand.grid(condition=task_type, grouping=count_type, feature_alpha=feature_alpha, crp_alpha=crp_alpha)
df.sim$avg_cats<-mapply(get_avg_cats, rep(1000,nrow(df.sim)), 
                        df.sim$condition, df.sim$grouping, 
                        df.sim$feature_alpha, df.sim$crp_alpha)

df.cat.flat<-df.sim # functions/sim_feat_cat() use init_cat(f_alpha)
df.cat.addf<-df.sim # functions/sim_feat_cat() use init_cat(f_alpha)+count_feat(td)
save(df.cat.flat, df.cat.addf, file='data/greedy_cat.Rdata')

# Plot results
ggplot(df.cat.flat, aes(x=grouping, y=avg_cats, fill=condition))+
  geom_bar(position="dodge", stat="identity")+
  facet_grid(crp_alpha~feature_alpha)

#######################################################################





