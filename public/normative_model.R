
library(dplyr)
rm(list=ls())

# Revise for the causal default version
# Edit likelihoods for re-use, because learning conditions changed
# for cogsci submission
cf.inf<-df.inf%>%select(hypo, n, 
                        yes_c1, li_c1, post_c1,
                        yes_c2, li_c2, post_c2,
                        yes_c3, li_c3, post_c3,
                        yes_c4, li_c4, post_c4,
                        yes_c5=yes_c6, li_c5=li_c6, post_c5=post_c6,
                        yes_c6=yes_c7, li_c6=li_c7, post_c6=post_c7)



# Used from normative_model.R
features<-list()
features[['color']]<-c('b', 'r', 'y')
features[['shape']]<-c('c', 'd', 's')
# Get all stones
stones<-vector()
for (i in 1:length(features[['color']])) {
  for (j in 1:length(features[['shape']])) {
    stones[j+length(features[['shape']])*(i-1)]<-paste0(features[['color']][i], features[['shape']][j])
  }
}
# Create a stone-value list
for_stones<-function(default=0, stone_list=stones){
  stone_vals<-list()
  for (s in stone_list) stone_vals[[s]]<-default
  return(stone_vals)
}

# Checking functions
get_feature<-function(feature, stone) return(if (feature=='c') substr(stone,1,1) else substr(stone,2,2))
get_stone<-function(abbr, data) return(switch (abbr, 'a'=data[[1]], 't'=data[[2]], 'r'=data[[3]]))

check_hypo<-function(hypo, data) {
  if (typeof(data)!='list') data<-as.list(strsplit(data, ',')[[1]])
  hypos<-strsplit(hypo, ',')[[1]]
  pass<-TRUE
  for (hp in hypos) {
    if (substr(hp, 3, 3)=='_') {
      pass<-pass&TRUE
    } else {
      feature_type<-substr(hp, 1, 1)
      s1<-get_stone(tolower(substr(hp, 2, 2)), data)
      s2<-get_stone(tolower(substr(hp, 3, 3)), data)
      if (substr(hp, 2, 2)==toupper(substr(hp, 2, 2))) {
        pass<-pass&(get_feature(feature_type, s1)==get_feature(feature_type, s2))
      } else {
        pass<-pass&(get_feature(feature_type, s1)!=get_feature(feature_type, s2))
      }
    }
  }
  return(pass)
}

# Check the antecedent of a causal rule
check_ante<-function(hypo, data) {
  hypos<-strsplit(hypo, ',')[[1]]
  hypos<-hypos[!grepl(".[Rr].", hypos)] # only take antecedents
  antes<-paste(hypos, collapse=',')
  return(check_hypo(antes, data))
}

check_cons<-function(hypo, data) {
  hypos<-strsplit(hypo, ',')[[1]]
  hypos<-hypos[grepl(".[Rr].", hypos)] # only take consequents
  conse<-paste(hypos, collapse=',')
  return(check_hypo(conse, data))
}

# Prediction
# Get trials
tasks <- df.tasks%>%select(learningTaskId, trial, agent, recipient=target)
tasks$learningTaskId <-as.character(tasks$learningTaskId)
tasks$trial <- as.numeric(as.character(tasks$trial))
tasks$agent <- as.character(tasks$agent)
tasks$recipient <- as.character(tasks$recipient)

hypo_pred<-function(hypo, data) {
  r_stones<-for_stones()
  if (typeof(data)!='list') data<-as.list(strsplit(data, ',')[[1]])
  if (check_ante(hypo, data)==TRUE) {
    agent=rep(data[[1]], length(r_stones))
    recipient=rep(data[[2]], length(r_stones))
    result=names(r_stones)
    pass=rep(0, length(r_stones))
    pt<-data.frame(agent, recipient, result, pass)
    for (i in 1:length(r_stones)) {
      pt$pass[i]<-check_cons(hypo, list(pt$agent[i], pt$recipient[i], pt$result[i]))
    }
    pt$prob<-if (sum(pt$pass)==0) 0 else pt$pass/sum(pt$pass)
    for (s in names(r_stones)) r_stones[[s]]<-as.numeric(pt$prob[pt$result==s])
  } else {
    self<-data[[2]]; r_stones[[self]]<-1 # default to self
    # for (i in names(r_stones)) r_stones[[i]]<-1/9 # Or, random
  }
  return(r_stones)
}

# Get posterior predictives
get_post_pred<-function(cond, tid, pd=tasks, hd=cf.inf) {
  results<-for_stones()
  pt<-pd %>% filter(learningTaskId==paste0('learn0', cond)&trial==tid)
  data<-list(pt$agent, pt$recipient)
  for (h in hd$hypo) {
    #li<-if (check_ante(h, pd)) 1 else hd[(hd$hypo==h), paste0('post_c', cond)] 
    li<-hd[(hd$hypo==h), paste0('post_c', cond)] 
    preds<-hypo_pred(h, data)
    predp<-lapply(preds, "*", li)
    for (i in names(results)) {
      results[[i]]<-results[[i]] + predp[[i]]
    }
  }
  # return a formatted data frame
  learningTaskId<-rep(pt$learningTaskId, length(results))
  trial<-rep(pt$trial, length(results))
  selection<-names(results)
  prob<-as.vector(unlist(results))
  return(data.frame(learningTaskId, trial, selection, prob))
}


cf.nm<-get_post_pred(1,1)
for (i in 2:6) {
  for (j in 1:max(tasks$trial)) {
    cf.nm<-rbind(cf.nm, get_post_pred(i,j))
  }
}

d<-cf.nm%>%mutate(sequence='default')
r<-cf.nm%>%mutate(sequence='reverse')
df.nm<-rbind(d, r)
df.nm.inf<-cf.inf

save(df.nm, df.nm.inf, file='models.Rdata')

# Some checks
check<-df.nm.inf%>%filter(post_c1>0)%>%select(hypo, n, yes_c1, li_c1, post_c1)

# Softmax it
softmax_me<-function(cond, tid, seq, da, fa, data=df.nm) {
  base<-if (seq=='default') da else fa
  probs<-data%>%filter(learningTaskId==paste0('learn0', cond)&trial==tid&sequence==seq)
  probs$exp<-exp(base*probs$prob)
  total<-sum(probs$exp)
  probs$prob<-probs$exp/total
  return(probs)
}
softmax_me(1, 1, 'default', 3, 1)


full_model<-function(d, f){
  likeli=c()
  for (i in 1:6) {
    for (j in 1:15) {
      for (k in c('default', 'reverse')) {
        behavorial<-df.sels%>%
          filter(learningTaskId==paste0('learn0', i)&trial==j&sequence==k) %>%
          select(selection, n)
        pred<-softmax_me(i, j, k, d, f)%>% select(selection, prob)
        
        behavorial$selection<-as.character(behavorial$selection) 
        pred$selection<-as.character(pred$selection)
        
        data<-behavorial%>%left_join(pred, by='selection')
        likeli=c(likeli, sum(log(data$prob)*data$n))
      }
    }
  }
  -sum(likeli)
}
full_model(3, 3)
mle(full_model, start = list(d=1,f=1)) %>% attributes()



