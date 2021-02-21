
library(tidyverse)

rm(list=ls())
load('paper/hypos.Rdata')
load('../behavioral_data/tasks.Rdata')
load('../behavioral_data/aggregated.Rdata')
df.sels<-df.sels %>% filter(sequence=='combined')

source('paper/shared.R')

# Get hypos
single_feat_hypos<-function(feat) {
  hypo_vec<-c()
  for (e in c('equal', 'neq')) {
    hypo_vec<-c(hypo_vec, paste0(e,'(',feat,'(M),',feat,'(A))'))
    hypo_vec<-c(hypo_vec, paste0(e,'(',feat,'(M),',feat,'(R))'))
    for (v in feature_setting[[feat]]) {
      hypo_vec<-c(hypo_vec, paste0(e,'(',feat,'(M),',"'",v,"'",')'))
    }
  }
  return(hypo_vec)
}
color_hypos<-single_feat_hypos('color')
shape_hypos<-single_feat_hypos('shape')
hypos<-c(color_hypos, shape_hypos)
for (ch in color_hypos) {
  for (sh in shape_hypos) {
    hypos<-c(hypos, paste0('and(',ch,',',sh,')'))
  }
}
df.hypos<-data.frame(hypo=hypos, stringsAsFactors = F)
#save(df.hypos,file='paper/hypos.Rdata')

# Priors
get_prior<-function(hypo) {
  hasAnd<-grepl('and',hypo)
  relative_infs<-length(grep('A|R',strsplit(hypo,'')[[1]]))
  absolute_infs<-(hasAnd+1)-relative_infs
  prior<-(1/2)^(2*3)*(1/2)^relative_infs*(1/3)^absolute_infs
  return(prior)
}
df.hypos$prior<-mapply(get_prior, df.hypos$hypo)
df.hypos$prior<-normalize(df.hypos$prior)
#save(df.hypos,file='paper/hypos.Rdata')

# Posteriors
for (i in seq(6)) {
  cond<-paste0('learn0',i)
  data<-tasks %>% filter(phase=='learn', learningTaskId==cond) %>% 
    select(agent, recipient, result) %>% 
    paste0(.,collapse=',')
  ll_col<-paste0('ll_l',i)
  post_col<-paste0('post_l',i)
  df.hypos[,ll_col]<-mapply(causal_mechanism, df.hypos$hypo, rep(data,nrow(df.hypos)))
  df.hypos[,post_col]<-df.hypos[,'prior']*df.hypos[,ll_col]
  df.hypos[,post_col]<-normalize(df.hypos[,post_col])
}
df.hypos<-df.hypos %>%
  select(hypo, prior, starts_with('post_'))
#save(df.hypos, file='paper/hypos.Rdata')


# Predictions likelihoods
likelis<-list()
for (i in seq(6)) {
  cond<-paste0('learn0', i)
  likelis[[cond]]<-list()
  for (j in seq(15)) {
    task_data<-tasks %>% 
      filter(phase=='gen', learningTaskId==cond, trial==j) %>%
      select(agent, recipient) %>% paste0(., collapse=',')
    preds<-lapply(1:nrow(df.hypos), function(x) {
      causal_mechanism(df.hypos$hypo[x], task_data)
    })
    likelis[[cond]][[j]]<-preds
  }
}
#save(df.hypos, likelis, file='hypos.Rdata')


# Posterior predictives
model.uni<-data.frame(
  learningTaskId=character(0), trial=numeric(0),
  object=character(0), prob=numeric(0)
)


for (i in seq(6)) {
  cond<-paste0('learn0', i)
  post_col<-paste0('post_l',i)
  for (j in seq(15)) {
    ll<-likelis[[cond]][[j]]
    preds<-lapply(1:nrow(df.hypos), function(x) {
      Map('*', ll[[x]], df.hypos[x,post_col])
    }) %>%
      reduce(function(a,b) Map('+', a, b))
    preds.data<-data.frame(object=names(preds), prob=unlist(preds)) %>%
      mutate(learningTaskId=cond, trial=j) %>%
      select(learningTaskId, trial, object, prob)
    model.uni<-rbind(model.uni, preds.data)
  }
}

ggplot(model.uni, aes(x=object, y=trial, fill=prob)) +
  geom_tile() +
  facet_wrap(~learningTaskId) +
  scale_y_continuous(trans="reverse", breaks=1:15) + 
  scale_fill_gradient(low='white', high='#293352')
save(model.uni, file='paper/models.Rdata')

# Fit softmax
fit_softmax<-function(par) {
  softed<-data.frame(
    learningTaskId=character(0),
    trial=numeric(0),
    object=character(0),
    prob=numeric(0),
    soft=numeric(0)
  )
  for (i in seq(6)) {
    for (j in seq(15)) {
      dt<-model.uni %>% filter(learningTaskId==paste0('learn0',i), trial==j)
      dt$prob_s<-softmax(dt$prob, par)
      softed<-rbind(softed, dt)
    }
  }
  ppt_data<-df.sels %>%
    filter(sequence=='combined') %>%
    select(learningTaskId, trial, object=selection, n)
  softed<-softed %>% 
    mutate(object=as.character(object)) %>%
    left_join(ppt_data, by=c('learningTaskId', 'trial', 'object')) %>%
    filter(n>0)
  return(-sum(softed$n*log(softed$prob_s)))
}

out<-optim(par=0, fn=fit_softmax, method='Brent', lower=0, upper=100)
# par = 6.96
# ll = 2760.672

# model.uni<-softed
# save(model.uni, model.cat, model.proc, file='models.Rdata')
















