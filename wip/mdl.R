
options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)
rm(list=ls())

# Settings
eps<-.Machine$double.eps

features<-list()
features[['color']]<-c('b', 'r', 'y')
features[['shape']]<-c('c', 'd', 's')

abbs<-list()
abbs[['c']]<-'color'
abbs[['s']]<-'shape'

relations<-c('=', '~')

stones<-c()
for (c in features[[1]]) {
  for (s in features[[2]]) {
    stones<-c(stones, paste0(c, s))
  }
}

# Generate hypotheses
## 1. Atomic descriptions
generate_atomic_hypos<-function(role, feature, type) {
  hypos<-list()
  compose<-function(val_1, val_2, relation) {
    atomic<-list()
    atomic[['role']]<-role
    atomic[['feature']]<-feature
    atomic[['type']]<-type
    atomic[['desc']]<-paste0(val_1, relation, val_2)
    return(atomic)
  }
  f<-substr(feature, 1, 1)
  if (type=='absolute') {
    obj<-if (role=='e') paste0(f, '(T)') else paste0(f, '(', c('A', 'R'), ')')
    val<-paste0(f,features[[feature]])
  } else {
    obj<-if (role=='e') paste0(f, '(T)') else paste0(f, '(A)') 
    val<-if (role=='e') paste0(f, '(', c('A', 'R'), ')') else paste0(f, '(R)')
  }
  for (o in 1:length(obj)) {
    for (v in 1:length(val)) {
      for (r in 1:length(relations)) {
        idx<-(o-1)*length(val)*length(relations)+(v-1)*length(relations)+r
        hypos[[idx]]<-compose(obj[o], val[v], relations[r])
      }
    }
  }
  return(do.call(rbind.data.frame, hypos))
}
atomics<-list()
atomics<-generate_atomic_hypos('c', 'color', 'relative')
for (r in c('c', 'e')) {
  for (f in names(features)) {
    for (t in c('relative', 'absolute')) {
      if (!(r=='c'&f=='color'&t=='relative')) atomics<-rbind(atomics, generate_atomic_hypos(r, f, t))
    }
  }
}
rownames(atomics) <- NULL
atomics$desc<-as.character(atomics$desc)

## 2. Composite hypotheses
color_effects<-(atomics%>%filter(role=='e'&feature=='color')%>%select(desc))[[1]]
shape_effects<-(atomics%>%filter(role=='e'&feature=='shape')%>%select(desc))[[1]]
color_causes<-(atomics%>%filter(role=='c'&feature=='color')%>%select(desc))[[1]]
shape_causes<-(atomics%>%filter(role=='c'&feature=='shape')%>%select(desc))[[1]]

### 2.1 Single universal effect descriptions
single_effects<-c(color_effects, shape_effects)

### 2.2 Double universal effect descriptions
double_effects<-c()
for (c in color_effects) {
  for (s in shape_effects) {
    double_effects<-c(double_effects, paste(c(c,s), collapse=','))
  }
}
uni_effects<-c(single_effects, double_effects)

### 2.3 Single cause descriptions
single_causes<-c(color_causes, shape_causes)
single_cause_effects<-c()
for (c in single_causes) {
  for (e in uni_effects) {
    single_cause_effects<-c(single_cause_effects, paste0(e, '[', c, ']'))
  }
}

### 2.4 Multi cause descriptions
multi_cause_effects<-c()
terminate<-0.5
sample_multi<-function(hypo){
  hypos<-if (length(hypo)>0) strsplit(hypo, '\\|')[[1]] else c()
  sampled<-sample(single_cause_effects, 1)
  if (sampled %in% hypos) return(hypo) else {
    hypo<-paste(c(hypos, sampled), collapse='|')
    if (runif(1)<0.5) return(hypo) else return(sample_multi(hypo))
  }
}
for (i in seq(2000)) {
  multi_cause_effects<-c(multi_cause_effects, sample_multi(sample(single_cause_effects, 1)))
}
multi_cause_effects<-unique(multi_cause_effects)

### Final hypothesis space
hypos<-c()
hypos<-c(hypos, uni_effects)
hypos<-c(hypos, single_cause_effects)
hypos<-c(hypos, multi_cause_effects)

hypo_space<-data.frame(hypos)
colnames(hypo_space)<-c('hypo')
hypo_space$hypo<-as.character(hypo_space$hypo)

df.hypos<-hypo_space

# Complexity measure
get_comp<-function(hypo, penalty='gen') {
  get_prob<-function(at) {
    relation<-substr(at, 5, 5)
    prob<-1/4 # draw certain feature and relation
    els<-strsplit(at, relation)[[1]]
    prob<-if (substr(els[1],3,3)=='T') prob else prob/2
    prob<-if (grepl('\\(', els[2])) prob/2 else prob/3
    return(prob)
  }
  atomics<-c()
  read_desc<-function(str){
    if (grepl('\\[', str)) {
      ats<-strsplit(str, '\\[')[[1]]
      conds<-gsub('\\]', '', ats[2])
      return(read_desc(paste(c(ats[1], conds), collapse=',')))
    } else {
      return(strsplit(str, ',')[[1]])
    }
  }
  if (grepl('\\|', hypo)) {
    hypos<-strsplit(hypo, '\\|')[[1]]
    for (h in hypos) atomics<-c(atomics, read_desc(h))
  } else atomics<-read_desc(hypo)
  if (penalty=='gen') {
    prob<-1
    for (a in atomics) prob<-prob*get_prob(a)
    return(prob*(0.5)^(length(atomics)-1))
  } else {
    return(exp(-length(atomics))) 
  }
}
df.hypos$exp<-mapply(get_comp, df.hypos$hypo)
df.hypos$gen<-mapply(get_comp, df.hypos$hypo)

# classify hypo
hypo_type<-function(hypo) {
  if (grepl('\\|', hypo)) return('ifelse') else
    if (grepl('\\[',hypo)) return('conditional') else
      return('universal')
}
df.hypos$type<-mapply(hypo_type, df.hypos$hypo)

# Checkers
flatten<-function(list, sep=',') {
  str=c()
  for (i in 1:length(list)) str<-c(str, list[[i]])
  return(paste(str, collapse=sep))
}
to_list<-function(str, sep=',') {
  vecs<-strsplit(str, sep)[[1]]
  data<-list()
  data[['A']]<-vecs[1]
  data[['R']]<-vecs[2]
  if (length(vecs) > 2) data[['T']]<-vecs[3]
  return(data)
}

check_hypo<-function(hypo, obs, type='strict') {
  obs<-if (typeof(obs)!='list') to_list(obs) else obs
  soft<-function(val, type) {
    if (type=='strict') return(val) else
      if (val==1) return(1-eps) else return(eps)
  }
  check_desc<-function(desc, obs, type) {
    check_atomic<-function(at, obs, type) {
      read_val<-function(v, str) {
        if (grepl('\\(', str)) {
          f_idx<-if (f=='c') 1 else 2
          return(substr(obs[[substr(str, 3, 3)]], f_idx, f_idx))
        } else return(substr(str, 2, 2))
      }
      f<-substr(at, 1, 1); feature<-abbs[[f]]
      relation<-substr(at, 5, 5)
      vals<-strsplit(at, relation)[[1]]
      is_match<-(read_val(f, vals[1])==read_val(f, vals[2]))
      if (relation=='=') {
        if (is_match) return(soft(1, type)) else return(soft(0, type))
      } else {
        if (is_match) return(soft(0, type)) else return(soft(1, type)/2)
      }
    }
    has_precon<-grepl('\\[', desc)
    effect_desc<-if (has_precon) strsplit(desc, '\\[')[[1]][1] else desc
    precond_desc<-if (has_precon) gsub('\\]', '', strsplit(desc, '\\[')[[1]][2]) else ''
    effs<-strsplit(effect_desc, ',')[[1]]; pres<-strsplit(precond_desc, ',')[[1]]
    
    if (length(effs)==1) {
      f<-substr(effs, 1, 1)
      other_f<-if (f=='c') 's' else 'c'
      default<-paste0(other_f, '(T)', '=', other_f, '(R)')
      effs<-c(effs, default)
    }
    
    sat_eff<-1
    for (e in effs) sat_eff<-sat_eff*check_atomic(e, obs, type)
    
    if (!has_precon) return(sat_eff) else {
      sat_pres<-1
      for (p in pres) sat_pres<-sat_pres*check_atomic(p, obs, type)
      if (sat_pres>eps) return(sat_pres*sat_eff) else {
        if (obs[['R']]==obs[['T']]) return(soft(1, type)) else return(soft(0, type))
      }
    }
  }
  is_multi<-grepl('\\|', hypo)
  if (!is_multi) return(check_desc(hypo, obs, type)) else {
    sat<-soft(0, type)
    descs<-strsplit(hypo, '\\|')[[1]]
    for (d in descs) sat<-sat+check_desc(d, obs, type)
    return(min(sat, 1))
  }
}

# Trial data
df.learn_tasks<-read.csv('learn_trials.csv')
df.learn_tasks$learningTaskId<-as.character(df.learn_tasks$learningTaskId)
df.learn_tasks$A<-as.character(df.learn_tasks$A)
df.learn_tasks$R<-as.character(df.learn_tasks$R)
df.learn_tasks$T<-as.character(df.learn_tasks$T)
save(df.hypos, df.learn_tasks, file='mdl.Rdata')

for (i in 1:6) {
  tid<-paste0('learn0',i)
  learn_data<-as.list(df.learn_tasks%>%filter(learningTaskId==tid)%>%select(A,R,T))
  df.hypos[,tid]<-mapply(check_hypo, df.hypos$hypo, rep(flatten(learn_data), length(df.hypos$hypo)))
}


# Generalization predictions
df.gen_trials<-df.trials%>%select(learningTaskId, trial, agent, recipient)%>%distinct()
colnames(df.gen_trials)<-c('learningTaskId', 'trial', 'A', 'R')
df.gen_trials$learningTaskId<-as.character(df.gen_trials$learningTaskId)
df.gen_trials$A<-as.character(df.gen_trials$A)
df.gen_trials$R<-as.character(df.gen_trials$R)

get_pred<-function(cid, tid, type='exp', src=test, gen_src=df.gen_trials) {
  normalize<-function(vec) {
    total<-sum(vec)
    for (i in 1:length(vec)) vec[i]<-vec[i]/total
    return(vec)
  }
  gd<-as.list(gen_src%>%filter(learningTaskId==cid&trial==tid)%>%select(A, R))
  gen_results<-c()
  for (i in 1:length(stones)) {
    dp<-gd; dp[['T']]<-stones[i]
    da<-src%>%select(hypo, !!type, !!cid)%>%mutate(data=flatten(dp))
    da$pass<-mapply(check_hypo, da$hypo, da$data)
    da$post<-da[,type]*da[,cid]*da$pass
    gen_results<-c(gen_results, sum(da$post))
  }
  stone<-stones
  prob<-normalize(gen_results)
  learningTaskId<-rep(cid, length(stones))
  trial<-rep(tid, length(stones))
  comp<-rep(type, length(stones))
  return(data.frame(learningTaskId, trial, comp, stone, prob))
}

df.pred<-get_pred('learn01', 1, 'exp', df.hypos)
df.pred<-rbind(df.pred, get_pred('learn01', 1, 'gen', df.hypos))
# Take an initial look
last_trial<-get_pred('learn01', 15, 'gen', df.hypos)
diff_trial<-get_pred('learn02', 1, 'gen', df.hypos)

# Full posterior prediction
df.pred.gen<-get_pred('learn01', 1, 'gen', df.hypos)
for (i in 1:15) {
  df.pred.gen<-rbind(df.pred.gen, get_pred('learn01', i, 'gen', df.hypos))
}
df.pred.gen<-df.pred.gen%>%distinct()
for (c in 2:6) {
  cid<-paste0('learn0',c)
  for (i in 1:15) {
    df.pred.gen<-rbind(df.pred.gen, get_pred(cid, i, 'gen', df.hypos))
  }
}
df.pred.gen$learningTaskId<-as.character(df.pred.gen$learningTaskId)
df.pred.gen$stone<-as.character(df.pred.gen$stone)

df.pred.exp<-get_pred('learn01', 1, 'exp', df.hypos)
for (c in 1:6) {
  cid<-paste0('learn0',c)
  for (i in 1:15) {
    if (!(c==1&i==1)) df.pred.exp<-rbind(df.pred.exp, get_pred(cid, i, 'exp', df.hypos))
  }
}

df.pred<-rbind(df.pred.gen, df.pred.exp)
colnames(df.pred)<-c('learningTaskId', 'trial', 'comp', 'selection', 'prob')
df.pred$learningTaskId<-as.character(df.pred$learningTaskId)
df.pred$selection<-as.character(df.pred$selection)

# Model comparison
## Get ppt data
df.sels<-df.sels%>%select(learningTaskId, sequence, trial, selection, n, freq)
df.sels$learningTaskId<-as.character(df.sels$learningTaskId)
df.sels$selection<-as.character(df.sels$selection)

df.sels<-df.sels%>%
  left_join(df.pred%>%
              filter(comp=='gen')%>%
              select(learningTaskId, trial, selection, gen=prob), 
            by=c('learningTaskId', 'trial', 'selection'))%>%
  left_join(df.pred%>%
              filter(comp=='exp')%>%
              select(learningTaskId, trial, selection, exp=prob), 
            by=c('learningTaskId', 'trial', 'selection'))
save(df.learn_tasks, df.gen_trials, df.hypos, df.pred, df.sels, file='mdl.Rdata')

## Regression
get_rsq<-function(lid, tid, seq, type, data=df.sels) {
  dt<-data%>%filter(learningTaskId==lid&sequence==seq&trial==tid)%>%
    select(selection, freq, pred=!!type)
  rsq<-summary(lm(data=dt, freq~pred))$r.squared
  df<-data.frame('learningTaskId'=c(lid),
                 'trial'=c(tid),
                 'sequence'=c(seq),
                 'comp'=c(type),
                 'rsq'=c(round(rsq,4)))
  return(df)
}
df.rsq<-get_rsq('learn01',1,'default','gen')
for (i in 1:6) {
  lid<-paste0('learn0', i)
  for (tid in 1:15) {
    for (comp in c('gen','exp')) {
      for (seq in c('default', 'reverse')) {
        if (!(i==1&tid==1&comp=='gen'&seq=='default')) {
          df.rsq<-rbind(df.rsq, get_rsq(lid, tid, seq, comp))
        }
      }
    }
  }
}

save(file='mdl.Rdata', df.hypos, df.sels, df.rsq)
save(file='tasks.Rdata', df.learn_tasks, df.gen_trials)
## Plots
### Plot heatmap
forheatmap<-df.rsq%>%
  mutate(learn=case_when(learningTaskId=='learn01'~'L1',
                         learningTaskId=='learn02'~'L2',
                         learningTaskId=='learn03'~'L3',
                         learningTaskId=='learn04'~'L4',
                         learningTaskId=='learn05'~'L5',
                         learningTaskId=='learn06'~'L6'),
         sequence=if_else(sequence=='default', 'near', 'far')) %>%
  select(learn, sequence, trial, comp, rsq)
forheatmap$sequence = factor(forheatmap$sequence, levels=c('near', 'far'))

ggplot(data=forheatmap, aes(x=comp, y=trial, fill=rsq)) + 
  geom_tile(colour = "black") +
  geom_text(aes(label = round(rsq, 2)), color="grey50", size=3) +
  labs(x='', y='', fill='R^2') +
  #scale_fill_gradient(low = "white", high = "steelblue4") +
  xlim('gen', 'exp') +
  scale_y_reverse(labels = c('task 1', seq(2, 15)), breaks = seq(1,15)) +
  scale_fill_viridis(direction=-1) +
  theme(legend.position="right", 
        panel.background = element_blank(),
        text = element_text(size=15)
  ) +
  facet_grid(sequence~learn) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 10))

### Plot predictions
near<-df.sels%>%filter(sequence=='default')%>%mutate(type='near')%>%
  select(learningTaskId, sequence, trial, selection, type, value=freq)
far<-df.sels%>%filter(sequence=='reverse')%>%mutate(type='far')%>%
  select(learningTaskId, sequence, trial, selection, type, value=freq)
exp<-df.sels%>%filter(sequence=='default')%>%mutate(type='exp')%>%
  select(learningTaskId, sequence, trial, selection, type, value=exp)
gen<-df.sels%>%filter(sequence=='default')%>%mutate(type='gen')%>%
  select(learningTaskId, sequence, trial, selection, type, value=gen)
plotdata<-rbind(near, far, exp, gen)
plotdata$type = factor(plotdata$type, levels=c('near', 'far', 'gen', 'exp'))

ggplot(plotdata, aes(x=selection, y=value, fill=type)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(trial ~ learningTaskId) +
  labs(x='', y='') + scale_fill_brewer(palette="RdBu") + 
  theme_light() +
  theme(legend.position="bottom", legend.title=element_blank())

### Plot overall regressions
df<-rbind(
  df.sels%>%select(selection,sequence, freq,pred=gen)%>%mutate(model='gen'),
  df.sels%>%select(selection,sequence, freq,pred=exp)%>%mutate(model='exp'))
ggplot(data=df, aes_string(x="pred", y="freq")) + 
  geom_point(shape=3, size=1) + 
  #labs(x=col, y='') +
  #scale_x_continuous(limits = c(0, 1)) + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  facet_grid(sequence~model) +
  #annotate("text", label = paste0("R^2 == ", r2), x = 1, y = 0, size = 5, parse=TRUE)
  #theme_ipsum()
  theme_light() +
  theme(text = element_text(size=15))
### Regression values
get_rsq<-function(seq, mod, df=df.sels) {
  dt<-df%>%filter(sequence==seq)%>%select(learningTaskId, trial, selection, freq, pred=!!mod)
  return(summary(lm(data=dt, freq~pred))$r.squared)
}
get_rsq('reverse','exp')






