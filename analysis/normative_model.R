
# Normative model according to normative_model_v2
library(dplyr)
rm(list=ls())

features<-list()
features[['color']]<-c('b', 'r', 'y')
features[['shape']]<-c('c', 'd', 's')

# Generate all hypothesis: (12 * 10) ^ 2 = 14,400
construct_atomic_hypo<-function(feature, type) {
  f<-substr(feature, 1, 1)
  if (type=='ante') {
    h<-c('A_', 'a_', 'T_', 't_', 'AT', 'at')
  } else if (type=='cons') {
    h<-c('R_', 'r_', 'RA', 'ra', 'RT', 'rt')
  }
  return(paste0(f, h))
}
construct_antes<-function(feature) {
  antes<-construct_atomic_hypo(feature, 'ante')
  ante_hypos<-list()
  ante_hypos[[1]]<-c(antes[1], antes[3])
  ante_hypos[[2]]<-c(antes[1], antes[4])
  ante_hypos[[3]]<-c(antes[2], antes[3])
  ante_hypos[[4]]<-c(antes[2], antes[4])
  for (i in 5:8) ante_hypos[[i]]<-c(ante_hypos[[i-4]], antes[[5]])
  for (i in 9:12) ante_hypos[[i]]<-c(ante_hypos[[i-8]], antes[[6]])
  return(ante_hypos)
}
construct_conse<-function(feature) {
  conses<-construct_atomic_hypo(feature, 'cons')
  conse_hypos<-list()
  for (i in 1:6) {
    conse_hypos[[i]]<-conses[i]
  }
  conse_hypos[[7]]<-c(conses[2], conses[4])
  conse_hypos[[8]]<-c(conses[2], conses[6])
  conse_hypos[[9]]<-c(conses[4], conses[6])
  conse_hypos[[10]]<-c(conses[2], conses[4], conses[6])
  return(conse_hypos)
}
construct_hypo<-function(feature){
  hypos<-list()
  antes<-construct_antes(feature)
  conse<-construct_conse(feature)
  for (i in 1:length(antes)) {
    for (j in 1:length(conse)) {
      hypos[[j+length(conse)*(i-1)]]<-c(antes[[i]], conse[[j]])
    }
  }
  return(hypos)
}
all_hypos<-list()
colors<-construct_hypo('color')
shapes<-construct_hypo('shape')
for (i in 1:length(colors)) {
  for (j in 1:length(shapes)) {
    all_hypos[[j+length(shapes)*(i-1)]]<-paste(c(colors[[i]], shapes[[j]]), collapse=',')
  }
}

# Generate all data points
stones<-vector()
for (i in 1:length(features[['color']])) {
  for (j in 1:length(features[['shape']])) {
    stones[j+length(features[['shape']])*(i-1)]<-paste0(features[['color']][i], features[['shape']][j])
  }
}
all_data_points<-list()
for (a in 1:length(stones)) {
  for (t in 1:length(stones)) {
    for (r in 1:length(stones)) {
      idx<-length(stones)^2*(a-1)+length(stones)*(t-1)+r
      all_data_points[[idx]]<-list('agent'=stones[a], 'target'=stones[t], 'result'=stones[r])
    }
  }
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

# For each hypothesis, get the number of datapoints that pass
get_n<-function(hypo) {
  passes<-0
  for (i in 1:length(all_data_points)) {
    passes<-passes+check_hypo(hypo, all_data_points[[i]])
  }
  return(passes)
}
hypo_base<-data.frame(hypo=all_hypos[[1]], n=get_n(all_hypos[[1]]))
for (i in 2:length(all_hypos)) {
  hypo_base<-rbind(hypo_base, data.frame(hypo=all_hypos[[i]], n=get_n(all_hypos[[i]])))
}
df.hypos<-hypo_base
save(df.hypos, file='normative_model.Rdata')

# Inference
# Prep training data
learnings<-list(
  'learn01'<-list('agent'='rs', 'target'='yc', 'result'='ys'),
  'learn02'<-list('agent'='yd', 'target'='rs', 'result'='rc'),
  'learn03'<-list('agent'='bs', 'target'='rd', 'result'='bd'),
  'learn04'<-list('agent'='rc', 'target'='bs', 'result'='ys'),
  'learn05'<-list('agent'='yd', 'target'='bs', 'result'='yc'),
  'learn06'<-list('agent'='bs', 'target'='yc', 'result'='bs'),
  'learn07'<-list('agent'='rd', 'target'='ys', 'result'='bc')
)

df.inf<-df.hypos
df.inf$hypo=as.character(df.inf$hypo)
df.inf$n=as.numeric(df.inf$n)

calc_post<-function(cond, df){
  yes<-paste0('yes_c', cond)
  likelihood<-paste0('li_c', cond)
  posterior<-paste0('post_c', cond)
  df[,yes]<-as.numeric(mapply(check_hypo, df[,'hypo'], paste(learnings[[cond]], collapse = ',')))
  df[,likelihood]<-df[,yes]/df[,'n']
  df[,posterior]<-df[,likelihood]/sum(df[,likelihood])
  return(df)
}

for (i in 1:length(learnings)) {
  df.inf<-calc_post(i, df.inf)
}
save(df.hypos, df.inf, file='normative_model.Rdata')

# Prediction
# Get trials
library("rjson")
trialsFile <- fromJSON(file='../data/viz/trials.json')
tasks <- data.frame(trialsFile)
tasks$learningTaskId <-as.character(tasks$learningTaskId)
tasks$trial <- as.numeric(as.character(tasks$trial))
tasks$agent <- as.character(tasks$agent)
tasks$recipient <- as.character(tasks$recipient)

# Get posterior predictives
get_post_pred<-function(cond, tid, pd=tasks, hd=df.inf) {
  post<-hd[,c('hypo', paste0('post_c', cond))]
  pt<-pd %>% filter(learningTaskId==paste0('learn0', cond)&trial==tid)
  pt<-bind_rows(replicate(length(stones), pt, simplify = FALSE))
  for (i in 1:length(stones)) {
    pt[i,'selection']<-stones[i]
    data<-paste(c(pt[i,'agent'], pt[i,'recipient'], stones[i]), collapse=',')
    post$yes<-as.numeric(mapply(check_hypo, post$hypo, data))
    pt[i,'raw_pp']<-sum(post[,2]*post$yes)
  }
  pt$pp<-pt$raw_pp/sum(pt$raw_pp)
  return(pt)
}
df.pred<-get_post_pred(1,1)
for (i in 1:length(learnings)) {
  for (j in 1:max(tasks$trial)) {
    if (!(i==1&j==1)) df.pred<-rbind(df.pred, get_post_pred(i,j))
  }
}
save(df.inf, df.pred, file='normative_model.Rdata')
