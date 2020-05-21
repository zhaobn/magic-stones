
options("scipen" = 10)
options()$scipen

library(dplyr)
# Get global settings and helper functions from
# mdl.R

# Construct effects from data
parse_effects<-function(data) {
  effects_per_feature<-function(f) {
    desc<-c()
    f_idx<-if (f=='color') 1 else 2
    f_v<-substr(f, 1, 1)
    af<-substr(data[['A']], f_idx, f_idx)
    rf<-substr(data[['R']], f_idx, f_idx)
    tf<-substr(data[['T']], f_idx, f_idx)
    # Effects compatible with available data
    desc<-c(desc,paste0(f_v, '(T)=', f_v, tf))
    if (tf==rf) {
      desc<-c(desc, paste0(f_v, '(T)=', f_v, '(R)'))
    } else {
      desc<-c(desc, paste0(f_v, '(T)~', f_v, '(R)'))
    }
    if (tf==af) {
      desc<-c(desc, paste0(f_v, '(T)=', f_v, '(A)'))
    } else {
      desc<-c(desc, paste0(f_v, '(T)~', f_v, '(A)'))
    }
    return(desc)
  }
  
  feature_effects<-list()
  for (f in names(features)) feature_effects[[f]]<-effects_per_feature(f)
  
  effects<-c(feature_effects[['color']], feature_effects[['shape']])
  for (c in feature_effects[['color']]) {
    for (s in feature_effects[['shape']]) {
      effects<-c(effects, paste0(c, ',', s))
    }
  }
  return(effects)
}

# Construct conditions from data
parse_conditions<-function(data) {
  condition_per_feature<-function(f) {
    desc<-c()
    f_idx<-if (f=='color') 1 else 2
    f_v<-substr(f, 1, 1)
    af<-substr(data[['A']], f_idx, f_idx)
    rf<-substr(data[['R']], f_idx, f_idx)
    # Effects compatible with available data
    desc<-c(desc,c(paste0(f_v, '(A)=', f_v, af),
                   paste0(f_v, '(R)=', f_v, rf)))
    if (af==rf) {
      desc<-c(desc, paste0(f_v, '(A)=', f_v, '(R)'))
    } else {
      desc<-c(desc, paste0(f_v, '(A)~', f_v, '(R)'))
    }
    return(desc)
  }
  
  conditions<-c()
  for (f in names(features)) {
    conditions<-c(conditions, condition_per_feature(f))
  }
  return(conditions)
}

# Simulations
pred_cond<-function(cid, comp, learn_src=df.learn_tasks, gen_src=df.gen_trials) {
  data<-as.list(learn_src%>%filter(learningTaskId==cid)%>%select(A,R,T))
  ## Get working hypos
  effects<-parse_effects(data)
  conditions<-parse_conditions(data)
  hypos<-c(effects)
  for (e in effects) {
    for (c in conditions) {
      hypos<-c(hypos,paste0(e,'[',c,']'))
    }
  }
  df<-data.frame(hypos)
  colnames(df)<-c('hypo')
  df$hypo<-as.character(df$hypo)
  ## Get complexity
  df[,comp]<-mapply(get_comp, df$hypo, rep(comp, length(df$hypo)))
  df[,cid]<-mapply(check_hypo, df$hypo, rep(flatten(data), length(df$hypo)))

  ## Gen generalization predictions
  pred<-get_pred(cid, 1, comp, df, gen_src)
  for (i in 2:15) pred<-rbind(pred, get_pred(cid, i, comp, df))
  return(pred)
}
df.dd_sim<-pred_cond('learn01', 'gen')
for (i in 2:6) df.dd_sim<-rbind(df.dd_sim, pred_cond(paste0('learn0', i), 'gen'))
for (i in 1:6) df.dd_sim<-rbind(df.dd_sim, pred_cond(paste0('learn0', i), 'exp'))
df.dd_sim$learningTaskId<-as.character(df.dd_sim$learningTaskId)
df.dd_sim$stone<-as.character(df.dd_sim$stone)

x<-df.sels%>%select(learningTaskId, sequence, trial, selection, n, freq)
gen<-df.dd_sim%>%filter(comp=='gen')%>%select(learningTaskId, trial, selection=stone, gen=prob)
exp<-df.dd_sim%>%filter(comp=='exp')%>%select(learningTaskId, trial, selection=stone, exp=prob)

x<-x%>%left_join(gen, by=c('learningTaskId', 'trial', 'selection'))
x<-x%>%left_join(exp, by=c('learningTaskId', 'trial', 'selection'))

df.dd_sim<-x

# Plot results
df.dd_rsq<-get_rsq('learn01',1,'default','gen', df.dd_sim)
for (i in 1:6) {
  lid<-paste0('learn0', i)
  for (tid in 1:15) {
    for (comp in c('gen','exp')) {
      for (seq in c('default', 'reverse')) {
        if (!(i==1&tid==1&comp=='gen'&seq=='default')) {
          df.dd_rsq<-rbind(df.dd_rsq, get_rsq(lid, tid, seq, comp, df.dd_sim))
        }
      }
    }
  }
}
save(df.dd_sim, df.dd_rsq, file='dd.Rdata')


# Order effects
sample_more_hypos<-function(ldata, gdata) {
  is_color_change<-substr(ldata[['R']],1,1)!=substr(ldata[['T']],1,1)
  is_shape_change<-substr(ldata[['R']],2,2)!=substr(ldata[['T']],2,2)
  conditions<-parse_conditions(gdata)
  effects<-c()
  if (is_color_change) effects<-c(effects, color_effects)
  if (is_shape_change) effects<-c(effects, shape_effects)
  hypos<-c()
  for (e in effects) {
    for (c in conditions) {
      hypos<-c(hypos, paste0(e, '[', c, ']'))
    }
  }
  return(hypos)
}

od_pred_cond<-function(cid, comp, learn_src=df.learn_tasks, gen_src=df.gen_trials) {
  data<-as.list(learn_src%>%filter(learningTaskId==cid)%>%select(A,R,T))
  ## Get working hypos
  effects<-parse_effects(data)
  conditions<-parse_conditions(data)
  hypos<-c(effects)
  for (e in effects) {
    for (c in conditions) {
      hypos<-c(hypos,paste0(e,'[',c,']'))
    }
  }
  ## Get more hypos
  first_gen<-as.list(df.gen_trials%>%filter(learningTaskId==cid&trial==15)%>%select(A,R))
  extra_hypos<-sample_more_hypos(data, first_gen)
  cond_hypos<-c(); for (h in hypos) if (grepl('\\[', h)) cond_hypos<-c(cond_hypos, h)
  hypos<-setdiff(hypos, cond_hypos)
  for (c in cond_hypos) {
    for (x in extra_hypos) {
      hypos<-c(hypos, paste0(c, '|', x))
    }
  }
  ## Formatting  
  df<-data.frame(hypos)
  colnames(df)<-c('hypo')
  df$hypo<-as.character(df$hypo)
  ## Get complexity
  df[,comp]<-mapply(get_comp, df$hypo, rep(comp, length(df$hypo)))
  df[,cid]<-mapply(check_hypo, df$hypo, rep(flatten(data), length(df$hypo)))
  
  ## Gen generalization predictions
  pred<-get_pred(cid, 1, comp, df, gen_src)
  for (i in 2:15) pred<-rbind(pred, get_pred(cid, i, comp, df))
  return(pred)
}
## Simulation results
x<-od_pred_cond('learn01', 'exp')
for (i in 2:6) x<-rbind(x, od_pred_cond(paste0('learn0', i), 'exp'))
for (i in 1:6) x<-rbind(x, od_pred_cond(paste0('learn0', i), 'gen'))
df.od<-x
df.od$learningTaskId<-as.character(df.od$learningTaskId)
df.od$stone<-as.character(df.od$stone)

df.od_sim<-df.dd_sim%>%select(learningTaskId, sequence, trial, selection, n, freq)
gen<-df.od%>%filter(comp=='gen')%>%select(learningTaskId, trial, selection=stone, gen=prob)
df.od_sim<-df.od_sim%>%left_join(gen, by=c('learningTaskId', 'trial', 'selection'))
exp<-df.od%>%filter(comp=='exp')%>%select(learningTaskId, trial, selection=stone, exp=prob)
df.od_sim<-df.od_sim%>%left_join(exp, by=c('learningTaskId', 'trial', 'selection'))

save(df.od_sim, df.dd_sim, df.dd_rsq, file='dd.Rdata')

# Overall regression stats
overall_rsg<-function(comp, seq, src) {
  ppt<-src%>%filter(sequence==seq)%>%select(learningTaskId, trial, selection, freq)
  pred<-src%>%filter(sequence==seq)%>%select(learningTaskId, trial, selection, prob=!!comp)
  dt<-ppt%>%left_join(pred, by=c('learningTaskId', 'trial', 'selection'))
  return(summary(lm(data=dt, freq~prob))$r.squared)
}
overall_rsg('gen', 'default', df.od_sim)
overall_rsg('exp', 'default', df.od_sim)
overall_rsg('gen', 'reverse', df.od_sim)
overall_rsg('exp', 'reverse', df.od_sim)



# May 21, new attempt
parse_hypo<-function(data) {
  parse_hypo_per_feature<-function(feature, data){
    hypos<-c()
    read_feature_value<-function(stone) {
      f_idx<-if (feature=='color') 1 else 2
      return(substr(stone, f_idx, f_idx))
    }
    f<-substr(feature, 1, 1)
    if (read_feature_value(data[['T']])==read_feature_value(data[['R']])) {
      hypos<-c(hypos, paste(c(f, '(T)', '=', f, '(R)'), collapse=''))
    } else {
      hypos<-c(hypos, paste(c(f, '(T)', '=', f, read_feature_value(data[['T']])), collapse=''))
      if (read_feature_value(data[['T']])==read_feature_value(data[['A']])) {
        hypos<-c(hypos, paste(c(f, '(T)', '=', f, '(A)'), collapse=''))
      } else {
        hypos<-c(hypos, paste(c(f, '(T)', '~', f, '(A)'), collapse=''))
        hypos<-c(hypos, paste(c(f, '(T)', '~', f, '(R)'), collapse=''))
      }
    }
    return(hypos)
  }
  hypos<-list()
  for (feature in names(features)) {
    hypos[[feature]]<-parse_hypo_per_feature(feature, data)
  }
  full_hypo<-c()
  for (c in hypos[['color']]) {
    for (s in hypos[['shape']]) {
      full_hypo<-c(full_hypo, paste0(c,',',s))
    }
  }
  return(full_hypo)
}
pred_cond<-function(cid, comp, learn_src=df.learn_tasks, gen_src=df.gen_trials) {
  data<-as.list(learn_src%>%filter(learningTaskId==cid)%>%select(A,R,T))
  df<-data.frame(parse_hypo(data))
  colnames(df)<-'hypo'; df$hypo<-as.character(df$hypo)
  df[,comp]<-mapply(get_comp, df$hypo, rep(comp, length(df$hypo)))
  df[,cid]<-mapply(check_hypo, df$hypo, rep(flatten(data), length(df$hypo)))
  ## Gen generalization predictions
  pred<-get_pred(cid, 1, comp, df, gen_src)
  for (i in 2:15) pred<-rbind(pred, get_pred(cid, i, comp, df))
  return(pred)
}

df.min<-pred_cond('learn01', 'gen')
for (i in 2:6) df.min<-rbind(df.min, pred_cond(paste0('learn0', i), 'gen'))

df.dd_sim<-append_preds('min', df.min, df.dd_sim)
df.dd_rsq<-rbind(df.dd_rsq, get_comp_rsq('min', df.dd_sim))

save(df.dd_sim, df.dd_rsq, file='dd.Rdata')

x<-fmt_heatmap(df.dd_rsq)
plot_heatmap(x)

y<-fmt_rg(df.dd_sim)
plot_rg('min', y)


