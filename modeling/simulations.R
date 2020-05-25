
options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
library(viridis)

# Customizable configs
features<-list()
features[['f']]<-paste0('f', seq(3))
features[['g']]<-paste0('g', seq(3))

relations<-c('=', '~')

obj_sep='-'
all_objs<-get_all_objs(features)

read_f<-function(feature, obj) {
  f_idx<-if (feature=='f') 1 else 2
  return(strsplit(obj, '-')[[1]][f_idx])
}

# Helper functions
abbr_feature<-function(features) {
  f_dict<-list()
  for (n in names(features)) {
    abbr<-substr(n, 1, 1)
    f_dict[[abbr]]<-n
  }
  return(f_dict)
}
get_all_objs<-function(features) {
  objs<-c()
  for (f in features[[1]]) {
    for (g in features[[2]]) {
      objs<-c(objs, paste0(f, obj_sep, g))
    }
  }
  return(objs)
}
normalize<-function(vec) {
  sum<-sum(vec); norm<-vec/sum; return(norm)
}
softmax<-function(vec, base=1) {
  v_exp<-exp(vec*base); sum<-sum(v_exp)
  return(v_exp/sum)
}
flatten<-function(list, sep=',') {
  str=c()
  for (i in 1:length(list)) str<-c(str, list[[i]])
  return(paste(str, collapse=sep))
}
to_list<-function(str, sep=',') {
  vecs<-strsplit(str, sep)[[1]]
  data<-list()
  data[['agent']]<-vecs[1]
  data[['recipient']]<-vecs[2]
  if (length(vecs) > 2) data[['result']]<-vecs[3]
  return(data)
}

# Non-causal baseline
## Single prediction
rand<-1/length(all_objs)
post_cat<-function(obs, alpha) {
  # Dirchilet
  post_cat_per_feature<-function(f, f_ob, alpha) {
    df<-data.frame(features[[f]]); colnames(df)<-c('f_val'); df$f_val<-as.character(df$f_val)
    df<-df%>%
      mutate(prior=alpha, obs=if_else(f_val==f_ob, 1, 0))%>%
      mutate(post=prior+obs)
    df$norm<-normalize(df$post)
    return(df%>%select(f_val, post=norm))
  }
  
  post<-data.frame(f_val=character(0),post=numeric(0))
  for (f in names(features)) {
    post<-rbind(post, post_cat_per_feature(f, read_f(f, obs), alpha))
  }
  
  post_pred<-c()
  for (o in all_objs) {
    f_post<-post%>%filter(f_val==read_f(names(features)[1], o))%>%select(post)
    g_post<-post%>%filter(f_val==read_f(names(features)[2], o))%>%select(post)
    post_pred<-c(post_pred, as.numeric(f_post*g_post))
  }
  
  df<-data.frame(obj=all_objs, post_pred); df$obj<-as.character(df$obj)
  return(df)
}

## Plots
non_causal<-post_cat(all_objs[1], 1)%>%mutate(alpha=1)
for (a in c(2,3,10)) non_causal<-rbind(non_causal,(post_cat(all_objs[1], a)%>%mutate(alpha=a)))
non_causal$alpha<-as.factor(non_causal$alpha)
ggplot(non_causal, aes(x=obj, y=post_pred, group=alpha, color=alpha)) + geom_line() + 
  geom_hline(yintercept=rand, linetype="dashed") +
  annotate(geom="text", label='random', x=9, y=rand, vjust=-.5)

## Predict for Agent-Recipient-Result data points
demo<-list()
demo[['agent']]<-'f1-g1'
demo[['recipient']]<-'f2-g2'
demo[['result']]<-'f1-g3'

get_trials<-function(data) {
  diff_fval<-function(feature, v1, v2) {
    return(setdiff(features[[feature]], c(v1, v2)))
  }
  af<-read_f(names(features)[1], data[['agent']])
  ag<-read_f(names(features)[2], data[['agent']])
  rf<-read_f(names(features)[1], data[['recipient']])
  rg<-read_f(names(features)[2], data[['recipient']])
  df<-sample(diff_fval(names(features)[1], af, rf), 1)
  dg<-sample(diff_fval(names(features)[2], ag, rg), 1)
  
  ta_f<-c(rep(af,3),rep(df,4),rep(af,4),rep(df,4))
  ta_g<-c(rep(ag,3),rep(ag,4),rep(dg,4),rep(dg,4))
  tr_f<-c(df, rep(c(rf,df),7))
  tr_g<-c(rg,dg,dg,rep(c(rg,rg,dg,dg),3))

  df<-data.frame(trial=seq(15), ta_f, ta_g, tr_f, tr_g)
  df<-df%>%
    mutate(agent=paste0(ta_f,obj_sep,ta_g), recipient=paste0(tr_f,obj_sep,tr_g))%>%
    mutate(task=paste0(agent, ',', recipient))%>%
    select(trial, task)
  return(df)
}

get_cat_preds<-function(data, alpha, noise=TRUE){
  df<-data.frame(task=character(0), pred=character(0), prob=numeric(0))
  pp<-post_cat(data[['result']], alpha)
  for (t in get_trials(data)$task) {
    for (o in pp$obj) {
      pf<-data.frame(task=t, pred=o, prob=pp$post_pred[pp$obj==o])
      if (noise==FALSE) df<-rbind(df, pf) else {
        pf$prob<-pf$prob+rnorm(1, 0.01, 0.01)
        df<-rbind(df, pf) 
      }
    }
  }
  return(df)
}
non_causal_demo<-get_cat_preds(demo, 1, T)

## Plot
plot_pred_hm<-function(data) {
  g<-ggplot(data, aes(pred, task, fill=prob)) + 
    geom_tile() + 
    scale_fill_gradient(low="white", high="black")
  return(g)
}
plot_pred_hm(non_causal_demo)

# Construct causal theories
compose_hypo<-function(data) {
  hypo_per_feature<-function(feature, data) {
    hypo<-c()
    f<-substr(feature, 1, 1)
    af<-read_f(feature, data[['agent']])
    rf<-read_f(feature, data[['recipient']])
    tf<-read_f(feature, data[['result']])
    if (tf==rf) {
      hypo<-c(hypo, paste(c(f,'(T)', '=', f, '(R)'), collapse=''))
    } else {
      if (tf==af) {
        hypo<-c(hypo, paste(c(f,'(T)', '=', f, '(A)'), collapse=''))
      } else {
        hypo<-c(hypo, c(paste(c(f,'(T)', '~', f, '(R)'), collapse=''),
                        paste(c(f,'(T)', '~', f, '(A)'), collapse='')))
      }
    }
    return(hypo)
  }
  hypo<-features
  hypo[[1]]<-hypo_per_feature(names(features)[1], data)
  hypo[[2]]<-hypo_per_feature(names(features)[2], data)
  hypos<-c()
  for (f in hypo[[1]]) {
    for (g in hypo[[2]]) {
      hypos<-c(hypos, paste0(f, ',', g))
    }
  }
  return(hypos)
}
get_causal_preds<-function(data, t, noise=FALSE) {
  get_pred_per_hypo<-function(task, hypo, t) {
    get_result_val<-function(feature, desc, task) {
      relation<-substr(desc, 5, 5)
      if (nchar(desc) < 8) {
        t_val<-substr(desc, 6, 6)
      } else {
        t_idx<-if (substr(desc, 8, 8)=='A') 1 else 2
        t_val<-read_f(feature, strsplit(task, ',')[[1]][t_idx])
      }
      if (relation=='=') return(t_val) else {
        return(setdiff(features[[feature]], t_val))
      }
    }
    
    f_dict<-abbr_feature(features)
    f_pred<-list()
    for (d in strsplit(hypo, ',')[[1]]) {
      f<-substr(d,1,1)
      f_pred[[f]]<-get_result_val(f_dict[[f]], d, task)
    }
    
    pred<-c()
    for (f in f_pred[[1]]) {
      for (g in f_pred[[2]]) {
        pred<-c(pred, paste0(f, obj_sep, g))
      }
    }
    
    df<-data.frame(obj=all_objs); df$obj<-as.character(df$obj)
    df$yes<-as.numeric(df$obj%in%pred)
    df$pp<-softmax(df$yes, t)
    return(df[,c('obj', 'pp')])
  }
  get_pred_per_task<-function(data, task, t) {
    hypo<-compose_hypo(data)
    df<-data.frame(obj=all_objs); df$obj<-as.character(df$obj)
    for (i in 1:length(hypo)) {
      hcol<-paste0('h_', i)
      x<-get_pred_per_hypo(task, hypo[i], t)
      df<-df%>%left_join(x, by='obj')%>%rename(!!hcol:=pp)
    }
    df<-df%>%mutate(task=task, pred=obj,
                    sum = rowSums(.[2:ncol(df)]))
    df$prob<-normalize(df$sum)
    return(df[,c('task', 'pred', 'prob')])
  }
  
  df<-data.frame(task=character(0), obj=character(0), prob=numeric(0))
  tasks<-get_trials(data)$task
  for (tk in tasks) {
    df<-rbind(df, get_pred_per_task(data, tk, t))
  }
  
  if (noise==FALSE) return(df) else {
    df$noise<-mapply(rnorm, 1, rep(0.01, length(df$pred)), rep(0.01, length(df$pred)))
    df$prob<-df$prob+df$noise
    return(df[,c('task', 'pred', 'prob')])
  }
}

## Plot
match_af<-demo; match_af[['result']]<-'f1-g2'
plot_pred_hm(get_causal_pred(match_af, 1, T))

match_diff<-demo; match_diff[['result']]<-'f2-g3'
plot_pred_hm(get_causal_pred(match_diff, 1, T))

all_diff<-demo; all_diff[['result']]<-'f3-g3'
plot_pred_hm(get_causal_pred(all_diff, 1, T))


# Full simulation results
get_obs_pred<-function(src, ob_cg, is_causal, noise=T, alpha=1, tpt=1) {
  od<-src; od[['result']]<-ob_cg[[1]]
  df<-if (is_causal) get_causal_pred(od, tpt, noise) else get_cat_preds(od, alpha, noise)
  df$type<-if (is_causal) 'causal' else 'non_causal'
  df$obs<-ob_cg[[1]]
  df$label<-names(ob_cg)
  return(df)
}

demo
ob_config<-list()
ob_config[['no_change']]='f2-g2'
ob_config[['same_as_agent']]='f1-g1'
ob_config[['one_match']]='f2-g1'
ob_config[['one_dif']]='f2-g3'
ob_config[['all_diff']]='f3-g3'

df.sim<-get_obs_pred(demo, ob_config[1], T)
for (i in 1:length(obs)) {
  for (t in c(T, F)) {
    if (!(i==1&t==T)) df.sim<-rbind(df.sim, get_obs_pred(demo, ob_config[i], t))
  }
}
df.sim$type<-factor(df.sim$type, levels=c('non_causal', 'causal'))
df.sim$label<-factor(df.sim$label, levels=names(ob_config))

save(df.sim, file='sim.Rdata')

ggplot(df.sim, aes(pred, task, fill=prob)) + geom_tile() + 
  scale_fill_viridis(option="E", direction=-1, end=0.7) +
  #scale_fill_gradient(low="white", high="black") +
  facet_grid(type~label)

# Random baseline
get_rand_preds<-function(data, noise=T) {
  n<-length(all_objs)
  df<-data.frame(task=character(0), pred=character(0), prob=numeric(0))
  for (t in get_trials(data)$task) {
    df<-rbind(df, data.frame(task=rep(t, n), pred=all_objs, prob=rep(1/n, n)))
  }
  if (noise==FALSE) return(df) else {
    nr<-length(df$task)
    df$noise<-mapply(rnorm, 1, rep(0.01, nr), rep(0.01, nr))
    df$prob<-df$prob+df$noise
    return(df[,c('task', 'pred', 'prob')])
  }
}
plot_pred_hm(get_rand_preds(demo, T))

# Try a normative approach
get_all_hypos<-function(features) {
  per_feature<-function(feature) {
    hypos<-c()
    f<-substr(feature, 1, 1)
    f_vals<-features[[feature]]
    obs<-paste0(f, c('(A)', '(R)'))
    for (r in relations) {
      for (obj in c(obs, f_vals)) {
        hypos<-c(hypos, paste0(f, '(T)', r, obj))
      }
    }
    return(hypos)
  }
  hypos<-features
  for (f in names(features)) {
    hypos[[f]]<-per_feature(f)
  }
  hypo<-c()
  for (f in hypos[[1]]) {
    for (g in hypos[[2]]) {
      hypo<-c(hypo, paste0(f,',',g))
    }
  }
  return(hypo)
}
all_hypos<-get_all_hypos(features)

get_learned_prior<-function(hypo, data) {
  task<-paste(strsplit(data, ',')[[1]][c(1,2)], collapse=',')
  result<-strsplit(data, ',')[[1]][3]
  post<-get_pred_per_hypo(task, hypo, 9)
  return(post[post$obj==result,'pp'])
}
get_norm_preds<-function(data, t, noise=FALSE) {
  dh<-data.frame(hypo=all_hypos)%>%mutate(hypo=as.character(hypo))
  dh$prior<-mapply(get_learned_prior, dh$hypo, rep(flatten(data), length(dh$hypo)))
  dh$prior<-normalize(dh$prior)

  get_norm_pred_per_task<-function(data, task, t) {
    df<-data.frame(obj=all_objs)%>%mutate(obj=as.character(obj))
    for (i in 1:length(dh$hypo)) {
      hcol<-paste0('h_', i)
      x<-get_pred_per_hypo(task, dh$hypo[i], t)
      x[,hcol]=x$pp*dh[i,'prior']
      df<-df%>%left_join((x[,c('obj', hcol)]), by='obj')
    }
    df<-df%>%mutate(task=task, pred=obj,
                    sum = rowSums(.[2:ncol(dp)]))%>%select(task, pred, sum)
    df$prob<-normalize(df$sum)
    return(df[,c('task', 'pred', 'prob')])
  }
  
  df<-data.frame(task=character(0), pred=character(0), prob=numeric(0))
  for (tk in trials$task) {
    df<-rbind(df, get_norm_pred_per_task(data, tk, t))
  }
  
  if (noise==FALSE) return(df) else {
    df$noise<-mapply(rnorm, 1, rep(0.01, length(df$pred)), rep(0.01, length(df$pred)))
    df$prob<-df$prob+df$noise
    return(df[,c('task', 'pred', 'prob')])
  }
}

plot_pred_hm(get_norm_preds(data, 6, T))






