
source("./functions.R")
load("../behavioral_data/tasks.Rdata")
load("../behavioral_data/aggregated.Rdata")

# 2020-07-16 Santity check of normative models ####
# Crp-normative with alpha=0 should equivalent to uni-normative
fsize=3.83

df<-get_cond_pred(1, fsize, 0)
for (i in 2:6) df<-rbind(df, get_cond_pred(i, fsize, 0))
uni_normative<-df

crp_normative<-get_crp_norm_pred(0.1, 0, fsize, 0)

# Compare uni-normative and crp-normative
crp_cut<-crp_normative%>%filter(condition=='near')%>%select(learningTaskId, trial, pred, crp=prob)
df<-uni_normative%>%left_join(crp_cut, by=c('learningTaskId', 'trial', 'pred'))
df%>%filter(prob!=crp) # 0
########


# 2020-07-14, optim() results for crp moddels ####
# Have a look at preliminary fitting results
fit_12<-out # 0.21, -0.004
fit_16<-out # 0.10, 0.10
fit_17<-out # 0.12, 0.09
fit_18<-out # 0.11, 0.10
# Not going anywhere, consider we start from 0.1, 0.1
# Try grid-search optimizations
########

# 2020-07-24: grid search results for crp-normative model ####
# Take a look at results
a<-ggplot(fits, aes(mu, alpha, fill=likeli)) + geom_tile() + scale_fill_viridis(option='magma')

sub<-fits%>%filter(mu>0.5&alpha<0.5)
b<-ggplot(sub, aes(mu, alpha, fill=likeli)) + geom_tile() + scale_fill_viridis(option='magma')

micro<-sub%>%filter(alpha<0.1)
c<-ggplot(micro, aes(mu, alpha, fill=likeli)) + geom_tile() + scale_fill_viridis(option='magma')

library(ggpubr)
ggarrange(a,b,c, nrow=1, legend='bottom')

# Compare 0.06 with 0.05, 0.07
more_fits<-function(mus, alphas) {
  data<-data.frame(mu=numeric(0), alpha=numeric(0), likeli=numeric(0))
  for (m in mus) {
    for (a in alphas) {
      df<-generate_pred(m, a)
      likeli<-sum(log(df$prob)*ppt_data$n)
      data<-rbind(data, data.frame(mu=m,alpha=a,likeli=likeli))
    }
  }
  return(data)
}
alphas<-c(0.05,0.06,0.07)
mus<-c(0.99, 1)
extra<-more_fits(mus, alphas)
save(extra, file='extra_fits_crpnorm.Rdata')

# Try increasing alpha
alphas<-c(0.06); mus<-seq(2, 5)
extra<-rbind(extra, more_fits(mus, alphas)); save(extra, file='extra_fits_crpnorm.Rdata')

mus<-c(10, 20, 30, 50, 80)
extra<-rbind(extra, more_fits(mus, alphas)); save(extra, file='extra_fits_crpnorm.Rdata')

mus<-c(100, 500, 1000, 2000, 5000, 10000, 12000, 13000, 14000, 15000, 20000, 30000)
extra<-rbind(extra, more_fits(mus, alphas)); save(extra, file='extra_fits_crpnorm.Rdata')

df<-extra%>%filter(alpha==0.06)
ggplot(df, aes(x=mu, y=likeli)) + geom_line() + 
  scale_x_continuous(trans='log10') + 
  labs(x='mu', y='log-likelihood') + 
  theme_bw() + 
  geom_vline(xintercept = 1000, linetype='dashed', color="orange")


# Plot P-yes for mu=15000, alpha=0.06
calc_all_pyes<-function(alpha=0.06, mu=15000, beta=3.83) {
  pyes_per_cond<-function(lid, alpha, mu, beta) {
    pyes_per_trial<-function(tid) {
      td<-read_task(tasks[tid])
      p_cat<-stone_likeli(td, cat, 'A')*cat_prior(cat, cats, mu, alpha, 'A', F)
      p_new<-stone_likeli(td, init_cat(mu), 'A')*cat_prior(init_cat(mu), cats, mu, alpha, 'A', T)
      p_yes<-p_cat/(p_cat+p_new)
      return(data.frame(learningTaskId=paste0('learn0',lid), trial=tid, p_yes=p_yes))
    }
    # prep data
    ld<-as.list(df.learn_tasks[lid, c(2:4)])
    tasks<-tasks_from_df(lid)
    cat<-init_cat(mu)+count_feats(ld, 'A') # learning's category
    cats<-list(); cats[[1]]<-cat
    # get p_yes for each trial
    df<-pyes_per_trial(1)
    for (i in 2:length(tasks)) df<-rbind(df, pyes_per_trial(i))
    return(df)
  }
  df<-pyes_per_cond(1)
  for (i in 2:6) df<-rbind(df, pyes_per_cond(i))
  return(df)
}

a<-(pyes_per_cond(1, 0.06, 1000))%>%mutate(params='D:fitted (alpha=0.06, mu=1000)')
b<-(pyes_per_cond(1, 0.1, 0.1))%>%mutate(params='A: alpha=mu=0.1')
c<-(pyes_per_cond(1, 0.8, 0.1))%>%mutate(params='C: alpha=0.8, mu=0.1')
d<-(pyes_per_cond(1, 0.1, 2))%>%mutate(params='B: alpha=0.1, mu=2')
df<-rbind(a, b, c, d)

ggplot(df, aes(x=trial, y=p_yes, shape=params, color=params)) + 
  geom_line() + geom_point(size=4) +
  theme_bw() + scale_color_brewer(palette="Set2") + 
  scale_x_discrete(limits=seq(15)) +
  theme(legend.position="bottom")

# Fix a very large mu, how does P_yes change with alpha?
ld<-as.list(df.learn_tasks[1,c(2:4)])
td<-read_task(tasks_from_df(1)[1])
mu<-10000
cat<-init_cat(mu)+count_feats(ld, 'A') # learning's category
cats<-list(); cats[[1]]<-cat

get_p_yes_per_alpha<-function(alpha) {
  p_cat<-stone_likeli(td, cat, 'A')*cat_prior(cat, cats, mu, alpha, 'A', F)
  p_new<-stone_likeli(td, init_cat(mu), 'A')*cat_prior(init_cat(mu), cats, mu, alpha, 'A', T)
  p_yes<-p_cat/(p_cat+p_new)
  return(data.frame(alpha=alpha, p_yes=p_yes))
}

alphas<-seq(0.01, 1, by=0.01)
df<-get_p_yes_per_alpha(alphas[1])
for (i in 2:length(alphas)) df<-rbind(df, get_p_yes_per_alpha(alphas[i]))
ggplot(df, aes(x=alpha,y=p_yes))+geom_line() + theme_bw() + ylab('P(same category)')



















