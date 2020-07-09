library(dplyr)
library(ggplot2)
library(viridis)
#library(ggpubr)

# Raw selection plot ####
ppt_near<-df.sels%>%filter(sequence=='default')%>%
  mutate(data='ppt_near', learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%
  select(learn_cond, trial, selection, prob=freq, data)
ppt_far<-df.sels%>%filter(sequence=='reverse')%>%
  mutate(data='ppt_far', learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%
  select(learn_cond, trial, selection, prob=freq, data)
normative<-df.norm%>%
  mutate(data='normative', learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%
  select(learn_cond, trial, selection=pred, prob, data)
sim_near<-df.sim%>%filter(condition=='near')%>%
  mutate(data='sim_near', learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%
  select(learn_cond, trial, selection=pred, prob=freq, data)
sim_far<-df.sim%>%filter(condition=='far')%>%
  mutate(data='sim_far', learn_cond=paste0('L', substr(learningTaskId,7,7)))%>%
  select(learn_cond, trial, selection=pred, prob=freq, data)

df<-rbind(ppt_near, ppt_far, normative, sim_near, sim_far)
df$data<-factor(df$data, levels=c('normative', 'ppt_near', 'sim_near', 'ppt_far', 'sim_far'))

a<-ggplot(df, aes(x=selection, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) +
  scale_fill_viridis(option="E", direction=-1) +
  facet_grid(data~learn_cond) +
  theme(#legend.position='bottom', 
    strip.text=element_text(size=20),
    axis.title=element_text(size=20),
    axis.text=element_text(size=10),
  )
a

# Sensitivity analysis ####
library(GA)
x<-c(seq(0.01,0.1,by=0.01), seq(0.2,1,by=0.1))
y<-c(seq(0.01,0.1,by=0.01), seq(0.2,1,by=0.1))
m<-matrix(unlist(read.table('data/output.txt')), ncol=length(y))


par(mfrow=c(1,2))
persp3D(x, y, m, theta=-30, phi=15, expand=1, col.palette=topo.colors,
        xlab="mu", ylab="alpha", zlab="", cex.lab=2.5)
persp3D(x, y, m, theta=120, phi=15, expand=1, col.palette=topo.colors,
        xlab="mu", ylab="alpha", zlab="", cex.lab=2.5)


# Model comparison ####
norm<-rbind(df.norm%>%mutate(sequence='near'), df.norm%>%mutate(sequence='far'))
rand<-norm%>%mutate(prob=1/9, data='random')

eps<-.Machine$double.eps
proc<-df.sim%>%select(learningTaskId, trial, pred, prob=freq, sequence=condition)%>%
  mutate(prob=if_else(prob==0, eps, prob))

ppt<-df.sels%>%filter(!(sequence=='combined'))%>%
  mutate(sequence=if_else(sequence=='default', 'near', 'far'))%>%
  select(learningTaskId, sequence, trial, pred=selection, n, freq)


get_likeli<-function(df, src=ppt) {
  combine<-src%>%left_join(df, by=c('learningTaskId', 'trial', 'pred', 'sequence'))
  return(sum(log(combine$prob)*combine$n))
}

get_bic<-function(df, nparam=0, src=ppt) {
  logli<-get_likeli(df, src)
  return(nparam*log(nrow(df))-2*logli)
}

get_rsq<-function(df, src=ppt) {
  combine<-src%>%left_join(df, by=c('learningTaskId', 'trial', 'pred', 'sequence'))
  return(summary(lm(data=combine, freq~prob))$adj.r.squared)
}

get_cov<-function(df, method="pearson", src=ppt) {
  combine<-src%>%left_join(df, by=c('learningTaskId', 'trial', 'pred', 'sequence'))
  return(cov(combine$prob, combine$freq, method=method))
}

get_likeli(rand) #-3955.004
get_likeli(norm) #-2598.727
get_likeli(proc) #-2608.816

get_bic(rand) #7910.008
get_bic(norm, 1) #5204.843
get_bic(proc, 3) #5239.802

get_rsq(rand) #0
get_rsq(norm) #0.612829
get_rsq(proc) #0.6143115

get_cov(rand) #0
get_cov(norm) #0.02807854
get_cov(proc) #0.02458963

get_cov(norm, 'spearman') #117944.1
get_cov(proc, 'spearman') #117084.1


# regression plots ####
ppts<-rbind(ppt_near, ppt_far)
ppts<-ppts%>%mutate(data=if_else(data=='ppt_near', 'near', 'far'))%>%
  select(learn_cond, trial, selection, ppt=prob, data)
norms<-rbind(normative%>%mutate(data='near'), normative%>%mutate(data='far'))%>%
  select(learn_cond, trial, selection, normative=prob, data)
sims<-(rbind(sim_far, sim_near))%>%mutate(data=if_else(data=='sim_near', 'near', 'far'))%>%
  select(learn_cond, trial, selection, process=prob, data)


norm_ppt<-ppts%>%left_join(norms, by=c('learn_cond', 'trial', 'selection', 'data'))

b<-ggplot(norm_ppt, aes(x=normative, y=ppt, color=learn_cond)) +
  geom_point(shape=3, size=1) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_light() + 
  ylab('participant') + xlab('normative model') +
  theme(
    legend.position='bottom',
    axis.title=element_text(size=20),
    axis.text=element_text(size=10),
  )
b

proc_ppt<-ppts%>%left_join(sims, by=c('learn_cond', 'trial', 'selection', 'data'))
c<-ggplot(proc_ppt, aes(x=process, y=ppt, color=learn_cond)) +
  geom_point(shape=3, size=1) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_light() +
  ylab('participant') + xlab('process model') +
  theme(
    legend.position='bottom',
    axis.title=element_text(size=20),
    axis.text=element_text(size=10),
  )
c

library(ggpubr)
ggarrange(b, c, common.legend=TRUE, nrow=2)

ggarrange(a, ggarrange(b, c, common.legend=TRUE, nrow=2, labels=c('B', 'C')), 
          ncol=2, widths=c(2.5,1), labels=c('A', ''))

summary(lm(data=norm_ppt, ppt~normative))$r.squared
summary(lm(data=proc_ppt, ppt~process))$r.squared


# eta plots #####








