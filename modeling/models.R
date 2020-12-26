
library(tidyverse)

rm(list=ls())
load('../../behavioral_data/aggregated.Rdata')

n<-df.sels %>% filter(sequence=='combined') %>% pull(n) %>% sum()

# Random baseline likelihood BIC
rand_ll<-log(1/9)*n
rand_BIC<-(-2)*rand_ll

# Universal model likelihood BIC
uni_st<-6.96
uni_ll<--2760.67
uni_BIC<-1*log(n)-2*uni_ll # 5529

# Normative DP model likelihood BIC
catg_ll<--2747.69
catg_BIC<-3*log(n)-2*catg_ll # 5518


4*log(1800)-2*(-2742.51)
3*log(1800)-2*(-2747.69)


# Process model likelihood BIC 
3*log(1800)-2*(-2736)
























