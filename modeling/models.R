
library(tidyverse)

rm(list=ls())
load('../behavioral_data/aggregated.Rdata')

n<-df.sels %>% filter(sequence=='combined') %>% pull(n) %>% sum()

# Random baseline likelihood BIC
rand_ll<-log(1/9)*n
rand_BIC<-(-2)*rand_ll

# Universal model likelihood BIC
uni_st<-6.96
uni_ll<--2760.67
uni_BIC<-1*log(n)-2*uni_ll

# Normative DP model likelihood BIC


# Process model likelihood BIC
























