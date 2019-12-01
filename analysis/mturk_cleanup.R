options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

## Load data
load('../data/mturk_20191128.Rdata')

## Fix learningTaskId bug
learning_tasks <- df.sw[,c(1,9)]
df.tw <- df.tw %>% left_join(learning_tasks, by = 'ix')

miss_labeled <- df.tw %>% 
  filter(learningTaskId=='learn02' & agent=='bs') %>%
  select(ix) %>% distinct()

batch_fixed <- df.sw %>%
  filter(ix %in% miss_labeled[[1]]) %>%
  mutate(learningTaskId='learn03') %>%
  top_n(6) # Drop some learn03 groups to keep balance

batch_orig <- df.sw %>% filter(!(ix %in% miss_labeled[[1]]))
df.sw <- rbind(batch_fixed, batch_orig) %>% arrange(ix)

kept_ix <- df.sw %>% select(ix) %>% distinct()
df.tw <- df.tw %>% filter(ix %in% kept_ix[[1]])

## Append learning info to trials
df.tw <- df.tw %>% 
  mutate(learn_agent = case_when(learningTaskId == 'learn01' ~ 'rs', learningTaskId == 'learn02' ~ 'yd', 
                                 learningTaskId == 'learn03' ~ 'bs', learningTaskId == 'learn04' ~ 'rc', 
                                 learningTaskId == 'learn05' ~ 'yd', learningTaskId == 'learn06' ~ 'bs')) %>%
  mutate(learn_recipient = case_when(learningTaskId == 'learn01' ~ 'yc', learningTaskId == 'learn02' ~ 'rs',
                                     learningTaskId == 'learn03' ~ 'rd', learningTaskId == 'learn04' ~ 'bs',
                                     learningTaskId == 'learn05' ~ 'bs', learningTaskId == 'learn06' ~ 'yc',)) %>%
  mutate(learn_rule = case_when(learningTaskId == 'learn01' ~ '-2s', learningTaskId == 'learn02' ~ '-2c',
                                learningTaskId == 'learn03' ~ '-2b', learningTaskId == 'learn04' ~ '-2y',
                                learningTaskId == 'learn05' ~ '-2y, -2c', learningTaskId == 'learn06' ~ '-2b, -2s',)) 

## Look a bit nicer
df.sw <- df.sw %>% 
  select(ix, learningTaskId, date, time, instructions_duration, task_duration,
         age, sex, engagement, difficulty, guess, feedback, id, token) %>% 
  arrange(ix)
df.tw <- df.tw %>% 
  select(ix, learningTaskId, learn_agent, learn_recipient, learn_rule, 
         trial, agent, recipient, selection, ts, id) %>% 
  arrange(ix)

## Save data
save(file='../data/mturk_20191128_fixed.Rdata', df.sw, df.tw)

