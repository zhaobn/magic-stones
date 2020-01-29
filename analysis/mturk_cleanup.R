options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

## Load data
load('../data/mturk_20200112_reverse.Rdata')

## Fix learningTaskId bug
learning_tasks <- df.sw[,c(1,9)]
df.tw <- df.tw %>% left_join(learning_tasks, by = 'ix')

miss_labeled <- df.tw %>% 
  filter(learningTaskId=='learn02' & agent=='bs') %>%
  select(ix) %>% distinct()

fix_data <- function(df) {
  batch_fixed <- df %>%
    filter(ix %in% miss_labeled[[1]]) %>%
    mutate(learningTaskId='learn03') 
  
  batch_orig <- df %>% filter(!(ix %in% miss_labeled[[1]]))
  
  return(rbind(batch_fixed, batch_orig) %>% arrange(ix))
}

df.sw <- fix_data(df.sw)
df.tw <- fix_data(df.tw)

# Drop some learn03 to keep balance
to_drop <- df.sw %>%
  filter(learningTaskId=='learn03') %>%
  select(ix) %>% distinct() %>%
  arrange(desc(ix)) %>%
  top_n(9)

df.sw <- df.sw %>% filter(!(ix %in% to_drop[[1]]))
df.tw <- df.tw %>% filter(!(ix %in% to_drop[[1]]))

## Append learning info to trials
learning_tasks<-df.sw %>% select(ix, learningTaskId)
df.tw <- df.tw %>%left_join(learning_tasks, by='ix')
df.tw <- df.tw %>% 
  mutate(learn_agent = case_when(learningTaskId == 'learn01' ~ 'rs', 
                                 learningTaskId == 'learn02' ~ 'yd', 
                                 learningTaskId == 'learn03' ~ 'bs', 
                                 learningTaskId == 'learn04' ~ 'rc', 
                                 learningTaskId == 'learn05' ~ 'yd', 
                                 learningTaskId == 'learn06' ~ 'bs',
                                 learningTaskId == 'learn07' ~ 'rd',)) %>%
  mutate(learn_recipient = case_when(learningTaskId == 'learn01' ~ 'yc', 
                                     learningTaskId == 'learn02' ~ 'rs',
                                     learningTaskId == 'learn03' ~ 'rd', 
                                     learningTaskId == 'learn04' ~ 'bs',
                                     learningTaskId == 'learn05' ~ 'bs', 
                                     learningTaskId == 'learn06' ~ 'yc',
                                     learningTaskId == 'learn07' ~ 'ys',)) %>%
  mutate(learn_rule = case_when(learningTaskId == 'learn01' ~ '-2s', 
                                learningTaskId == 'learn02' ~ '-2c',
                                learningTaskId == 'learn03' ~ '-2b', 
                                learningTaskId == 'learn04' ~ '-2y',
                                learningTaskId == 'learn05' ~ '-2y, -2c', 
                                learningTaskId == 'learn06' ~ '-2b, -2s',
                                learningTaskId == 'learn06' ~ '-2b, -2c',)) 

## Look a bit nicer
df.sw <- df.sw %>% 
  select(ix, learningTaskId, date, time, instructions_duration, task_duration,
         age, sex, engagement, difficulty, guess, feedback, id, token) %>% 
  arrange(ix)
df.tw <- df.tw %>% 
  select(ix, learningTaskId, learn_agent, learn_recipient, learn_rule,
         trial, agent, recipient, selection, ts, id) %>% 
  arrange(ix)

## Fix trial order
# Read ordered trials
library("rjson")
load('../data/mturk_20200107.Rdata')
trialsFile <- fromJSON(file='../data/viz/trials.json')
ordered_trials <- data.frame(trialsFile)
ordered_trials$learningTaskId <-as.character(ordered_trials$learningTaskId)
ordered_trials$trial <- as.numeric(as.character(ordered_trials$trial))
ordered_trials$agent <- as.character(ordered_trials$agent)
ordered_trials$recipient <- as.character(ordered_trials$recipient)
df.tw$agent <- as.character(df.tw$agent)
df.tw$recipient <- as.character(df.tw$recipient)
df.tw$learningTaskId <- as.character(df.tw$learningTaskId)

df.tw <- df.tw %>% 
  left_join(ordered_trials, by = c('learningTaskId', 'agent', 'recipient')) %>%
  select(ix, learningTaskId, learn_agent, learn_recipient, learn_rule, 
         trial.y, agent, recipient, selection, ts, id) %>% 
  rename(trial=trial.y) %>%
  arrange(ix, trial) 

## Save data
save(file='../data/mturk_20200109.Rdata', df.sw, df.tw)

## Prep viz data
# Subjects per learning task condition
export <- df.tw %>% select(learningTaskId, ix) %>% 
  distinct() %>%
  arrange(learningTaskId, ix)
showIx <- function(cond) {
  dt <- export %>% filter(learningTaskId==cond)
  dt$ix
}
showIx('learn07')

# Subject data
ixes <- df.tw %>% select(ix) %>% distinct()
read_selection <- function(x) {
  sel <- df.tw %>% 
    filter(ix==x) %>% arrange(trial) %>% select(selection)
  return(sel)
}
data <-read_selection(ixes[[1]][1])
for (i in 2:length(ixes[[1]])) {
  data <- cbind(data, read_selection(ixes[[1]][i]))
}
names(data) <- ixes[[1]]
data <- t(data)
write.csv(data, file = '../data/rev_selections.csv')

# Check how many needed to get 10 per group
p.sw<-df.sw
p.tw<-df.tw
load('../data/processed/mturk_20191128_trial_fixed.Rdata')
ex<-df.sw %>% select(ix, learningTaskId)
df<-p.sw %>% select(ix, learningTaskId)
groups<-rbind(ex,df)%>%group_by(learningTaskId)%>%tally()

# Combine all batches
pt.sw<-df.sw
pt.tw<-df.tw
df.sw<-rbind(df.sw, pt.sw)
df.tw<-rbind(df.tw, pt.tw)
save(df.sw, df.tw, file='../data/mturk_20200108_combo.Rdata')

# Combo test
df.tw <- df.tw %>%
  mutate(learningTaskId='learn07', learn_agent='rd', learn_recipient='ys', learn_rule='-2b, -2c') %>%
  select(ix, learningTaskId, learn_agent, learn_recipient, learn_rule, 
         trial, agent, recipient, selection, ts, id)


# Clean up for cogsci
rename_df<-function(data) {
  data<-data%>%filter(learningTaskId!='learn05')
  data$learningTaskId<-as.character(data$learningTaskId)
  data<-data%>%mutate(new_learningTaskId=case_when(
    learningTaskId=='learn06'~'learn05',
    learningTaskId=='learn07'~'learn06',
    TRUE~learningTaskId
  ))
  return(data)
}

pred2<-rename_df(df.w2)
names(pred2)
pred2<-pred2%>%select(learningTaskId=new_learningTaskId, trial, selection, wc_pp=prob)%>%
  mutate(sequence='reverse')
pd<-rbind(pred, pred2)

save(df.sw, df.tw, df.freq, df.tasks, file='cogsci_20200127.Rdata')















