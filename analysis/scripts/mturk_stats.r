options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
rm(list=ls())

# Load data
load('../data/mturk_20191128_trial_fixed.Rdata')

# Ensure correct data type
df.sw$sex <- as.character(df.sw$sex)
df.sw$learningTaskId <- as.character(df.sw$learningTaskId)

df.sw$age <- as.numeric(as.character(df.sw$age))
df.sw$task_duration <- as.numeric(as.character(df.sw$task_duration))
df.sw$instructions_duration <- as.numeric(as.character(df.sw$instructions_duration))
df.sw$difficulty <- as.numeric(as.character(df.sw$difficulty))
df.sw$engagement <- as.numeric(as.character(df.sw$engagement))

# Helper functions
report_col <- function(list, ndigits=4, separator='|') {
  cat('min:', round(min(list), ndigits), separator,
      'max:', round(max(list), ndigits), separator,
      'mean:', round(mean(list), ndigits), separator,
      'sd:', round(sd(list), ndigits), '\n');
}

summarise_group <- function(list, ndigits=2, separator='|') {
  total <- length(list);
  summary <- paste('total', total);
  
  groups <- unique(list);
  for (i in 1:length(groups)) {
    count <- length(which(list==groups[i]));
    percent <- paste0(round(count/total*100, ndigits), '%');
    summary <- paste(summary, separator, groups[i], count, percent);
  }
  cat(summary)
}


# Subjectwise basic stats
# Age
report_col(df.sw$age)
# min: 24 | max: 64 | mean: 40.7647 | sd: 10.8685 

# Sex
summarise_group(df.sw$sex)
# total 34 | male 16 47.06% | female 18 52.94%
genders <- unique(df.sw$sex);
for (i in 1:length(genders)) {
  gender_group <- df.sw %>% filter(sex==genders[i]);
  cat(genders[i], ' age\n');
  report_col(gender_group$age);
}
# Male age: min: 24 | max: 58 | mean: 40.375 | sd: 12.0768 
# Female age: min: 27 | max: 64 | mean: 41.1111 | sd: 10.017 

# Task duration
report_col(df.sw$task_duration)
# min: 132781 | max: 2322982 | mean: 356366 | sd: 363682.9 

# Learn tasks
summarise_group(df.sw$learningTaskId)
#summarise_group(df.tw$learningTaskId)
# total 34 
# learn01 4 11.76% 
# learn02 5 14.71% 
# learn03 6 17.65%
# learn04 6 17.65% 
# learn05 7 20.59% 
# learn06 6 17.65% 

# Out of curisity
report_col(df.sw$difficulty) # min: 0 | max: 10 | mean: 4.8235 | sd: 2.9896 
report_col(df.sw$engagement) # min: 2 | max: 10 | mean: 8.5882 | sd: 2.0906 

# Take a peek of stats per learning condition groups
lg_shape <- df.sw %>% filter(learningTaskId %in% c('learn01', 'learn02')) # groups with a shape-changing rule
lg_color <- df.sw %>% filter(learningTaskId %in% c('learn03', 'learn04')) # groups with a color-changing rule
lg_object <- df.sw %>% filter(learningTaskId %in% c('learn05', 'learn06')) # groups with color & shape changing rules

nrow(lg_shape) # 9
nrow(lg_color) # 12
nrow(lg_object) # 13

report_col(lg_shape$task_duration)  # min: 218973 | max: 575022 | mean: 345779.4 | sd: 133882.9 
report_col(lg_color$task_duration)  # min: 157104 | max: 370915 | mean: 294222.9 | sd: 64136.33 
report_col(lg_object$task_duration) # min: 132781 | max: 2322982 | mean: 432938.5 | sd: 579199 

report_col(lg_shape$difficulty)  # min: 0 | max: 10 | mean: 4.8889 | sd: 3.2189 
report_col(lg_color$difficulty)  # min: 3 | max: 10 | mean: 5 | sd: 3.4112 
report_col(lg_object$difficulty) # min: 0 | max: 9 | mean: 4.6154 | sd: 2.6312 

lg_same <- df.sw %>% filter(learningTaskId %in% c('learn01', 'learn03', 'learn06')) # change to the same feature
lg_diff <- df.sw %>% filter(learningTaskId %in% c('learn02', 'learn04','learn05')) # change to a different feature

nrow(lg_same) # 16
nrow(lg_diff) # 18

report_col(lg_same$task_duration) # min: 183647 | max: 575022 | mean: 308112.8 | sd: 94710.8 
report_col(lg_diff$task_duration) # min: 132781 | max: 2322982 | mean: 407838.1 | sd: 492196.9 

report_col(lg_same$difficulty) # min: 0 | max: 10 | mean: 3.625 | sd: 3.2223 
report_col(lg_diff$difficulty) # min: 0 | max: 10 | mean: 5.8889 | sd: 2.3736 


# Take a look per learning task
check_task <- function(taskId) {
  ids <- df.sw %>% filter(learningTaskId == taskId) %>% select(id)
  trials <- df.tw %>% filter(id %in% ids[[1]])
  return(trials)
}
inspect <- check_task('learn01')
selections <- inspect %>% arrange(trial, selection)

# Label selections
add_sel_label <- function(dataframe) {
  df <- dataframe %>%
    mutate(sel_col=case_when(
      substr(dataframe$selection, 1, 1) == substr(dataframe$agent, 1, 1) ~ 'a',
      substr(dataframe$selection, 1, 1) == substr(dataframe$recipient, 1, 1) ~ 'r',
      TRUE ~ 'd')) %>%
    mutate(sel_shp=case_when(
      substr(dataframe$selection, 2, 2) == substr(dataframe$agent, 2, 2) ~ 'a',
      substr(dataframe$selection, 2, 2) == substr(dataframe$recipient, 2, 2) ~ 'r',
      TRUE ~ 'd')) %>%
    mutate(sel_label=paste0(sel_col, sel_shp))
  return(df)
}
df.tw <- add_sel_label(df.tw)
df.sim <- add_sel_label(df.sim)

# Plot selections
ggplot(df.tw, aes(sel_label)) + geom_bar(aes(fill=learningTaskId))
ggplot(df.tw, aes(as.numeric(as.character(trial)))) + geom_bar(aes(fill=sel_label))


# Take a look at free responses
df.fr <- df.sw %>% select(ix, learningTaskId, guess, id) %>% arrange(learningTaskId, ix)
# Append learning info to trials
df.fr <- df.fr %>% 
  mutate(learn_agent = case_when(learningTaskId == 'learn01' ~ 'rs', learningTaskId == 'learn02' ~ 'yd', 
                                 learningTaskId == 'learn03' ~ 'bs', learningTaskId == 'learn04' ~ 'rc', 
                                 learningTaskId == 'learn05' ~ 'yd', learningTaskId == 'learn06' ~ 'bs')) %>%
  mutate(learn_recipient = case_when(learningTaskId == 'learn01' ~ 'yc', learningTaskId == 'learn02' ~ 'rs',
                                     learningTaskId == 'learn03' ~ 'rd', learningTaskId == 'learn04' ~ 'bs',
                                     learningTaskId == 'learn05' ~ 'bs', learningTaskId == 'learn06' ~ 'yc',)) %>%
  mutate(learn_rule = case_when(learningTaskId == 'learn01' ~ '-2s', learningTaskId == 'learn02' ~ '-2c',
                                learningTaskId == 'learn03' ~ '-2b', learningTaskId == 'learn04' ~ '-2y',
                                learningTaskId == 'learn05' ~ '-2y, -2c', learningTaskId == 'learn06' ~ '-2b, -2s',)) %>%
  select(ix, learningTaskId, learn_agent, learn_recipient, learn_rule, guess, id) %>%
  arrange(learningTaskId, ix)
write.csv(df.fr, file = '../data/free_reponses.csv')

