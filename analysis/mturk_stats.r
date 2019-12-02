options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
rm(list=ls())

# Load data
load('../data/mturk_20191128_fixed.Rdata')

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
# min: 24 | max: 64 | mean: 41.0882 | sd: 11.4452 

# Sex
summarise_group(df.sw$sex)
# total 34 | male 17 50% | female 17 50%
genders <- unique(df.sw$sex);
for (i in 1:length(genders)) {
  gender_group <- df.sw %>% filter(sex==genders[i]);
  cat(genders[i], ' age\n');
  report_col(gender_group$age);
}
# Male age: min: 24 | max: 58 | mean: 41 | sd: 11.9739 
# Female age: min: 25 | max: 64 | mean: 41.1765 | sd: 11.2596 

# Task duration
report_col(df.sw$task_duration)
# min: 132781 | max: 2322982 | mean: 360908.5 | sd: 362532.1 

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
df.tw <- df.tw %>%
  mutate(sel_col=case_when(
    substr(df.tw$selection, 1, 1) == substr(df.tw$agent, 1, 1) ~ 'a',
    substr(df.tw$selection, 1, 1) == substr(df.tw$recipient, 1, 1) ~ 'r',
    TRUE ~ 'd')) %>%
  mutate(sel_shp=case_when(
    substr(df.tw$selection, 2, 2) == substr(df.tw$agent, 2, 2) ~ 'a',
    substr(df.tw$selection, 2, 2) == substr(df.tw$recipient, 2, 2) ~ 'r',
    TRUE ~ 'd')) %>%
  mutate(sel_label=paste0(sel_col, sel_shp))

ggplot(df.tw, aes(sel_label)) + geom_bar()
ggplot(df.tw, aes(sel_label)) + geom_bar(aes(fill=learningTaskId))



