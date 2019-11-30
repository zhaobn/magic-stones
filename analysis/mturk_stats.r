options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

# Load data
load('../data/mturk_20191128.Rdata')

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
# min: 24 | max: 58 | mean: 38.8235 | sd: 9.9376

# Sex
summarise_group(df.sw$sex)
# total 34 | male 16 47.06% | female 18 52.94%
genders <- unique(df.sw$sex);
for (i in 1:length(genders)) {
  gender_group <- df.sw %>% filter(sex==genders[i]);
  cat(genders[i], ' age\n');
  report_col(gender_group$age);
}
# Male age: min: 24 | max: 58 | mean: 38.5 | sd: 11.4717
# Female age: min: 27 | max: 57 | mean: 39.1111 | sd: 8.6832

# Task duration
report_col(df.sw$task_duration)
# min: 132,781 | max: 2,322,982 | mean: 352,052.5 | sd: 365,263.9

# Learn tasks
summarise_group(df.sw$learningTaskId)
# total 34 | learn04 6 17.65% | learn06 6 17.65% | learn05 7 20.59% | learn01 2 5.88% | learn02 13 38.24%

# Out of curisity
report_col(df.sw$difficulty) # min: 0 | max: 10 | mean: 4.8529 | sd: 2.7428 
report_col(df.sw$engagement) # min: 2 | max: 10 | mean: 8.4706 | sd: 1.9577

# Take a peek of stats per learning condition groups
lg_shape <- df.sw %>% filter(learningTaskId=='learn01'|learningTaskId=='learn02') # groups with a shape-changing rule
lg_color <- df.sw %>% filter(learningTaskId=='learn03'|learningTaskId=='learn04') # groups with a color-changing rule
lg_object <- df.sw %>% filter(learningTaskId=='learn05'|learningTaskId=='learn06') # groups with color & shape changing rules

nrow(lg_shape) # 15
nrow(lg_color) # 6
nrow(lg_object) # 13

report_col(lg_shape$task_duration)  # min: 166007 | max: 575022 | mean: 311988.5 | sd: 121420.6 
report_col(lg_color$task_duration)  # min: 157104 | max: 370915 | mean: 276959.8 | sd: 74514.82
report_col(lg_object$task_duration) # min: 132781 | max: 2322982 | mean: 432938.5 | sd: 579199 

report_col(lg_shape$difficulty)  # min: 0 | max: 10 | mean: 4.5333 | sd: 2.9488 
report_col(lg_color$difficulty)  # min: 3 | max: 10 | mean: 6.1667 | sd: 2.4833 
report_col(lg_object$difficulty) # min: 0 | max: 9 | mean: 4.6154 | sd: 2.6312

lg_same <- df.sw %>% filter(learningTaskId=='learn01'|learningTaskId=='learn03'|learningTaskId=='learn06') # change to the same feature
lg_diff <- df.sw %>% filter(learningTaskId=='learn02'|learningTaskId=='learn04'|learningTaskId=='learn05') # change to a different feature

nrow(lg_same) # 8
nrow(lg_diff) # 26

report_col(lg_same$task_duration) # min: 183647 | max: 575022 | mean: 319050.4 | sd: 126109.5
report_col(lg_diff$task_duration) # min: 132781 | max: 2322982 | mean: 362207 | sd: 413766.4 

report_col(lg_same$difficulty) # min: 0 | max: 9 | mean: 3.75 | sd: 3.1053 
report_col(lg_diff$difficulty) # min: 0 | max: 10 | mean: 5.1923 | sd: 2.5926 

