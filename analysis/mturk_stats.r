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
get_basic_stats <- function(list, ndigits=4, separator='|') {
  cat('min:', round(min(list), ndigits), separator,
      'max:', round(max(list), ndigits), separator,
      'mean:', round(mean(list), ndigits), separator,
      'sd:', round(sd(list), ndigits), '\n');
}

get_group_stats <- function(list, ndigits=2, separator='|') {
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
get_basic_stats(df.sw$age)
# min: 24 | max: 58 | mean: 38.8235 | sd: 9.9376

# Sex
get_group_stats(df.sw$sex)
# total 34 | male 16 47.06% | female 18 52.94%
genders <- unique(df.sw$sex);
for (i in 1:length(genders)) {
  gender_group <- df.sw %>% filter(sex==genders[i]);
  cat(genders[i], ' age\n');
  get_basic_stats(gender_group$age);
}
# Male age: min: 24 | max: 58 | mean: 38.5 | sd: 11.4717
# Female age: min: 27 | max: 57 | mean: 39.1111 | sd: 8.6832

# Task duration
get_basic_stats(df.sw$task_duration)
# min: 132,781 | max: 2,322,982 | mean: 352,052.5 | sd: 365,263.9

# Learn tasks
get_group_stats(df.sw$learningTaskId)
# total 34 | learn04 6 17.65% | learn06 6 17.65% | learn05 7 20.59% | learn01 2 5.88% | learn02 13 38.24%

