options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

# Load data
load('../data/mturk_20191128.Rdata')

# Subject stats
# Age

# Gender

# Task duration


# Condition counts
df.sw %>% group_by(learningTaskId) %>% summarise(n())
# 1 learn05            7
# 2 learn04            6
# 3 learn06            6
# 4 learn02           13
# 5 learn01            2
