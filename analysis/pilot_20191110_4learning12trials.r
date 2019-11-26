setwd('bramleylab/magic_stones/analysis')

# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("rjson")
library("dplyr")
library("tidyverse")
library("rjson")

# Load data
df1 <- as.data.frame(fromJSON(file = "../data/pilot-cleaned/c-bonan.json"))
df2 <- as.data.frame(fromJSON(file = "../data/pilot-cleaned/c-miguelangel.json"))
df3 <- as.data.frame(fromJSON(file = "../data/pilot-cleaned/c-mohammed.json"))
df4 <- as.data.frame(fromJSON(file = "../data/pilot-cleaned/c-yunlu.json"))

# Combine and save
pilotRaw <- rbind(df1, df2, df3, df4)
save(file='../data/pilot_20191110.Rdata', pilotRaw)

