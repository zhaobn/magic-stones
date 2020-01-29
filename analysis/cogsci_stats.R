options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
rm(list=ls())

# Helper function
report<-function(dt) return(paste(round(mean(dt), 2), '\\pm', round(sd(dt), 2)))

# Basic participant stats
df.sw %>% count(sex)

df.sw$age<-as.numeric(as.character(df.sw$age))
report(df.sw$age)

df.sw$task_duration<-as.numeric(as.character(df.sw$task_duration))
report(df.sw$task_duration/60000)

# Group info
groups<-df.sw %>% count(learningTaskId, order)
report(groups$n)

# Get homogeneity measures
# Prep all possible selections
objects <- vector('character')
for (c in c('b', 'r', 'y')) {
  for (s in c('c', 'd', 's')) objects <- c(objects, paste0(c, s))
}
default <- data.frame(objects) %>% select('selection'=objects)
default$selection <- as.character(default$selection)

# Calculate homogeneity measure
max <- var(c(1, rep(0, 8)))
get_hm <- function(cond, tid, data=reverses) {
  dt <- data %>% 
    filter(learningTaskId == cond & trial == tid) %>%
    count(selection) %>% 
    mutate(freq=n/sum(n))
  dt$selection <- as.character(dt$selection)
  dt <- default %>%
    left_join(dt, by='selection') %>%
    replace(is.na(.), 0) %>%
    select(selection, freq)
  return(var(dt$freq)/max)
}

# Over all trials
df.tasks[,'hm_all']<-as.numeric(mapply(get_hm, df.tasks[,'learningTaskId'], df.tasks[,'trial']))
report(df.tasks$hm_all)

min(df.tasks$hm_all); df.tasks%>%filter(hm_all==min(df.tasks$hm_all))
max(df.tasks$hm_all); df.tasks%>%filter(hm_all==max(df.tasks$hm_all))

# Generate random predictions
df.tw[,'random_selection']<-sapply(df.tw$ix, function(x) objects[sample(1:length(objects), 1)])
# Sanity checks
df.tw%>%count(random_selection) # Looking good :)
# Create proper data format
rand_sel<-df.tw%>%select(ix, learningTaskId, order, trial, selection=random_selection)
# Get homogeneity measure for random selections
df.tasks[,'hm_rand']<-as.numeric(mapply(get_hm, df.tasks[,'learningTaskId'], df.tasks[,'trial']))

# Test overall strategies
t.test(df.tasks$hm_all, df.tasks$hm_rand, paired = TRUE)

# Test order effects
defaults<-df.tw%>%
  filter(order=='default') %>% select(ix, learningTaskId, order, trial, selection)
reverses<-df.tw%>%
  filter(order=='reverse') %>% select(ix, learningTaskId, order, trial, selection)
df.tasks[,'hm_default']<-as.numeric(mapply(get_hm, df.tasks[,'learningTaskId'], df.tasks[,'trial']))
df.tasks[,'hm_reverse']<-as.numeric(mapply(get_hm, df.tasks[,'learningTaskId'], df.tasks[,'trial']))

report(df.tasks$hm_default)
report(df.tasks$hm_reverse)

t.test(df.tasks$hm_default, df.tasks$hm_reverse, paired = TRUE)

# Test similarity effects
similars<-df.tasks%>%filter(learningTaskId%in%c('learn01', 'learn03', 'learn05'))
differents<-df.tasks%>%filter(learningTaskId%in%c('learn02', 'learn04', 'learn06'))

report(similars$hm_all)
report(differents$hm_all)

t.test(similars$hm_all, differents$hm_all, var.equal = TRUE)

# Save up for plotting
save(df.sw, df.tw, df.freq, df.tasks, file='cogsci_20200127.Rdata')

# Self-report difficulty
df.sw$difficulty<-as.numeric(as.character(df.sw$difficulty))
df_sm<-df.sw%>%filter(learningTaskId%in%c('learn01', 'learn03', 'learn05'))
df_df<-df.sw%>%filter(learningTaskId%in%c('learn02', 'learn04', 'learn06'))
t.test(df_sm$difficulty, df_df$difficulty, var.equal = TRUE)

# Test feature effects
shapes<-df.tasks%>%filter(learningTaskId%in%c('learn01', 'learn02'))
colors<-df.tasks%>%filter(learningTaskId%in%c('learn03', 'learn04'))

report(shapes$hm_all)
report(colors$hm_all)

t.test(shapes$hm_all, colors$hm_all, var.equal = TRUE)



