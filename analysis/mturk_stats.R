options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
rm(list=ls())

# Load data
load(paste0('../data/mturk_20200112_reverse.Rdata'))


# Ensure correct data type
df.sw$sex <- as.character(df.sw$sex)
df.sw$learningTaskId <- as.character(df.sw$learningTaskId)

df.sw$age <- as.numeric(as.character(df.sw$age))
df.sw$task_duration <- as.numeric(as.character(df.sw$task_duration))
df.sw$instructions_duration <- as.numeric(as.character(df.sw$instructions_duration))
df.sw$difficulty <- as.numeric(as.character(df.sw$difficulty))
df.sw$engagement <- as.numeric(as.character(df.sw$engagement))

# Helper functions
report_col <- function(list, ndigits=4, separator=',') {
  cat('min', round(min(list), ndigits), separator,
      'max', round(max(list), ndigits), separator,
      'mean', round(mean(list), ndigits), separator,
      'sd', round(sd(list), ndigits), '\n');
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
report_col(df.sw$age)
summarise_group(df.sw$sex)

genders <- unique(df.sw$sex);
for (i in 1:length(genders)) {
  gender_group <- df.sw %>% filter(sex==genders[i]);
  cat(genders[i], ' age\n');
  report_col(gender_group$age);
}

report_col(df.sw$task_duration/60000)
summarise_group(df.sw$learningTaskId)

# Is there a outlier in task duration?
td <- df.sw %>% arrange(desc(task_duration)) # P43

# Out of curisity
report_col(df.sw$difficulty) 
report_col(df.sw$engagement) 

td <- df.sw%>%filter(!(ix==43))
td <- td %>% group_by(learningTaskId) %>% 
  summarise(avg_task_dur=mean(task_duration/60000))
df.sw %>% group_by(learningTaskId) %>% summarise(avg_task_dur=mean(task_duration/60000))
df.sw %>% group_by(learningTaskId) %>% summarise(avg_dfty=mean(difficulty))


# Take a peek of stats per learning condition groups
lg_shape <- df.sw %>% filter(learningTaskId %in% c('learn01', 'learn02')) # groups with a shape-changing rule
lg_color <- df.sw %>% filter(learningTaskId %in% c('learn03', 'learn04')) # groups with a color-changing rule
lg_object <- df.sw %>% filter(learningTaskId %in% c('learn05', 'learn06')) # groups with color & shape changing rules

nrow(lg_shape)
nrow(lg_color)
nrow(lg_object)

report_col(lg_shape$task_duration)
report_col(lg_color$task_duration)
report_col(lg_object$task_duration)

report_col(lg_shape$difficulty)
report_col(lg_color$difficulty)
report_col(lg_object$difficulty)

lg_same <- df.sw %>% filter(learningTaskId %in% c('learn01', 'learn03', 'learn06')) # change to the same feature
lg_diff <- df.sw %>% filter(learningTaskId %in% c('learn02', 'learn04', 'learn07')) # change to a different feature

nrow(lg_same) 
nrow(lg_diff) 

hg_same<-hgt %>%filter(learningTaskId %in% c('learn01', 'learn03', 'learn06')) 
hg_diff<-hgt %>% filter(learningTaskId %in% c('learn02', 'learn04','learn05'))

t.test(lg_same$task_duration, lg_diff$task_duration)
t.test(lg_same$difficulty, lg_diff$difficulty)
t.test(hg_same$vary, hg_diff$vary)


report_col(lg_same$task_duration) 
report_col(lg_diff$task_duration) 

report_col(lg_same$difficulty) 
report_col(lg_diff$difficulty)


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
extract_free_responses <- function(cond) {
  fr <- df.sw %>% filter(learningTaskId==cond) %>% select(ix, guess)
  write.csv(fr, file=paste0('rev_', cond, '_guess.csv'))
}
extract_free_responses('learn07')

# Append learning info to trials
df.fr <- df.fr %>% 
  mutate(learn_agent = case_when(
    learningTaskId == 'learn01' ~ 'rs', 
    learningTaskId == 'learn02' ~ 'yd', 
    learningTaskId == 'learn03' ~ 'bs', 
    learningTaskId == 'learn04' ~ 'rc', 
    learningTaskId == 'learn05' ~ 'yd', 
    learningTaskId == 'learn06' ~ 'bs')) %>%
  mutate(learn_recipient = case_when(
    learningTaskId == 'learn01' ~ 'yc', 
    learningTaskId == 'learn02' ~ 'rs',
    learningTaskId == 'learn03' ~ 'rd', 
    learningTaskId == 'learn04' ~ 'bs',
    learningTaskId == 'learn05' ~ 'bs', 
    learningTaskId == 'learn06' ~ 'yc',)) %>%
  mutate(learn_rule = case_when(
    learningTaskId == 'learn01' ~ '-2s', 
    learningTaskId == 'learn03' ~ '-2b', 
    learningTaskId == 'learn04' ~ '-2y',
    learningTaskId == 'learn05' ~ '-2y, -2c', 
    learningTaskId == 'learn06' ~ '-2b, -2s',)) %>%
  select(ix, learningTaskId, learn_agent, learn_recipient, learn_rule, guess, id) %>%
  arrange(learningTaskId, ix)
write.csv(df.fr, file = '../data/free_reponses.csv')

# Check theory compliance
df.tw <- df.tw %>%
  mutate(to_same_shape = if_else(substr(selection,2,2)==substr(agent,2,2), TRUE, FALSE)) %>%
  mutate(to_same_color = if_else(substr(selection,1,1)==substr(agent,1,1), TRUE, FALSE)) %>%
  mutate(to_same_object = if_else(selection==agent, TRUE, FALSE)) %>%
  mutate(to_diff_shape = if_else(substr(selection,2,2)!=substr(agent,2,2), TRUE, FALSE)) %>%
  mutate(to_diff_color = if_else(substr(selection,1,1)!=substr(agent,1,1), TRUE, FALSE)) %>%
  mutate(to_diff_object = if_else(selection!=agent & selection!=recipient, TRUE, FALSE)) %>%
  mutate(kept_shape = if_else(substr(selection,2,2)==substr(recipient,2,2), TRUE, FALSE)) %>%
  mutate(kept_color = if_else(substr(selection,1,1)==substr(recipient,1,1), TRUE, FALSE))
save(file='../data/mturk_20200101_trial_evaluated.Rdata', df.sw, df.tw, df.sim)

comp <- df.tw %>%
  mutate(r1 = case_when(
    learningTaskId=='learn01' & to_same_shape ~ TRUE,
    learningTaskId=='learn02' & to_diff_shape ~ TRUE,
    learningTaskId=='learn03' & to_same_color ~ TRUE,
    learningTaskId=='learn04' & to_diff_color ~ TRUE,
    learningTaskId=='learn05' & to_diff_object ~ TRUE,
    learningTaskId=='learn06' & to_same_object ~ TRUE,
    TRUE ~ FALSE)) %>%
  mutate(r2 = case_when(
    learningTaskId=='learn01' & kept_color ~ TRUE,
    learningTaskId=='learn02' & kept_color ~ TRUE,
    learningTaskId=='learn03' & kept_shape ~ TRUE,
    learningTaskId=='learn04' & kept_shape ~ TRUE,
    learningTaskId=='learn05' & to_diff_object ~ TRUE,
    learningTaskId=='learn06' & to_same_object ~ TRUE,
    TRUE ~ FALSE)) %>%
  select(ix, learningTaskId, trial, agent, recipient, selection, r1, r2) %>%
  mutate(compliance=r1&r2)
# Stats: compliance per group
per_group_total <- comp %>%
  group_by(learningTaskId, compliance) %>%
  summarise (n = n()) %>% mutate(total_compliance = n / sum(n)) %>%
  filter(compliance==TRUE) %>% select(learningTaskId, total_compliance)
per_group_r1 <- comp %>%
  group_by(learningTaskId, r1) %>%
  summarise (n = n()) %>% mutate(r1_compliance = n / sum(n)) %>%
  filter(r1==TRUE) %>% select(learningTaskId, r1_compliance)
per_group_r2 <- comp %>%
  group_by(learningTaskId, r2) %>%
  summarise (n = n()) %>% mutate(r2_compliance = n / sum(n)) %>%
  filter(r2==TRUE) %>% select(learningTaskId, r2_compliance)
per_group <- per_group_r1 %>% left_join(per_group_r2) %>% left_join(per_group_total)
# Plot it
per_group_r1_p <- per_group_r1%>%mutate(compliance='r1')%>%select(learningTaskId, compliance, value=r1_compliance)
per_group_r2_p <- per_group_r2%>%mutate(compliance='r2')%>%select(learningTaskId, compliance, value=r2_compliance)
per_group_total_p <- per_group_total%>%mutate(compliance='total')%>%select(learningTaskId, compliance, value=total_compliance)
per_group_p <- rbind(per_group_r1_p, per_group_r2_p, per_group_total_p) %>%
  filter(!(learningTaskId=='learn05'&(compliance=='r1'|compliance=='r2'))) %>%
  filter(!(learningTaskId=='learn06'&(compliance=='r1'|compliance=='r2')))
ggplot(per_group_p, aes(fill=compliance, y=value, x=learningTaskId)) + 
  geom_bar(position="dodge", stat="identity") + 
  labs(x='', y='') +
  scale_fill_manual(values = c("lightsteelblue3", "steelblue", "steelblue4"))
  #scale_colour_gradient(low = "gray90", high = "lightsteelblue3")
# -> compliance_per_learning_condition.jpeg
ggplot(per_group_total_p, aes(y=value, x=learningTaskId)) + 
  geom_bar(stat="identity", fill="steelblue") + labs(x='', y='') + 
  scale_fill_grey()
# -> total_compliance_per_learning_condition.jpeg


# Per trial
trials <- seq(1,15)

per_trial_total <- comp %>%
  group_by(trial, compliance) %>%
  summarise (n = n()) %>% mutate(total_compliance = n / sum(n)) %>%
  filter(compliance==TRUE) %>% select(trial, total_compliance)
per_trial_r1 <- comp %>%
  group_by(trial, r1) %>%
  summarise (n = n()) %>% mutate(r1_compliance = n / sum(n)) %>%
  filter(r1==TRUE) %>% select(trial, r1_compliance)
per_trial_r2 <- comp %>%
  group_by(trial, r2) %>%
  summarise (n = n()) %>% mutate(r2_compliance = n / sum(n)) %>%
  filter(r2==TRUE) %>% select(trial, r2_compliance)

ggplot(per_trial_total, aes(x=trial, y=total_compliance)) + 
  geom_bar(stat="identity", fill="lightpink") + labs(x='', y='') + scale_fill_grey() +
  scale_x_continuous("Trials", labels = as.character(trials), breaks = trials)
# -> total_compliance_per_trial.jpeg

per_trial_r1_p <- per_trial_r1%>%mutate(compliance='r1')%>%select(trial, compliance, value=r1_compliance)
per_trial_r2_p <- per_trial_r2%>%mutate(compliance='r2')%>%select(trial, compliance, value=r2_compliance)
per_trial_total_p <- per_trial_total%>%mutate(compliance='total')%>%select(trial, compliance, value=total_compliance)
per_trial_p <- rbind(per_trial_r1_p, per_trial_r2_p, per_trial_total_p)
  
ggplot(per_trial_p, aes(fill=compliance, y=value, x=trial)) + 
  geom_bar(position="dodge", stat="identity") + ylab('') + scale_fill_grey() +
  scale_x_continuous("Trials", labels = as.character(trials), breaks = trials)
# -> compliance_per_trial.jpeg

# Individual
ix <- comp$ix %>% unique()
ind <- comp %>%
  group_by(ix, compliance) %>%
  summarise (n = n()) %>% mutate(perc = n / sum(n)) %>%
  filter(compliance==TRUE)
ggplot(ind, aes(x=reorder(ix, -perc), y=perc)) + 
  geom_bar(stat="identity", fill="lightpink") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Participant', y='Compliance') +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")
# -> individual_compliance.jpeg

# Compliance vs aga
ind <- ind %>% left_join(df.sw, by='ix') %>%
  select(ix, learningTaskId, age, sex, perc)
ind$age <- as.numeric(as.character(ind$age))
cor.test(ind$perc, ind$age)
summary(lm(perc~sex, ind))

# Measure homogeneity
# For each learning condition
# For each trial - count all unique selections / # ppt
# Sum over trials for each condition
hg <- df.tw %>%
  select(ix, learningTaskId, trial, selection) %>%
  group_by(learningTaskId, trial) %>%
  summarise(vary=(n_distinct(selection)-1)/n()) %>%
  group_by(learningTaskId) %>%
  summarise(total_vary=sum(vary))
hg2 <- df.tw %>%
  select(ix, learningTaskId, trial, selection) %>%
  group_by(learningTaskId, trial) %>%
  summarise(vary=n_distinct(selection)-1, total=n()) %>%
  mutate(max=if_else(total>8, 8, as.numeric(total))) %>%
  group_by(learningTaskId) %>%
  summarise(total_vary=sum(vary), total_max=sum(max)) %>%
  mutate(var=total_vary/total_max)
hgt <- df.tw %>%
  select(ix, learningTaskId, trial, selection) %>%
  group_by(learningTaskId, trial) %>%
  summarise(vary=(n_distinct(selection)-1)/n())

# Try var
default <- data.frame(objects) %>% select('selection'=objects)
default$selection <- as.character(default$selection)
max <- var(c(1, rep(0, 8)))

var_hg <- function(data, cond, tid, norm = TRUE) {
  dt <- data %>% filter(learningTaskId==cond&trial==tid) %>%
    count(selection) %>% mutate(freq=n/sum(n))
  dt$selection <- as.character(dt$selection)
  dt <- default %>% left_join(dt, by='selection') %>% replace(is.na(.), 0) %>%
    select(selection, freq)
  if (norm) {
    return(var(dt$freq)/max)# normalize
  } else {
    return(var(dt$freq))
  }
}

var_hg_cond <- function(data, cond) {
  condition <- rep(cond, 15)
  trial <- seq(15)
  homogeneity <- rep(0, 15)
  for (i in 1:15) {
    homogeneity[i] <- var_hg(data, cond, i)
  }
  return(data.frame(condition, trial, homogeneity))
}

vhg <- var_hg_cond(df.tw, 'learn01')
for (i in 2:7) {
  vhg <- rbind(vhg, var_hg_cond(df.tw, paste0('learn0', i)))
}

ggplot(vhg, aes(x=condition, y=reorder(trial, desc(trial)), fill = homogeneity)) + 
  geom_raster() +
  geom_text(aes(label = round(homogeneity, 2))) +
  scale_fill_gradient(low = "white", high = "steelblue4") + 
  labs(x='', y='')

# Aggregated per condition
get_var_per_cond <- function(data, groupName) {
  vhg_per_cond <- rep(0, 7)
  for (i in 1:7) {
    trials <- rep(0, 15)
    for (j in 1:15) {
      trials[j] <- var_hg(data, paste0('learn0', i), j, FALSE)
    }
    vhg_per_cond[i] <- sum(trials)/(15*max)
  }
  condition <- rep('', 7)
  group <- rep(groupName, 7)
  for (i in 1:7) {
    condition[i] <- paste0('learn0', i)
  }
  return(data.frame(condition, group, vhg_per_cond))
}
var_cond <- rbind(get_var_per_cond(og.tw, 'default'), get_var_per_cond(df.tw, 'reverse'))

ggplot(var_cond, aes(fill=group,x=condition, y=vhg_per_cond)) + 
  geom_bar(position="dodge", stat="identity") + 
  labs(x = '', y = '') +
  scale_fill_manual(values=c("lightsteelblue3","steelblue4"))

# Aggregated per trial
get_var_per_trial <- function(data, groupName) {
  vhg_per_trial <- rep(0, 15)
  for (i in 1:15) {
    conds <- rep(0, 7)
    for (j in 1:7) {
      conds[j] <- var_hg(data, paste0('learn0', j), i, FALSE)
    }
    vhg_per_trial[i] <- sum(conds)/(7*max)
  }
  trial <- seq(15)
  group <- rep(groupName, 15)
  return(data.frame(trial, group, vhg_per_trial))
}
var_t <- rbind(get_var_per_trial(og.tw, 'default'), get_var_per_trial(df.tw, 'reverse'))

ggplot(var_t, aes(x=trial, y=vhg_per_trial, fill=group)) + 
  geom_bar(stat="identity", position="dodge") + 
  scale_x_continuous(breaks = trial) +
  scale_fill_manual(values=c("lightsteelblue3","steelblue4")) +
  labs(x = '', y = '')

# Prep a frequency table for comparison
default$selection<-as.character(default$selection)
count_selection<-function(cond, tid, data=df.tw) {
  learningTaskId<-rep(paste0('learn0', cond), length(default[,1]))
  trial<-rep(tid, length(default[,1]))
  dt<-data%>%filter(learningTaskId==paste0('learn0',cond)&trial==tid)%>%count(selection) 
  dt$selection<-as.character(dt$selection)
  dt<-default%>%left_join(dt, by='selection')%>%replace(is.na(.), 0)%>%mutate(freq=n/sum(n))
  dt<-cbind(data.frame(learningTaskId, trial), dt)
  return(dt)
}
df.freq<-count_selection(1,1)
ncond<-as.numeric(substr(max(df.tw$learningTaskId),7,7))
for (i in 1:ncond) {
  for (j in 1:max(df.tw$trial)) {
    if (!(i==1&j==1)) df.freq<-rbind(df.freq, count_selection(i,j))
  }
}
save(df.sw, df.tw, df.freq, file='../data/mturk_20200112_reverse.Rdata')


