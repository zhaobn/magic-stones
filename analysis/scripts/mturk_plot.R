options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
library(gridExtra)
rm(list=ls())

# Load data
load('../data/mturk_20191128_fixed.Rdata')
df.tw$selection <- as.character(df.tw$selection)

ops <- data.frame(
  "selection" = c('rs', 'rd', 'rc', 'ys', 'yd', 'yc', 'bs', 'bd', 'bc'),
  "count" = rep(0, 9)
)
ops$selection <- as.character(ops$selection)

plot <- function(task_idx, t) {
  group <- paste0('learn0', task_idx)
  dt <- df.tw %>% 
    filter(learningTaskId==group & trial==t) %>%
    select(ix, selection) %>% mutate(count = 1) %>%
    right_join(ops, by='selection') %>%
    mutate(count.x = ifelse(is.na(count.x), 0, count.x)) %>%
    mutate(c = count.x + count.y) %>% select(selection, c) %>%
    group_by(selection) %>% summarize(n = sum(c)) %>%
    mutate(perc=round(n/sum(n), 2)) %>% select(selection, perc)
  p <- ggplot(dt, aes(x = selection, y = perc)) +
    geom_bar(stat = "identity") +
    xlab('') +
    #xlab(paste("Task", task_idx, "Trial", t)) + 
    ylab('') +
    coord_cartesian(ylim=c(0,1))
  return(p)
}

make_plots <- function(plot_func, fig_name, width = 1200, height = 1500) {
  plots <- list()
  for (i in 1:6) {
    for (j in 1:15) {
      idx <- i + 6 * (j - 1)
      plots[[idx]] <- do.call(plot_func, list(i,j))
    }
  }

  jpeg(fig_name, width = width, height = height)
  do.call("grid.arrange", c(plots, ncol=6))
  dev.off()
}
#plot(1,1)
make_plots(plot, 'trials.jpeg')

## Plot relative labels
rel <- data.frame(
  "sel_label" = c('aa', 'ar', 'ad', 'ra', 'rr', 'rd', 'da', 'dr', 'dd'),
  "count" = rep(0, 9)
)
rel$sel_label <- as.character(rel$sel_label)
plot_rel <- function(task_idx, t) {
  group <- paste0('learn0', task_idx)
  dt <- df.tw %>% 
    filter(learningTaskId==group & trial==t) %>%
    select(ix, sel_label) %>% mutate(count = 1) %>%
    right_join(rel, by="sel_label") %>%
    mutate(count.x = ifelse(is.na(count.x), 0, count.x)) %>%
    mutate(c = count.x + count.y) %>% select(sel_label, c) %>%
    group_by(sel_label) %>% summarize(n = sum(c)) %>%
    mutate(perc=round(n/sum(n), 2)) %>% select(sel_label, perc)
  p <- ggplot(dt, aes(x = sel_label, y = perc)) +
    geom_bar(stat = "identity") +
    xlab('') + ylab('') + coord_cartesian(ylim=c(0,1))
  return(p)
}
#plot_rel(1,1)
make_plots(plot_rel, 'trials_relative.jpeg')

## Plot aggregated relative labels
plot_group <- function(task_idx) {
  dt <- df.tw %>% 
    filter(learningTaskId==paste0('learn0', task_idx)) %>%
    select(ix, sel_label) %>% mutate(count = 1) %>%
    right_join(rel, by="sel_label") %>%
    mutate(count.x = ifelse(is.na(count.x), 0, count.x)) %>%
    mutate(c = count.x + count.y) %>% select(sel_label, c) %>%
    group_by(sel_label) %>% summarize(n = sum(c)) %>%
    mutate(perc=round(n/sum(n), 2)) %>% select(sel_label, perc)
  p <- ggplot(dt, aes(x = sel_label, y = perc)) +
    geom_bar(stat = "identity") +
    xlab('') + ylab('') + coord_cartesian(ylim=c(0,1))
  return(p)
}

plot_trial <- function(trial_idx) {
  dt <- df.tw %>% 
    filter(trial==trial_idx) %>%
    select(ix, sel_label) %>% mutate(count = 1) %>%
    right_join(rel, by="sel_label") %>%
    mutate(count.x = ifelse(is.na(count.x), 0, count.x)) %>%
    mutate(c = count.x + count.y) %>% select(sel_label, c) %>%
    group_by(sel_label) %>% summarize(n = sum(c)) %>%
    mutate(perc=round(n/sum(n), 2)) %>% select(sel_label, perc)
  p <- ggplot(dt, aes(x = sel_label, y = perc)) +
    geom_bar(stat = "identity") +
    xlab(paste('trial', trial_idx)) + ylab('') + coord_cartesian(ylim=c(0,1))
  return(p)
}

trial_plots <- list()
for (i in 1:15) {
  trial_plots[[i]] <- plot_trial(i)
}
jpeg('agg_trials.jpeg', width = 1000, height = 550)
do.call("grid.arrange", c(trial_plots, ncol=5))
dev.off()

#######################################################################
## Plot simulations
#######################################################################

# Selections
df.sim$selection <- as.character(df.sim$selection)
ops <- data.frame(
  "selection" = c('rs', 'rd', 'rc', 'ys', 'yd', 'yc', 'bs', 'bd', 'bc'),
  "prob" = rep(0, 9)
)
ops$selection <- as.character(ops$selection)
plot <- function(task_idx, t) {
  group <- paste0('learn0', task_idx)
  dt <- df.sim %>% 
    filter(learningTaskId==group & trial==t) %>%
    select(selection, prob) %>%
    right_join(ops, by='selection') %>%
    mutate(prob.x = ifelse(is.na(prob.x), 0, prob.x)) %>%
    mutate(p = prob.x + prob.y) %>% select(selection, p)
  p <- ggplot(dt, aes(x = selection, y = p)) +
    geom_bar(stat = "identity") +
    xlab('') +
    #xlab(paste("Task", task_idx, "Trial", t)) + 
    ylab('') +
    coord_cartesian(ylim=c(0,1))
  return(p)
}
make_plots(plot, 'sim_trials.jpeg')

# Relative labels
rel <- data.frame(
  "sel_label" = c('aa', 'ar', 'ad', 'ra', 'rr', 'rd', 'da', 'dr', 'dd'),
  "prob" = rep(0, 9)
)
rel$sel_label <- as.character(rel$sel_label)
plot_rel <- function(task_idx, t) {
  group <- paste0('learn0', task_idx)
  dt <- df.sim %>% 
    filter(learningTaskId==group & trial==t) %>%
    select(sel_label, prob) %>%
    right_join(rel, by="sel_label") %>%
    mutate(prob.x = ifelse(is.na(prob.x), 0, prob.x)) %>%
    mutate(p = prob.x + prob.y) %>% select(sel_label, p)
  p <- ggplot(dt, aes(x = sel_label, y = p)) +
    geom_bar(stat = "identity") +
    xlab('') + ylab('') + coord_cartesian(ylim=c(0,1))
  return(p)
}

make_plots(plot_rel, 'sim_trials_relative.jpeg')

## Aggregated data
plot_group <- function(task_idx) {
  dt <- df.sim %>% 
    filter(learningTaskId==paste0('learn0', task_idx)) %>%
    select(sel_label, prob) %>%
    right_join(rel, by="sel_label") %>%
    mutate(prob.x = ifelse(is.na(prob.x), 0, prob.x)) %>%
    mutate(p = prob.x + prob.y) %>% select(sel_label, p) %>%
    group_by(sel_label) %>% summarize(n = sum(p)) %>%
    mutate(perc=round(n/sum(n), 2)) %>% select(sel_label, perc)
  p <- ggplot(dt, aes(x = sel_label, y = perc)) +
    geom_bar(stat = "identity") +
    xlab('') + ylab('') + coord_cartesian(ylim=c(0,1))
  return(p)
}
group <- 6
jpeg(paste0('sim_agg_g', group, '.jpeg'), width = 500, height = 350)
plot_group(group)
dev.off()


plot_trial <- function(trial_idx) {
  dt <- df.sim %>% 
    filter(trial==trial_idx) %>%
    select(sel_label, prob) %>%
    right_join(rel, by="sel_label") %>%
    mutate(prob.x = ifelse(is.na(prob.x), 0, prob.x)) %>%
    mutate(p = prob.x + prob.y) %>% select(sel_label, p) %>%
    group_by(sel_label) %>% summarize(n = sum(p)) %>%
    mutate(perc=round(n/sum(n), 2)) %>% select(sel_label, perc)
  p <- ggplot(dt, aes(x = sel_label, y = perc)) +
    geom_bar(stat = "identity") +
    xlab(paste('trial', trial_idx)) + ylab('') + coord_cartesian(ylim=c(0,1))
  return(p)
}
trial_plots <- list()
for (i in 1:15) {
  trial_plots[[i]] <- plot_trial(i)
}
jpeg('sim_agg_trials.jpeg', width = 1000, height = 550)
do.call("grid.arrange", c(trial_plots, ncol=5))
dev.off()
