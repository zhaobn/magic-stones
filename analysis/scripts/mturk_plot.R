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
    right_join(ops, by="selection") %>%
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

plots <- list()

for (i in 1:6) {
  for (j in 1:15) {
    idx <- i + 6 * (j - 1)
    plots[[idx]] <- plot(i, j)
  }
}

jpeg('trials.jpeg', width = 1200, height = 1500)
do.call("grid.arrange", c(plots, ncol=6))
dev.off()

