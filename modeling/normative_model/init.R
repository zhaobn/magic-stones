
## Set up primitive elements
colors <- c('b', 'r', 'y') # blue, red, yellow
shapes <- c('c', 'd', 's') # circle, diamond, square
objects <- vector('character') # color-shape
for (c in 1:length(colors)) {
  for (s in 1:length(shapes)) {
    objects <- c(objects, paste0(colors[c], shapes[s]))
  }
}

## Generate the complete hypothesis space
full_hypothesis_space <- list()
generate_hypo <- function(cause, effect, space) {
  n <- length(space)
  for (i in 1:length(cause)) {
    for (j in (1:length(effect))) {
      idx <- n + j + length(effect) * (i - 1)
      hypothesis <- c(compose_from(cause[i]), compose_from(effect[j]))
      space[[idx]] <- hypothesis
    }
  }
  return(space)
}
compose_from <- function(s) {
  return(if (s %in% colors) paste0(s, '*') else 
    if (s %in% shapes) paste0('*', s) else s)
}

# (1) o -> o
full_hypothesis_space <- generate_hypo(objects, objects, full_hypothesis_space)
# (2) o -> c
full_hypothesis_space <- generate_hypo(objects, colors, full_hypothesis_space)
# (3) o -> s 
full_hypothesis_space <- generate_hypo(objects, shapes, full_hypothesis_space)
# (4) c -> o 
full_hypothesis_space <- generate_hypo(colors, objects, full_hypothesis_space)
# (5) c -> c
full_hypothesis_space <- generate_hypo(colors, colors, full_hypothesis_space)
# (6) c -> s 
full_hypothesis_space <- generate_hypo(colors, shapes, full_hypothesis_space)
# (7) s -> o
full_hypothesis_space <- generate_hypo(shapes, objects, full_hypothesis_space)
# (8) s -> c
full_hypothesis_space <- generate_hypo(shapes, colors, full_hypothesis_space)
# (9) s -> s
full_hypothesis_space <- generate_hypo(shapes, shapes, full_hypothesis_space)


## Update from learnings
# Exclude hypotheses that are incosistent with observations
# according to the nine theories
check_hypo <- function(observation, hypo) {
  pass <- 0
  if (observation[1] == hypo[1] && observation[3] == hypo[2]) {
    pass <- 1 # (1) o -> o
  } else if (observation[1] == hypo[1] && 
             substr(hypo[2], 2, 2) == '*' && 
             substr(observation[3], 1, 1) == substr(hypo[2], 1, 1)) {
    pass <- 1 # (2) o -> c
  } else if (observation[1] == hypo[1] &&
             substr(hypo[2], 1, 1) == '*' &&
             substr(observation[3], 2, 2) == substr(hypo[2], 2, 2)) {
    pass <- 1 # (3) o -> s
  } else if (substr(hypo[1], 2, 2) == '*' &&
             substr(hypo[1], 1, 1) == substr(observation[1], 1, 1) && 
             hypo[2] == observation[3]) {
    pass <- 1 # (4) c -> o 
  } else if (substr(hypo[1], 2, 2) == '*' &&
             substr(hypo[1], 1, 1) == substr(observation[1], 1, 1) && 
             substr(observation[3], 1, 1) == substr(hypo[2], 1, 1)) {
    pass <- 1 # (5) c -> c
  } else if (substr(hypo[1], 2, 2) == '*' &&
             substr(hypo[1], 1, 1) == substr(observation[1], 1, 1) && 
             substr(observation[3], 2, 2) == substr(hypo[2], 2, 2)) {
    pass <- 1 # (6) c -> s
  } else if (substr(hypo[1], 1, 1) == '*' &&
             substr(hypo[1], 2, 2) == substr(observation[1], 2, 2) && 
             hypo[2] == observation[3]) {
    pass <- 1 # (7) s -> o
  } else if (substr(hypo[1], 1, 1) == '*' &&
             substr(hypo[1], 2, 2) == substr(observation[1], 2, 2) &&
             substr(observation[3], 1, 1) == substr(hypo[2], 1, 1)) {
    pass <- 1 # (8) s -> c
  } else if (substr(hypo[1], 1, 1) == '*' &&
             substr(hypo[1], 2, 2) == substr(observation[1], 2, 2) &&
             substr(observation[3], 2, 2) == substr(hypo[2], 2, 2)) {
    pass <- 1 # (9) s -> s
  } 
  return(pass)
}

# Get the filtered hypothesis space
filter_hypos <- function(observation, space) {
  passed <- list()
  for (i in 1:length(space)) {
    passed[[i]] <- check_hypo(observation, space[[i]])  
  }
  return(space[which(passed==1)])
}

# Get posterior probablities
get_posterior <- function(data, hypo_space) {
  n <- length(hypo_space)
  likelihoods <- rep(0, len=n)
  for (i in 1:n) {
    likelihoods[i] <- check_hypo(c(data[1], '', data[2]), hypo_space[[i]])
  }
  # Smooth & normalize
  # Ignore multiplying prior probablity because it is assumed to be unform over hypos
  posterior <- sapply(likelihoods, 
                      function(x) exp(x*3)/sum(sapply(likelihoods, function(x) exp(x*3))))
  return(posterior)
}
# Calculate marginal posteriors
get_prediction <- function(data, hypo_space) {
  get_pred <- function(data, hypo) {
    is_included <- (data[1] == hypo[1] || 
                      (data[1] != hypo[1] && substr(data[1], 1, 1) == substr(hypo[1], 1, 1)) ||
                      (data[1] != hypo[1] && substr(data[1], 2, 2) == substr(hypo[1], 2, 2)))
    # Get predicted state
    pred <- if (is_included) hypo[2] else objects[sample(1:length(objects), 1)]
    # Fill in blanks
    if (substr(pred, 1, 1) == '*') substr(pred, 1, 1) <- shapes[sample(1:length(shapes), 1)] else
      if (substr(pred, 2, 2) == '*') substr(pred, 2, 2) <- colors[sample(1:length(colors), 1)]
    return(pred)
  }
  predictions <- rep('', length(hypo_space))
  for (i in 1:length(hypo_space)) {
    predictions[i] <- get_pred(data, hypo_space[i])
  }
  return(predictions)
}
# final output
get_posterior_predictive <- function(data, hypo_space) {
  posteriors <- get_posterior(data, hypo_space)
  predictions <- get_prediction(data, hypo_space)
  post_pred <- list()
  selections <- unique(predictions)
  dist <- rep(1, length(selections))
  # Calculate marginal distributions
  for (i in 1:length(selections)) {
    dist[i] <- sum(posteriors[which(predictions==selections[i])])
  }
  post_pred[[1]] <- selections
  post_pred[[2]] <- sapply(dist, function(x) x/sum(dist)) # normalized
  return(post_pred)
}

## Work with data## Create simulation data
# List the six learning scenarios
get_learning_settings <- function(learningTaskId) {
  switch (learningTaskId,
    'learn01' = c('rs', 'yc', 'ys'),
    "learn02" = c('yd', 'rs', 'rc'),
    "learn03" = c('bs', 'rd', 'bd'),
    "learn04" = c('rc', 'bs', 'ys'),
    "learn05" = c('yd', 'bs', 'yc'),
    "learn06" = c('bs', 'yc', 'bs')
  )
}
# Read data
library(dplyr)
load('../../analysis/data/mturk_20191128_trial_fixed.Rdata')
tasks <- df.tw %>% 
  select(learningTaskId, trial, agent, recipient) %>% 
  distinct() %>%
  arrange(learningTaskId, trial)

# Generate simulation data
simulate_for <- function(lid, tid) {
  base <- tasks %>% filter(learningTaskId==lid, trial==tid)
  prio <- filter_hypos(get_learning_settings(lid), full_hypothesis_space)
  post <- as.data.frame(get_posterior_predictive(c(base[[3]], base[[4]]), prio))
  names(post) <- c('selection', 'prob')
  return(merge(base, post))
}

df.sim <- simulate_for('learn01', 1)
for (i in 1:6) {
  for (j in 1:15) {
    task <- paste0('learn0', i)
    if (!(i==1&&j==1)) df.sim <- rbind(df.sim, simulate_for(task, j))
  }
}

