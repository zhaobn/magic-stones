
## Set up primitive elements
colors <- c('b', 'r', 'y')
shapes <- c('c', 'd', 's')
objects <- vector('character')
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

## Create theories

## Create learning updates

## Create generalization simulations
