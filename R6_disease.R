tic <- function() {
  tic_start <<- base::Sys.time()
}

toc <- function() {
  dt <- base::difftime(base::Sys.time(), tic_start)
  dt <- round(dt, digits = 1L)
  message(paste(format(dt), "since tic()"))
}

################################################################################

library(R6)

Agent <- R6Class("Agent",
  public = list(
    # Public data
    state = character(),
    
    # Public methods
    initialize = function(state = "healthy") {
      self$state <- state
    },
    
    update_state = function(probs) {
      if (self$state == "healthy") { # If agent is healthy it can either stay healthy or get infected
        new_state <- sample(x = c("healthy", "sick"), 
                       size = 1, 
                       prob = c(1 - probs['sick'], probs['sick']))
      } else if (self$state == "sick") { # A sick agent can continue being sick, recover and become immune or die
        new_state <- sample(x = c("sick", "immune", "dead"), 
                       size = 1, 
                       prob = c(1 - (probs['recovery'] + probs['death']), probs['recovery'], probs['death']))
      
      } else {
        new_state <- self$state # Immune and dead states do not change
      }
      self$state <- new_state  # Update state
    }
  )
)

World <- R6Class("World",
  public = list(
    # Public data
    size = NULL,
    world = NULL,
    
    # Public methods
    initialize = function(size) {
      self$size <- size
      self$world <- matrix(vector("list", size * size), nrow = size, ncol = size)
      for (i in 1:size) {
        for (j in 1:size) {
          self$world[[i, j]] <- list()
        }
      }
    },
    
    add_agent = function(row, col, agent) {
      self$world[[row, col]] <- c(self$world[[row, col]], list(agent))
    },
    
    move_agents = function() {
      new_world <- matrix(vector("list", self$size * self$size), nrow = self$size, ncol = self$size)
      for (i in 1:self$size) {
        for (j in 1:self$size) {
          agents <- self$world[[i, j]]
          for (agent in agents) {
            if (agent$state != "dead") {
              move <- rnorm(2, mean = 0, sd = 1)
              new_row <- min(max(1, round(i + move[1])), self$size)
              new_col <- min(max(1, round(j + move[2])), self$size)
              new_world[[new_row, new_col]] <- c(new_world[[new_row, new_col]], list(agent))
            } else {
              new_world[[i, j]] <- c(new_world[[i, j]], list(agent))
            }
          }
        }
      }
      self$world <- new_world
    },
    
    update_states = function(probs) {
      # Iterate over all grid cells in the world and determine infection probability based on
      # the number of already sick individuals in that grid cell.
      for (i in 1:self$size) {
        for (j in 1:self$size) {
          agents <- self$world[[i, j]]
          num_sick <- sum(vapply(agents, function(agent) agent$state == "sick", logical(1)))
          # Now, probability of becoming sick is naively proportional to percentage of 
          # already sick individuals in a cell
          probs['sick'] <- 1 - ((1 - probs['beta']) ^ num_sick)
          for (agent in agents) {
            agent$update_state(probs)
          }
        }
      }
    },
    
    get_counts = function() {
      counts <- data.frame(
        row = integer(),
        col = integer(),
        healthy = integer(),
        immune = integer(),
        sick = integer(),
        dead = integer()
      )
      for (i in 1:self$size) {
        for (j in 1:self$size) {
          agents <- self$world[[i, j]]
          counts <- rbind(counts, data.frame(
            row = i,
            col = j,
            healthy = sum(vapply(agents, function(agent) agent$state == "healthy", logical(1))),
            immune = sum(vapply(agents, function(agent) agent$state == "immune", logical(1))),
            sick = sum(vapply(agents, function(agent) agent$state == "sick", logical(1))),
            dead = sum(vapply(agents, function(agent) agent$state == "dead", logical(1)))
          ))
        }
      }
      return(counts)
    }
  )
)

library(ggplot2)
library(gganimate)
library(progress)

run_simulation <- function(size, num_steps, initial, prob, progress = FALSE) {
  stopifnot(prob['recovery'] + prob['death'] <= 1) # some simplistic input validation
  if (progress) { pb <- progress_bar$new(total = num_steps + 1) }
  
  world <- World$new(size)
  
  # Initialize agents
  for (state in c('healthy', 'immune', 'sick')) {
    if (initial[state] > 0) {
      for (i in 1:initial[state]) {
        row <- sample(1:size, 1)
        col <- sample(1:size, 1)
        world$add_agent(row, col, Agent$new(state = state))
      }
    }
  }
  
  results <- list()
  
  # Save the initial state of the world
  counts <- world$get_counts()
  counts$step <- 1
  results[[1]] <- counts
  
  
  # Simulate one generation
  for (step in 2:num_steps + 1) {
    world$update_states(prob)
    world$move_agents()
    counts <- world$get_counts()
    counts$step <- step
    results[[step]] <- counts
    if (progress) { pb$tick() }
  }
  
  results <- do.call(rbind, results)
  
  return(results)
}

visualize_simulation <- function(results) {
  p <- ggplot(results, aes(x = col, y = row)) +
    geom_tile(aes(fill = sick)) +
    scale_fill_gradient(low = "white", high = "red") +
    theme_minimal() +
    labs(title = "Disease Spread Simulation", x = "X", y = "Y", fill = "Sick Individuals") +
    transition_states(step, transition_length = 2, state_length = 1) +
    ease_aes('linear') +
    ggtitle('Disease Spread Simulation - Generation {closest_state}') +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  
  animate(p, nframes = length(unique(results$step)), fps = 2, renderer = gifski_renderer())
}

visualise_snapshot <- function(step = 1) {
  # See a particular snapshot from simulation step N
  results %>% filter(step == 1) %>% ggplot(aes(x = col, y = row)) +
    geom_tile(aes(fill = sick)) +
    scale_fill_gradient(low = "white", high = "red") +
    theme_minimal()
}

###############################################################################
initial <- c()
probs <- c()
# Parameters
size <- 20                    # World size
num_steps <- 50               # Number of generations
initial['healthy'] <- 998
initial['immune'] <- 0
initial['sick'] <- 2
probs['beta'] <- 0.5           # Transmission probability
probs['recovery'] <- 0.1
probs['death'] <- 0.05

# Run the simulation

library(tidyverse)
runme <- function(size, num_steps, initial, probs) {
  results <- run_simulation(size, num_steps, initial, probs)
  results <- results |> group_by(step) |> 
    summarise(n_sick = sum(sick), n_healthy = sum(healthy), n_immune = sum(immune), n_dead = sum(dead)) |> 
    pivot_longer(starts_with("n_"), names_to = 'measure')
}

library(future)
future::plan(multisession, workers=parallel::detectCores()-1)

trajectories <- list()
tic()
for (i in 1:10) {
  message(paste0("Trajectory ", i))
  trajectories[[i]] <- future({ runme(size, num_steps, initial, probs) }, seed = 48)
}
toc()
trajectories <- value(trajectories)
toc()

data <- bind_rows(lapply(seq_along(trajectories), function(i) {
  trajectories[[i]] %>% mutate(trajectory = i)
}))

data |> ggplot(aes(x = step, y = value, col = measure, group = measure)) + 
  geom_smooth() + 
  theme_minimal()

# Visualize the results
visualize_simulation(trajectories[[1]])
