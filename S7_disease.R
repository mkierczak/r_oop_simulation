library(S7)

agent <- S7::new_class("agent", properties = list(
  state = S7::new_property(class = S7::class_character, default = 'healthy'),
  xpos = S7::new_property(class = S7::class_numeric, default = NULL),
  ypos = S7::new_property(class = S7::class_numeric, default = NULL)
))

update_state <- S7::new_generic("update_state", dispatch_args = "x")

S7::method(update_state, agent) <- function(x, sick_prob, recovery_prob, death_prob) {
    if (x@state == "healthy" && runif(1) <= sick_prob) {
      x@state <- "sick"
    } else if (x@state == "sick") {
      fate <- runif(1)
      if (fate <= recovery_prob) {
        x@state <- "immune"
      } else if (fate > recovery_prob && fate <= (recovery_prob + death_prob)) {
        x@state <- "dead"
      } else if (fate > recovery_prob + death_prob) {
        x@state == "sick"                   # Explicitly written for teaching purposes only
      }
    }
    # Immune and dead states do not change
  return(x)
  }

#### Create the world

world <- S7::new_class("agent", properties = list(
  size = S7::new_property(class = S7::class_numeric),
  world = S7::new_property(class = S7::class_any)
),
constructor = function(s) {
  tmp <- matrix(vector("list", s * s), nrow = s, ncol = s)
  for (i in 1:s) {
    for (j in 1:s) {
      tmp[[i, j]] <- list()
    }
  }
  value <- new_object(S7_object(), size = s, world = tmp) 
  return(value)  
  }
)

########################### Add agent #############################
add_agent <- S7::new_generic("add_agent", dispatch_args = "x")

S7::method(add_agent, world) <- function(x, row, col, agent) {
  agent@xpos <- col
  agent@ypos <- row
  x@world[[row, col]] <- c(x@world[[row, col]], list(agent))
  return(x)
}

######################### Move agents ############################
move_agents <- S7::new_generic("move_agents", dispatch_args = "x")

S7::method(move_agents, world) <- function(x) {
  size <- x@size
  world <- x@world
  new_world <- matrix(vector("list", size * size), nrow = size, ncol = size)
  for (i in 1:size) {
    for (j in 1:size) {
      agents <- world[[i, j]]
      for (agent in agents) {
        if (agent@state != "dead") {
          move <- rnorm(2, mean = 0, sd = 1)
          new_row <- min(max(1, round(i + move[1])), size)
          new_col <- min(max(1, round(j + move[2])), size)
          agent@xpos <- new_col
          agent@ypos <- new_row
          new_world[[new_row, new_col]] <- c(new_world[[new_row, new_col]], list(agent))
        } else {
          new_world[[i, j]] <- c(new_world[[i, j]], list(agent))
        }
      }
    }
  }
  x@world <- new_world
  return(x)
}

############################### Update states #################################

update_states <- new_generic("update_states", dispatch_args = "x")
S7::method(update_states, world) <- function(x, beta, recovery_prob, death_prob) {
  size <- x@size
  world <- x@world
  new_world <- matrix(vector("list", size * size), nrow = size, ncol = size)
  for (i in 1:size) {
    for (j in 1:size) {
      agents <- world[[i, j]]
      num_sick <- sum(vapply(agents, function(agent) agent@state == "sick", logical(1)))
      # Now, probability of becoming sick is naively proportional to percentage of 
      # already sick individuals in a cell
      sick_prob <- 1 - (1 - beta) ^ num_sick
      for (agent in agents) {
        agent <- update_state(agent, sick_prob, recovery_prob, death_prob)
        new_world[[i, j]] <- c(new_world[[i, j]], list(agent))
      }
    }
  }
  x@world <- new_world
  return(x)
}

################################## Get counts #################################
world_snapshot <- new_generic("world_snapshot", dispatch_args = "x")

S7::method(world_snapshot, world) <- function(x) {
  size <- x@size
  world <- x@world
  counts <- data.frame(
    row = integer(),
    col = integer(),
    healthy = integer(),
    immune = integer(),
    sick = integer(),
    dead = integer()
  )
  for (i in 1:size) {
    for (j in 1:size) {
      agents <- world[[i, j]]
      counts <- rbind(counts, data.frame(
        row = i,
        col = j,
        healthy = sum(vapply(agents, function(agent) agent@state == "healthy", logical(1))),
        immune = sum(vapply(agents, function(agent) agent@state == "immune", logical(1))),
        sick = sum(vapply(agents, function(agent) agent@state == "sick", logical(1))),
        dead = sum(vapply(agents, function(agent) agent@state == "dead", logical(1)))
      ))
    }
  }
  return(counts)
}

########################## Run Simulation ############################

run_simulation <- function(size, num_steps, initial_immune, initial_sick, beta, recovery_prob, death_prob) {
  world <- world(size)
  
  # Introduce healthy agents
  if (initial_healthy > 0) {
    for (i in 1:initial_immune) {
      row <- sample(1:size, 1)
      col <- sample(1:size, 1)
      world <- add_agent(world, row, col, agent())
    }
  }  
  # Introduce immune and sick agents
  if (initial_immune > 0) {
    for (i in 1:initial_immune) {
      row <- sample(1:size, 1)
      col <- sample(1:size, 1)
      a <- agent()
      a@state <- "immune"
      world <- add_agent(world, row, col, a)
    }
  }
  
  if (initial_sick > 0) {
    for (i in 1:initial_sick) {
      row <- sample(1:size, 1)
      col <- sample(1:size, 1)
      a <- agent()
      a@state <- "sick"
      world <- add_agent(world, row, col, a)
    }
  }
  
  results <- list()
  
  for (step in 1:num_steps) {
    world <- update_states(world, beta, recovery_prob, death_prob)
    world <- move_agents(world)
    counts <- world_snapshot(world)
    counts$step <- step
    results[[step]] <- counts
  }
  
  results <- do.call(rbind, results)
  
  return(results)
}

library(ggplot2)
library(gganimate)

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
  
  animate(p, nframes = length(unique(results$step)), fps = 2)
}



# Parameters
size <- 20                    # World size
num_steps <- 50               # Number of generations
initial_healthy <- 100
initial_immune <- 0
initial_sick <- 100
beta <- 0.7                  # Transmission probability
recovery_prob <- 0.9
death_prob <- 0.05

# Run the simulation
results <- run_simulation(size, num_steps, initial_immune, initial_sick, beta, recovery_prob, death_prob)

# Visualize the results
visualize_simulation(results)

library(tidyverse)
results |> group_by(step) |> summarise(infected = sum(sick))
# a1 <- agent()
# a2 <- agent()
# w <- world(s = 10)
# w <- add_agent(w, 1, 1, a1)
# w <- add_agent(w, 1, 1, a2)
# w <- move_agents(w)
