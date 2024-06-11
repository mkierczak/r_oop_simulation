library(R6)

Agent <- R6Class("Agent",
                 public = list(
                   state = character(),
                   initialize = ,
                   update_state = 
                 )
)

World <- R6Class("World",
                 public = list(
                   size = NULL,
                   world = NULL,
                   
                   initialize = function(size),
                   
                   add_agent = function(row, col, agent),
                   
                   move_agents = function(),
                   
                   update_states = function(beta, recovery_prob, death_prob),
                   
                   get_counts = function() 
                 )
)

library(ggplot2)
library(gganimate)

run_simulation <- function(size, num_steps, initial_immune, initial_sick, beta, recovery_prob, death_prob) {
  world <- World$new(size)
  
  # Initialize agents

  # Introduce immune and sick agents

  # Update states
  
  # Move agents
  
  # Get counts and store them as a result


# Parameters
size <- 20                    # World size
num_steps <- 50               # Number of generations
initial_healthy <- 998
initial_immune <- 0
initial_sick <- 2
beta <- 0.5                  # Transmission probability
recovery_prob <- 0.1
death_prob <- 0.05

# Run the simulation
results <- run_simulation(size, num_steps, initial_immune, initial_sick, beta, recovery_prob, death_prob)

library(tidyverse)
results |> group_by(step) |> 
  summarise(n_sick = sum(sick), n_healthy = sum(healthy), n_immune = sum(immune), n_dead = sum(dead)) |> 
  pivot_longer(starts_with("n_"), names_to = 'measure') |> 
  ggplot(aes(x = step, y = value, col=measure)) + geom_line() + theme_minimal()

# Visualize the results
visualize_simulation(results)


#results %>% filter(step == 1) %>% ggplot(aes(x = col, y = row)) +
geom_tile(aes(fill = sick)) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal()
