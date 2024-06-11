library(R6)

Agent <- R6Class("Agent",
                 public = list(
                   # Data
                   state = character(),
                   
                   # Methods
                   initialize = function(),
                   update_state = function()
                 )
)

World <- R6Class("World",
                 public = list(
                   # Data
                   size = NULL,
                   world = NULL,
                   
                   # Methods
                   initialize = function(size),
                   
                   add_agent = function(row, col, agent),
                   
                   move_agents = function(),
                   
                   update_states = function(beta, recovery_prob, death_prob),
                   
                   get_counts = function() 
                 )
)

library(ggplot2)
library(gganimate)

run_simulation <- function(size, 
                           num_steps, 
                           initial_immune, 
                           initial_sick, 
                           beta, 
                           recovery_prob, 
                           death_prob) {

  # Initialize agents

  # Introduce immune and sick agents

  # Update states
  
  # Move agents
  
  # Get counts and store them as a result

}

# Parameters
size <- 20                    # World size
num_steps <- 50               # Number of generations
initial_healthy <- 998
initial_immune <- 0
initial_sick <- 2
beta <- 0.5                  # Transmission probability
recovery_prob <- 0.1
death_prob <- 0.05

