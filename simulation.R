init_simulation <- function(size, initial) {
  world <- World$new(size) # create the world
  
  # Initialize agents
  for (state in c('naive', 'immune', 'sick')) {
    if (initial[state] > 0) {
      for (i in 1:initial[state]) {
        row <- sample(1:size, 1)
        col <- sample(1:size, 1)
        world$add_agent(row, col, Agent$new(state = state))
      }
    }
  }
  return(world)
}

run_step <- function(world, probs) {
  world$update_states(probs)
  world$move_agents()
  return(world)
}

