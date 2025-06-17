require(R6)

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
        naive = integer(),
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
            naive = sum(vapply(agents, function(agent) agent$state == "naive", logical(1))),
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