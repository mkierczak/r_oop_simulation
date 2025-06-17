require(R6)

Agent <- R6Class("Agent",
  public = list(
    # Public data
    state = character(),
    
    # Public methods
    initialize = function(state = "naive") {
      self$state <- state
    },
    
    update_state = function(probs) {
      if (self$state == "naive") { # If agent is naive it can either stay naive or get infected
        new_state <- sample(x = c("naive", "sick"), 
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