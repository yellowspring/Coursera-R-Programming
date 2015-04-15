best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  states <- unique(outcome[,7],fromLast = FALSE)
  if ( ! state %in% states) {
    stop('invalid state')
  }
  
  if (! outcome %in% c("heart attack", "heart failure", "pneumonia") ) {
    stop('invalid outcome')
  }
  
  
}