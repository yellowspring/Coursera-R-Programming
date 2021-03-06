best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  content <- read.csv('/home/osboxes/workspace/learningR/Coursera-R-Programming/Assignment3/outcome-of-care-measures.csv')
  states <- unique(content[,7],fromLast = FALSE)
  if ( ! state %in% states) {
    stop('invalid state')
  }
  
  if (! outcome %in% c("heart attack", "heart failure", "pneumonia") ) {
    stop('invalid outcome')
  }
  
  rv <- c()
  hp <- content[content$State == state,]
  if (outcome == 'heart attack') {
    rv <- content[content$State == state,][[11]]
  } else if (outcome == 'heart failure') {
    rv <- content[content$State == state,][[17]]
  } else if (outcome == 'pneumonia') {
    rv <- content[content$State == state,][[23]]
  }  
  rv <- suppressWarnings(as.numeric(as.character(rv)))
  min_val<- min(rv, na.rm=TRUE)
  index <- which(rv %in% c(min_val))

  hospital_names <- as.character(hp[[2]])[index]
  sort(hospital_names)[1]
}