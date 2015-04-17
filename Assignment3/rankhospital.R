rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
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
    
  if (as.character(num) == 'best') {
    min_val<- min(rv, na.rm=TRUE)
    index <- which(rv %in% c(min_val))
    hospital_names <- as.character(hp[[2]])[index]
    return (sort(hospital_names)[1])
  } else if(as.character(num) == 'worst') {
    max_val<- max(rv, na.rm=TRUE)
    index <- which(rv %in% c(max_val))
    hospital_names <- as.character(hp[[2]])[index]
    return (sort(hospital_names, decreasing=TRUE)[1])
  } else {
    num <- as.numeric(num)
    if (is.na(num)) {
      stop('invalide num')
    }
    if (round(num) %% num != 0) {
      stop('invalid num')
    }
    if (num > length(rv)) {
      return(NA)
    }
    hos_name <- NA
    for(i in 1:num) {   
      min_val<- min(rv, na.rm=TRUE)
      index <- which(rv %in% c(min_val))
      hospital_names <- as.character(hp[[2]])[index]
      hos_name <- sort(hospital_names, decreasing=FALSE,na.last=TRUE)[1]
      cat('value:',min_val,'\n')
      index <- match(hos_name, hp[[2]])
      
      rv <- rv[-index]
      hp <- hp[-index,]
    }
    return(hos_name)   
  }
  
}


