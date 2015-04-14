corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  files <- list.files(directory)
  coorelation_list <- c()
  id_list <- as.numeric(sub('\\.csv', '', files))
  
  for (i in 1:332) {
    if ( complete(directory, i)$nobs < threshold) {
      next
    }
    data <- read.csv(file.path(directory, files[match(i, id_list)]))
    coorelation_list <- c(coorelation_list, cor(data$sulfate, data$nitrate, use= "na.or.complete"))
  }
  coorelation_list
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
}