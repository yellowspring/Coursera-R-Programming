complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  path <- directory
  
  file_list <- list.files(path)
  id_list <- as.numeric(sub('\\.csv', '', file_list))

  row_num <- 1
  return_df <- data.frame(id= integer(0), nobs=integer(0)) 
  for(i in id) {
    selected_file <- file_list[match(i, id_list)]
    data <- read.csv(file.path(path,selected_file))
    nobs <- nrow(data[complete.cases(data), ])
    return_df[row_num, ] <- c(i, nobs) 
    row_num <- row_num + 1
  }
  return_df
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}