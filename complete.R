completeSub <- function(directory, id) 
  { 
  
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ##  to be used
    fileName <- paste(formatC(id, width = 3, flag = "0"), ".csv", sep = "") 
    filePath <- paste(directory, "/", fileName, sep = "") 
    frame <- read.csv(filePath) 
    obsClean <- na.omit(frame) 
    obsCleanCount <- nrow(obsClean) 
    list(id = id, nobs = obsCleanCount) 
  } 
 

complete <- function(directory, id = 1:332)
{ 
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  completeLists <- Map(function(id) completeSub(directory, id), id) 
  matrix <- do.call(rbind, completeLists) 
  as.data.frame(matrix) 
} 
