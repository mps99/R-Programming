
# collect all the files in to pollutant data frame by passing the direcotry location for the files.Remove all the NA entries
pollutants <- function(directory, pollutant, id)
  { 
    fileName <- paste(formatC(id, width = 3, flag = "0"), ".csv", sep = "") 
    filePath <- paste(directory, "/", fileName, sep = "") 
    frame <- read.csv(filePath) 
    pollutantData <- frame[[pollutant]] 
    pollutantData[!is.na(pollutantData)] 
  } 

# take the mean of all the records for which the pollutant value is not NA,passed in the function call
pollutantmean <- function(directory, pollutant, id = 1:332)
 { 
    pollutantLists <- sapply(id, function(id) pollutants(directory, pollutant, id)) 
    pollutants <- unlist(pollutantLists) 
    mean(pollutants) 
 }
