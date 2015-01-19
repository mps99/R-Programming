## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
corrSub <- function(directory, id) 
{ 
       fileName <- paste(formatC(id, width = 3, flag = "0"), ".csv", sep = "") 
       filePath <- paste(directory, "/", fileName, sep = "") 
       frame <- read.csv(filePath) 
       na.omit(frame) 
 } 
 
## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

corr <- function(directory, threshold = 0) 
{ 
      frames <- Map(function(threshold) corrSub(directory, threshold), 1:332) 
      framesAboveThreshold <- Filter(function(frame) dim(frame)[1] > threshold, frames) 
      correlationList <- Map(function(frame) 
      { cor(frame$sulfate, frame$nitrate)}, framesAboveThreshold) 
      correlations <- unlist(correlationList) 
      ## Return a numeric vector of correlations
      if (length(correlations) > 0) correlations 
      else vector("numeric", 0) 
} 
