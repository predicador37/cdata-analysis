complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  #data <- data.frame(id=id, nobs= numeric(length(id)))
  
  nobs <- vector()
  
  j=1
  
  for (i in id) {
    file <- read.csv(paste(directory,"/",formatC(i, width = 3, format = "d", flag = "0"),".csv",sep=""))
    nobs[j] <- sum(complete.cases(file))
    j <- j+1
  }
 
  data = data.frame(id, nobs)
  data
}
