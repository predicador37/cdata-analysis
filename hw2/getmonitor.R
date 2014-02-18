getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  data = read.csv(paste(directory,"/",formatC(id, width = 3, format = "d", flag = "0"),".csv",sep=""))
  
  if (isTRUE(summarize)) {
    summary(data)
  }
  
  data
  
}