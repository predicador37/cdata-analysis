corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
 
  cr <- vector()
  j=1
  
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  datasets <- lapply(filenames, read.csv)
  
  for (dataset in datasets){ 
    if (sum(complete.cases(dataset)) > threshold){
     
    cr[j]=cor(dataset[,c("sulfate")],dataset[,c("nitrate")],use="complete.obs")
    j <- j+1
    }
    
    if(length(cr)==0) {
      cr = numeric(0)
    }
    
  }
  
 cr
 
}