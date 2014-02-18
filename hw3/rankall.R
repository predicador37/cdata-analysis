rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  #this is being harcoded because it's kind of a hell
  labels=list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  dataCols=sapply(labels, "[[",1)
  #Coerce chars to numeric and char to factor
  outcomeData[dataCols] <- lapply(outcomeData[dataCols],function(x) as.numeric(x))
  outcomeData$State <- as.factor(outcomeData$State)
  ## Check that outcome id valid
 if(!(outcome %in% names(labels))){
    #outcome not in list, stop with error
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  #get only relevant columns: hospital name, state and outcome
  filteredData <- outcomeData[,c(which(names(outcomeData)=="Hospital.Name"),which(names(outcomeData)=="State"), labels[[outcome]])]
  
  #split data by state: the result is a list of dataframes
  splittedData <- split(filteredData,filteredData$State)
  
  if (num=="best"){
    hospital <- lapply(splittedData, function (x) na.omit(x[x[3]==min(na.omit(x[3]))]))
  }
  else if (num=="worst"){
    hospital <- lapply(splittedData, function (x) na.omit(x[x[3]==max(na.omit(x[3]))]))
  }
  else if (num>nrow(outcomeData)){
    hospital <- NA
  }
  else { #num is integer
    #order data frame by outcome
    hospital <- lapply(splittedData, function (x) x[order(x[3], x[1]),])
    #get numth row from each data frame
    hospital <- lapply(hospital, function(x) x[as.numeric(num), ,drop=FALSE])
  }
  #create return structure: data frame with two variables and proper column names
  hospitals<-structure(as.data.frame(do.call(rbind,hospital)), names=c('hospital','state','outcome'))
  
  #get rid of NAs using states stored in row names
  hospitals$state[is.na(hospitals$state)] <- rownames(hospitals[is.na(hospitals$state),]) 
  
  #return desired output
  hospitals[,c(1,2)]
}