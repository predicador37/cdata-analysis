best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #this is being harcoded because it's kind of a hell
  labels=list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  dataCols=sapply(labels, "[[",1)
  
  #Coerce chars to numeric and char to factor
  outcomeData[dataCols] <- lapply(outcomeData[dataCols],function(x) as.numeric(x))
  outcomeData$State <- as.factor(outcomeData$State)
  
  ## Check that state and outcome are valid
  #check state input
  if (!(state %in% levels(outcomeData$State))) {
    #state not in list, stop with error
    stop("invalid state")
  }
  else if(!(outcome %in% names(labels))){
    #outcome not in list, stop with error
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  #filter data by state
  outcomeData <- outcomeData[outcomeData$State == state,]
  #get data column
  hospital <- as.vector(na.omit(outcomeData$Hospital.Name[outcomeData[,labels[[outcome]]]==min(na.omit(outcomeData[,labels[[outcome]]]))]))
  
  #check for ties
  if (length(hospital)>1) {
    hospital=sort(hospital)
  }
  
  hospital[1]
}