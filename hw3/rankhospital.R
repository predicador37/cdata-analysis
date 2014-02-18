rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #this is being harcoded because it's kind of a hell
  labels=list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  dataCols=sapply(labels, "[[",1)
  
  #Coerce chars to numeric and char to factor
  outcomeData[dataCols] <- lapply(outcomeData[dataCols],function(x) as.numeric(x))
  outcomeData$State <- as.factor(outcomeData$State)
  
  ## Check that state and outcome are valid
  if (!(state %in% levels(outcomeData$State))) {
    #state not in list, stop with error
    stop("invalid state")
  }
  else if(!(outcome %in% names(labels))){
    #outcome not in list, stop with error
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  #filter data by state
  outcomeData <- outcomeData[outcomeData$State == state,]
  
  if (num=="best"){
    hospital <- as.vector(na.omit(outcomeData$Hospital.Name[outcomeData[,labels[[outcome]]]==min(na.omit(outcomeData[,labels[[outcome]]]))]))
  }
  else if (num=="worst"){
    hospital <- as.vector(na.omit(outcomeData$Hospital.Name[outcomeData[,labels[[outcome]]]==max(na.omit(outcomeData[,labels[[outcome]]]))]))
  }
  else if (num>nrow(outcomeData)){
    hospital <- NA
  }
  else { #num is integer
    hospital <- as.vector(outcomeData$Hospital.Name[order(outcomeData[labels[[outcome]]], outcomeData$Hospital.Name,na.last=NA)])
    hospital <- hospital[as.numeric(num)]
  }
  
  hospital
}