outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
cols=c(11,17,23)
outcome[cols] <- lapply(outcome[cols],function(x) as.numeric(x))
mapply(hist, as.data.frame(outcome[cols]),  main=gsub("Rates from", "", gsub("\\.", " ", lapply(strsplit(colnames(outcome[cols]),"\\.\\."),"[[",length(cols)))), xlab="30-day Death Rate", xlim=rep(list(range(na.omit(outcome[cols]))),3))
