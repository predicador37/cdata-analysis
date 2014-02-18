count <- function(cause = NULL) {
  
  valid_options <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")

  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) {
    stop("Cause cannot be null.")
  }
  
  ## Check that specific "cause" is allowed; else throw error
  if (!(cause %in% valid_options)){
    stop("Cause not found in valid options.")
  }
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
 
  ## Extract causes of death
  i <- length(grep(paste("[cC]ause: ","[",toupper(substring(cause, 1, 1)),tolower(substring(cause, 1, 1)),"]",substring(cause, 2), sep="") , homicides))
  
  ## Return integer containing count of homicides for that cause
  i
}