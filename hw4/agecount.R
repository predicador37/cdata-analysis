agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if (is.null(cause)) {
    stop("Age cannot be NULL.")
  }
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  ## Extract ages of victims; ignore records where no age is
  ## given
  i <- length(grep(paste("", as.character(age), "years old") , homicides))
  ## Return integer containing count of homicides for that age
  i
}