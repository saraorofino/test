#' This function calculates a z-score based on a single vector
#'
#' @param vec a vector of numbers
#' @return z the z-score for each number in the vector


zscore <- function(vec) {

  if(length(vec) == 1){
    return("input must be a vector") #Return error message if the input is a single value
  }

  # Can only use on numeric variables
  if(is.numeric(vec)==T){
    vec <- vec
  } else {
    vec <- as.numeric(vec)
  }
  u <- mean(vec, na.rm=T) # find the mean of the vector
  stdev <- sd(vec, na.rm=T) # find the standard deviation of the vector

  z <- (vec-u)/stdev # calculate z-score for each number in the vector
  return(z)
}


