# This set of functions was used to calculate a z-score-index of social vulnerability based on four different metrics of socioeconomic status. # The z-score for each metric is calculated and then mean of each individual z-score is taken to give the z-score-index


# Function to compute the z-score for a single vector
# Can be used alone for just one variable or run to feed into the z-score index function below
z_score <- function(vec) {

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


