#' Function to randomly generate a mix of energy from different sources to meet the global energy demand
#'
#' @param energy a vector of the names of different energy sources (i.e. s = solar, w = wind etc)
#' @param demand the amount of energy (in TWh) that you want to draw (i.e. the global demand)
#' @param probs the probability of drawing each type of energy based on global energy potential for each source
#' @return out a data frame consisting of the following items
#' \describe{
#' \item{energy_type}{The different energy sources}
#' \item{TWh}{The number of TWh provided by each energy source}
#' \item{percent_of_demand}{The percent of total demand that comes from each energy source}
#' }



energy_mixes <- function(energy, demand, probs){
  library(tidyverse)
  if(length(probs) != length(energy)){
    return("length of probabilities must match energy types")
  }

  if(demand < 0){
    return("energy demand must be positive")
  }

  out <- sample(energy, size = demand, replace = TRUE, prob = probs) # randomly draw a mix of energy to meet global demand
  return(as.data.frame(out) %>%
           rename(energy_type = "out") %>%
           group_by(energy_type) %>% # determine the amount of energy in TWh from each source
           tally() %>%
           rename(TWh = "n") %>%
           mutate(percent_of_demand = round(((TWh/23000)*100), 2))) # calculate what percent of the demand comes from each source
}
