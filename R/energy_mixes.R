# Function to randomly generate a mix of energy from different sources to meet the global energy demand
# Where energy is a vector of the different energy sources (i.e. s = solar, w = wind etc)
# Probs is the probability of drawing each type of energy - this can be adjusted based on global energy potential
# N is the amount of energy in TWh that you want to draw (i.e. the global energy demand)
# Each time a type of energy drawn it represents 1 TWh of energy from that source



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
