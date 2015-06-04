# This function finds the best hospital in a state in regards to the 30-day mortality for heart attack, heart failure and pneumonia
# Hospitals that dont have data for the selected outcome should be excluded
# If there is a tie, hospitals should be sorted in alphabeticcal order and the first in that order should be chosen.

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  validoutcome <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!is.element(outcome,validoutcome)) stop("invalid outcome")
  if (!is.element(state,data$State)) stop("invalid state")
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
