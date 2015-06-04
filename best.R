# This function finds the best hospital in a state in regards to the 30-day mortality for heart attack, heart failure and pneumonia
# Hospitals that dont have data for the selected outcome should be excluded
# If there is a tie, hospitals should be sorted in alphabeticcal order and the first in that order should be chosen.

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!is.element(state,data$State)) stop("invalid state")
  validoutcome <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome,validoutcome)) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  data <- data[data[,"State"]==state,] #Select only data in the right state
  outcomecolumn <- switch(outcome,
      "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
      "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
      "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  ## Clean data
  data <- data[,c("Hospital.Name",outcomecolumn)]
  data[,2] <- suppressWarnings(as.numeric(data[,2]))
  
  ## rate
  bestidx <- which.min(data[,2])
  data[bestidx,]$Hospital.Name
}
