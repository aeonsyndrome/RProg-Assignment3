# This function finds the best hospital in a state in regards to the 30-day mortality for heart attack, heart failure and pneumonia
# Hospitals that dont have data for the selected outcome should be excluded
# If there is a tie, hospitals should be sorted in alphabeticcal order and the first in that order should be chosen.

rankhospital <- function(state, outcome,num="best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!is.element(state,data$State)) stop("invalid state")
  validoutcome <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome,validoutcome)) stop("invalid outcome")
  
  ## Clean data
  data <- data[data[,"State"]==state,] #Select only data in the right state
  outcomecolumn <- switch(outcome,
                          "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                          "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                          "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  data <- data[,c("Hospital.Name","State",outcomecolumn)]
  names(data)[3]<-"Score"
  data[,3] <- suppressWarnings(as.numeric(data[,3]))
  
  ## Rank
  sorteddata <- data[order(data$Score,data$Hospital.Name,na.last=NA,decreasing=FALSE),]
  print(head(sorteddata))
  
  ## adapt for "best" and "worst
  if (num=="best") num <- 1
  if (num=="worst") num <- length(sorteddata[,3])
  
  ## Return
  sorteddata[num,]$Hospital.Name
}
