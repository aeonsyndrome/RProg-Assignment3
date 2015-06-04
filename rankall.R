rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  
  ## Check that outcome are valid
  validoutcome <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome,validoutcome)) stop("invalid outcome")
  
  outcomecolumn <- switch(outcome,
      "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
      "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
      "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  results <- data.frame("hospital"=character(),"state"=character(),stringsAsFactors=FALSE)
  
  ## For each state, find the hospital of the given rank
  for(i in 1:length(unique(data$State))) {
    localnum <- num
    state <- unique(data$State)[i]
    datastate <- data[data[,"State"]==state,] #Select only data in the right state
    
    ## Check for out of bounds
    ## if (num>length(unique(datastate$Hospital.Name))) return NA
    
    datastate <- datastate[,c("Hospital.Name","State",outcomecolumn)]
    names(datastate)[3]<-"Score"
    datastate[,3] <- suppressWarnings(as.numeric(datastate[,3]))
    
    ## Rank
    sorteddata <- datastate[order(datastate$Score,datastate$Hospital.Name,na.last=NA,decreasing=FALSE),]
    
    ## adapt for "best" and "worst
    if (localnum=="best") localnum <- 1
    if (localnum=="worst") localnum <- length(sorteddata[,3])
    
    ## Return
    results[nrow(results)+1,] <- c(sorteddata[localnum,1],state)
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  results <- results[order(results$state),]
  results
}