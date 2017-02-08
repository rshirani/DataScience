getwd()
#setwd("RScripts\\rprog2Fdata2FProgAssignment3-data")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
#outcome[,11] <- as.numeric(outcome[,11])

#par(mar = rep(2, 4))
#hist(outcome[,11])

best <- function(state, outcome) {
  ## Read outcome data
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(outcomeFile$State)
  diseases <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% states)) stop()
  if(!(outcome %in% diseases)) stop()
  
  ## Return hospital name in that state with lowest 30-day death rate
  stateDF <- subset(outcomeFile, outcomeFile$State == state)
  
  if(outcome == diseases[1])
  {
    minRatePerState <- tapply(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, stateDF$State, min)
    t1 <- subset(stateDF, stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minRatePerState[state]) 
  }
  else if(outcome == diseases[2])
  {
    minRatePerState <- tapply(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, stateDF$State, min)
    t1 <- subset(stateDF, stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == minRatePerState[state]) 
  }
  else 
  {
    minRatePerState <- tapply(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, stateDF$State, min)
    t1 <- subset(stateDF, stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == minRatePerState[state]) 
  }
  
  min(t1$Hospital.Name)
}


rankhospital <- function(state, outcome, num = "best") 
{
  ## Read outcome data
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(outcomeFile$State)
  diseases <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% states)) stop()
  if(!(outcome %in% diseases)) stop()
  

  ## Return hospital name in that state with the given rank ## 30-day death rate
  
  stateDF <- subset(outcomeFile, outcomeFile$State == state)
  
  if(outcome == diseases[1])
  {
    sortedRatePerState <- tapply(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, stateDF$State, sort)
    t1 <- subset(stateDF, stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == sortedRatePerState[[1]][num]) 
  }
  else if(outcome == diseases[2])
  {
    sortedRatePerState <- tapply(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, stateDF$State, sort)
    t1 <- subset(stateDF, stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == sortedRatePerState[[1]][num]) 
  }
  else 
  {
    sortedRatePerState <- tapply(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, stateDF$State, sort)
    t1 <- subset(stateDF, stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == sortedRatePerState[[1]][num]) 
  }
  
  min(t1$Hospital.Name)
}



rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(outcomeFile$State)
  diseases <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(outcome %in% diseases)) stop()
  
  
  decreasing = FALSE
  if (num == "best") num = 1
  else if (num == "worst")
  {
    num = 1
    decreasing = TRUE
  }
  else if (!is.numeric(num)) stop()
  else message("num is a valid integer")
  
  ## For each state, find the hospital of the given rank
  hospital = c()
  
  for(i in 1:length(states))
  {
    state = states[i]
    stateDF <- subset(outcomeFile, outcomeFile$State == state)
    
    if(outcome == diseases[1]) #heart attack
    {
      sortedRatePerState <- tapply(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, stateDF$State, sort, decreasing)
      charRatePerState <- ifelse (sortedRatePerState[[1]] == "Not Available", NA, sortedRatePerState[[1]])
      validSortedRatePerState <- sort(as.numeric(charRatePerState), decreasing)
      t1 <- subset(stateDF, as.numeric(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == validSortedRatePerState[num]) 
    }
    else if(outcome == diseases[2]) #heart failure
    {
      sortedRatePerState <- tapply(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, stateDF$State, sort, decreasing)
      charRatePerState <- ifelse (sortedRatePerState[[1]] == "Not Available", NA, sortedRatePerState[[1]])
      validSortedRatePerState <- sort(as.numeric(charRatePerState), decreasing)
      t1 <- subset(stateDF, as.numeric(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == validSortedRatePerState[num]) 
    }
    else #pneumonia
    {
      sortedRatePerState <- tapply(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, stateDF$State, sort)
      charRatePerState <- ifelse (sortedRatePerState[[1]] == "Not Available", NA, sortedRatePerState[[1]])
      validSortedRatePerState <- sort(as.numeric(charRatePerState), decreasing)
      t1 <- subset(stateDF, as.numeric(stateDF$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == validSortedRatePerState[num]) 
    }
    
    hospitalName = NA
    if (length(t1$Hospital.Name)>0) hospitalName = min(t1$Hospital.Name)
    
    hospital= c(hospital, hospitalName)
  }
  
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  state = states
  data.frame(hospital, state)
}
