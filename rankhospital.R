rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  myState <- toupper(state)

    ## Check that state and outcome are valid
  if(is.na(match(myState,state.abb))) {
    stop('invalid state')
  }
  
  outcome <- tolower(outcome)
  if(outcome=="heart attack") outcomeCol <- 11
  else if (outcome=="heart failure") outcomeCol <- 17 
  else if (outcome=="pneumonia") outcomeCol <- 23
  else stop('invalid outcome')
  
  ## Read outcome data
  care <- read.csv('outcome-of-care-measures.csv')

  #Extract just the Hospital Name, State and Outcome column, omit NAs
  care <- care[,c(2,7,outcomeCol)]
  names(care) <- c("Hospital","State","Outcome")
  care$Outcome <- as.numeric(as.character(care$Outcome))
  care <- na.omit(care)
  
  #Order the data by Outcome and Hospital
  care <- care[order(care$Outcome, care$Hospital, na.last=TRUE),]
  
  #Split out rows from the required state
  care <- split(care, care$State)
  
  #Get the number of hospitals reporting this outcome in this state
  reporting <- nrow(care[[myState]])
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if(num=="best") as.character(care[[myState]][1,1])
  else if (num=="worst") as.character(care[[myState]][reporting,1]) 
  else if (as.numeric(num)>reporting) NA
  else as.character(care[[myState]][as.numeric(num),1])
  
}