rankall <- function(outcome, num = "best") {

  ## Check that state and outcome are valid
  outcome <- tolower(outcome)
  if(outcome=="heart attack") outcomeCol <- 11
  else if (outcome=="heart failure") outcomeCol <- 17 
  else if (outcome=="pneumonia") outcomeCol <- 23
  else stop('invalid outcome')
  
  ## Read outcome data
  care <- read.csv('outcome-of-care-measures.csv')
  
  #Extract just the Hospital Name, State and Outcome column
  care <- care[,c(2,7,outcomeCol)]
  names(care) <- c("Hospital","State","Outcome")
  #Cast Outcome as numeric to avoid alphabetic sorting on its values
  care$Outcome <- as.numeric(as.character(care$Outcome))
  #Omit NAs
  #care <- na.omit(care)
  
  #Order the data by Outcome, Hospital
  care <- care[order(care$Outcome, care$Hospital, na.last=TRUE),]
  
  #Split out rows from the required state
  care <- split(care, care$State)
  
  print(length(care))
  
  df = data.frame()
  
  ## For each state, find the hospital of the given rank
  for(i in 1:length(care)) {

    #Get the number of hospitals reporting this outcome in this state
    reporting <- nrow(care[[i]])
    
    ## Add to a data frame with the hospital names and the
    ## (abbreviated) state name
    if(num=="best") 
      num<-as.numeric(1)
    else if (num=="worst") 
      num<-as.numeric(reporting)
    else if (as.numeric(num)>reporting) 
      num<-as.numeric(-1)
    else 
      num<-as.numeric(num)
    
    #print(care[[i]][num,])
    df = rbind(df, c(as.character(care[[i]][num,1]), care[[i]][num,2]))
  
  }

  names(df)<- c("Hospital", "State")
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df
  
}