# uses the package Package hash; installation with "install.packages("hash")"
library(hash)

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ## Check that state and outcome are valid
  
  # 1. Check State
  states <- unique(data$State)
  if ( !is.element(state,states) ) {
    stop("invalid state")
  }
  message("Parameter \"state\" is valid")
  
  # 2. Check outcome
  mort30day <- hash(
    "heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  ) 
  
  if ( !has.key(outcome,mort30day)) {
    stop("invalid outcome")
  }
  message("Parameter \"outcome\" is valid")
  
  # outcome and state are valid 
  
  # reduce data to the data of the required state
  dataOfState <- subset(data,data$State==state)
  message("Prepared subset of data based on the parameter \"state\"")
  
  # switch Values from charater to numeric (to avoid NA-coersion)
  dataOfState[,11]<- suppressWarnings(as.numeric (dataOfState[,11]))
  dataOfState[,17]<- suppressWarnings(as.numeric (dataOfState[,17]))
  dataOfState[,23]<- suppressWarnings(as.numeric (dataOfState[,23]))
  
  # sorting dataOfState in case, several hospitals have the same value
  dataOfState <- dataOfState[order(dataOfState$Hospital.Name),] 
  
  ## Return hospital name in that state with lowest 30-day death rate
  if ( outcome == "heart attack" ) {
    return(dataOfState$Hospital.Name[which.min(dataOfState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)])
  }
  if ( outcome == "heart failure" ) {
    return(dataOfState$Hospital.Name[which.min(dataOfState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)])
  }
  if ( outcome == "pneumonia" ) {
    return(dataOfState$Hospital.Name[which.min(dataOfState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)])
  }

}