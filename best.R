best <- function(state,outcome){
  #Read outcome data
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ##Check that state and outcome are valid
  #validating the state and outcome
  if (!is.element(outcome,c("heart attack","heart failure","pneumonia"))){
    stop("invalid outcome")
  }
  if(!is.element(state,data[,"State"])){
    stop("invalid state")
  }
  
  ## setting the column number to sort the correct rate depengding on outcome
  if (outcome=="heart attack") { 
    colnum <- 11 }
  else if(outcome=="heart failure") {
    colnum <- 17 }
  else if(outcome=="pneumonia") {
    colnum <- 23 }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Subsetting hospitals in the state
  stateData<-data[data["State"]==state,]
  ## converting rates as numeric
  stateData[,colnum]<-as.numeric(stateData[,colnum])
  
  ## Getting lowest rate for corresponding column
  minimalRate<-min(stateData[,colnum],na.rm=TRUE)
  
  #Getting hospital(s) with lowest rate on corresponding column according to variable "outcome"
  cases<-stateData[complete.cases(stateData[,colnum]),]
  hospital<-cases[cases[,colname]==minimalRate,]
  
  #sorting the list if there are several hospitals with lowest rating
  hospital<-hospital[order(hospital[,"Hospital.Name"]),]
  
  #returning the name of the hospital
  if (length(hospital[,1])>1){
    return(hospital[1,"Hospital.Name"])
  }
  else{
    return(hospital$Hospital.Name)
  }
}