rankhospital <- function(state,outcome,num = "best"){
  ## Read outcome data
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  stateData <- data[data["State"]==state,]
  stateData[,colnum]<-as.numeric(stateData[,colnum])
  stateData<-stateData[complete.cases(stateData[,colnum]),]
  sortedStateData <- stateData[order(stateData[,colnum],stateData[,"Hospital.Name"]),]
  sortedStateData <- cbind("Rank"=1:length(stateData[,1]),sortedStateData)
  
  if(num=="best"){
    num<-1}
  else if(num=="worst"){
    num <- length(sortedStateData[,"Hospital.Name"])}
  else if(num > length(sortedStateData[,"Hospital.Name"])){
    return(NA)} 
  else {
    num <- as.integer(num)
  }
  
  hospital <- sortedStateData[sortedStateData[,"Rank"]==num,]
  
  return(hospital$Hospital.Name)
}