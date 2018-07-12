## setwd("C:/Users/RADERAS/Documents/Cursos/Data Science - Johns Hopkins - Coursera/prog3")


rankall <- function(outcome,num = "best"){
  
  ## Read outcome data
  
  #Read outcome data
  
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  
  
  ## Check that outcome is valid
  
  if (!is.element(outcome,c("heart attack","heart failure","pneumonia"))){
    
    stop("invalid outcome")
    
  }
  
  
  
  ## setting the column number to sort the correct rate depengding on outcome
  
  if (outcome=="heart attack") { 
    
    colnum <- 11 }
  
  else if(outcome=="heart failure") {
    
    colnum <- 17 }
  
  else if(outcome=="pneumonia") {
    
    colnum <- 23 }
  
  
  
  ## For each state, find the hospital of the given rank
  
  ## getting the list of states from the data
  
  stateList <- levels(as.factor(data[,"State"]))
  
  
  result <- data.frame(
    hospital=character(),
    state=character(),
    stringsAsFactors = FALSE
  )
  browser()
  resultList<-lapply(stateList,function(state){
    ##browser()
    stateData <- data[data["State"]==state,]
    stateData <- stateData[complete.cases(stateData[,colnum]),]
    sortedStateData <- stateData[order(stateData[,colnum],stateData[,"Hospital.Name"]),]
    sortedStateData <- cbind("Rank"=1:length(stateData[,1]),sortedStateData)
    
    if(num=="best"){
      
      num<-1}
    
    else if(num=="worst"){
      
      num <- length(sortedStateData[,"Hospital.Name"])}
    
    else if(num > length(sortedStateData[,"Hospital.Name"])){
      
      return(as.list(c(state,"NA")))} 
    
    else {
      
      num <- as.integer(num)
      
    }
    hospital <- sortedStateData[sortedStateData[,"Rank"]==num,]
    ##browser()
    c(hospital[1,"Hospital.Name"],hospital[1,"State"])
    ##hospital[,c("Hospital.Name","State")]
  })
  
  ## Return a data frame with the hospital names and the
  
  ## (abbreviated) state name
  result
}
