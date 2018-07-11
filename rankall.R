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
  
  rankall <- data.frame()
  
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}