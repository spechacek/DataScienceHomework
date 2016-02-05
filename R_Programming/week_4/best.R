#setwd("E:/Coursera/R_Programming/homework/week_4")

rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

best<-function(state,outcome){
  
  valid_outcomes<-c("heart attack", "heart failure", "pneumonia")
  
  if(is.na(match(outcome, valid_outcomes))){
    stop("invalid outcome")
  }
  
  #create dataframe for outcome index lookup
  outcomes_idx<-c(11,17,23)
  outcomes_lookup<-data.frame(valid_outcomes, outcomes_idx)
  
  outcome_idx<-outcomes_lookup[outcomes_lookup$valid_outcomes == outcome,2]
  #message("outcome_idx is ", outcome_idx)
  
  #rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #clean state data
  rawdata<-rawdata[!rawdata[,outcome_idx] == "Not Available",]
  
  #convert column to numeric for sorting
  rawdata[,outcome_idx]<-as.numeric(unlist(rawdata[,outcome_idx]))
  
  long_names<-names(rawdata)
  
  
  #get only data for provided state
  state_data<-rawdata[rawdata$State == state,] #alternate -> subset(outcome, State == 'AL')
  #message("row count: ", nrow(state_data))
  
  #if row count is 0, then probably couldn't find the state
  if(nrow(state_data) == 0){
    stop("invalid state")
  }
  
  ordered_data<-state_data[ order(state_data[,outcome_idx], state_data[,2]), ]
  #ordered_data<-min(clean_state_data[,outcome_idx])
  
  #ordered_data[1,2]
  ordered_data
}