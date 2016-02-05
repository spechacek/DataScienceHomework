rankall <- function(outcome, num = "best") {
  ## Read outcome data
  valid_outcomes<-c("heart attack", "heart failure", "pneumonia")
  
  if(is.na(match(outcome, valid_outcomes))){
    stop("invalid outcome")
  }
  
  #create dataframe for outcome index lookup
  outcomes_idx<-c(11,17,23)
  outcomes_lookup<-data.frame(valid_outcomes, outcomes_idx)
  
  outcome_idx<-outcomes_lookup[outcomes_lookup$valid_outcomes == outcome,2]
  #message("outcome_idx is ", outcome_idx)
  
  rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #get all of the states
  states<-unique(rawdata[,7])
  
  results_dataframe<-data.frame()
  
  for(s in 1:length(states)){
    if(num == "best"){
      results_dataframe<-rbind(results_dataframe, best(states[s], outcome)[1,c(2,7,outcome_idx)])
    }
    
    if(num == "worst"){
      b<-best(states[s], outcome)
      results_dataframe<-rbind(results_dataframe, b[nrow(b),c(2,7,outcome_idx)])
    }
    
    if(is.numeric(num)){
      b<-best(states[s], outcome)
      results_dataframe<-rbind(results_dataframe, b[num,c(2,7,outcome_idx)])
    }
    
    #results_dataframe<-rbind(results_dataframe, best(states[s], outcome))
  }
  
  results_dataframe[order(results_dataframe[,2]),]
}



















