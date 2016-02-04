rankhospital<-function(state, outcome, num = "best"){
  
  best_results<-best(state, outcome)
  
  valid_outcomes<-c("heart attack", "heart failure", "pneumonia")
  #create dataframe for outcome index lookup
  outcomes_idx<-c(11,17,23)
  outcomes_lookup<-data.frame(valid_outcomes, outcomes_idx)
  outcome_idx<-outcomes_lookup[outcomes_lookup$valid_outcomes == outcome,2]
  
  ordered_results<-best_results[order(best_results[,outcome_idx], best_results[,2]),]
  
  output<-NULL
  
  if(num == "best"){
    #message("best")
    output<-ordered_results[1,2]
  }
  
  if(num == "worst"){
    #message("worst")
    output<-ordered_results[nrow(best_results),2]
  }
  
  if(is.numeric(num)){
    #message("number")
    #ordered_results[,c(2,17)]
    output<-ordered_results[num,2]
  }
  
  output
  
}