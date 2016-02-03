rankhospital<-function(state, outcome, num = "best"){
  
  best_results<-best(state, outcome)
  
  if(num == "best"){
    message("best")
    best_results[1,2]
  }
  
  if(num == "worst"){
    message("worst")
    best_results[nrow(best_results),2]
  }
  
  if(is.numeric(num)){
    message("number")
    best_results[,c(2,17)]
  }
  
}