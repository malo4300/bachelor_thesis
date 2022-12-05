get_ytm_and_duration = function(cashflow,
                     time_to_cashflow,
                     B_i){
  if(length(cashflow) == length(time_to_cashflow)){
    ytm_func = function(y){
      return((sum(cashflow*exp((-time_to_cashflow/365)*y))-B_i)^2)
    }
    ytm_solved = optimize(ytm_func,
                         lower = 0,
                         upper = 1)$minimum
    
    dur_solved = sum((time_to_cashflow/365)*cashflow*exp(-time_to_cashflow/365*ytm_solved))/B_i
    
    return(list(ytm_solved = ytm_solved,
                dur_solved = dur_solved))   
  
  }else{
    print("Cashflow und time_to_cashflow sind nicht gleich lang")
    return(NA)
  }
}


