source("functions/ytm_and_duration.R")
get_input_for_weights = function(C_mat, B_vec){
  #Takes Cashflow and Prices and outputs yyt, dur and ttm
  number_of_bonds = length(B_vec)
  
  
  ytm = rep(0,number_of_bonds)
  dur = rep(0,number_of_bonds)
  ttm = rep(0,number_of_bonds) # time to maturity
  
  for (i in 1:number_of_bonds) {
    
    time_to_cashflow = which(C_mat[i,] != 0)
    ytm_and_dur = get_ytm_and_duration(cashflow = C_mat[i, time_to_cashflow],
                                       time_to_cashflow = time_to_cashflow,
                                       B_i =  B_vec[i])
    ytm[i] = ytm_and_dur$ytm_solved
    dur[i] = ytm_and_dur$dur_solved
    ttm[i] = max(time_to_cashflow)
  }
  return(list(Yield_to_maturity = ytm,
              Duration = dur,
              Time_to_maturity = ttm))
}


get_inv_weights = function(duration, B_vec){
  return((duration*B_vec)^2*length(B_vec))
}
