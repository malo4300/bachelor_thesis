library(tidyverse)
library(nleqslv)
source("functions/functions_for_dgp.R")
d_tau = function(tau, f, ttm){
  sorted_ttm = sort(ttm)
  K = sorted_ttm[sorted_ttm <= min(sorted_ttm[tau <= sorted_ttm])] # get jumps
  sum_for_exp = 0
  K_minus_one = 0
  
  if(length(K)>1){
    for(i in 1:(length(K)-1)){
      sum_for_exp = sum_for_exp + f[K[i]]*(K[i]-K_minus_one)/365
      K_minus_one = K[i]
    }
  }
  sum_for_exp = sum_for_exp + f[max(K)]*(tau-K_minus_one)/365
  print(sum_for_exp)
  return(exp(-sum_for_exp))
  
}



fb_fit = function(c_mat, 
                  price_vec, 
                  max_mat = 30*365){
  ttm = apply(c_mat, 1, which.max) # find maximum in each row
  sorted_ttm = sort(ttm, decreasing = F)
  # first assume uniqueness of time to maturity
  f = rep(0,max_mat)
  ttm_minus_one = 0
  for (i in 1:length(ttm)) {
    current_ttm = sorted_ttm[i]
    current_bond_row = which(ttm == current_ttm)
    payment_days = which(c_mat[current_bond_row, ] != 0)
    
    function_for_nleq = function(forward_rate){
      check_sum = sum(payment_days<=ttm_minus_one)
      sum_of_known_part = 0
      if(check_sum>0){
        known_part = payment_days[payment_days<=ttm_minus_one]
          for(j in 1:length(known_part)){
            sum_of_known_part = sum_of_known_part + 
              c_mat[current_bond_row,known_part[j]] * d_tau(tau = known_part[j],
                                                            f = f, 
                                                            ttm = ttm)
          }
      }
      check_sum = sum(payment_days>ttm_minus_one)
      sum_of_unkown_part = 0
      if(check_sum>0){
        unkown_part = payment_days[payment_days>ttm_minus_one]
        future_cashflow = c_mat[current_bond_row,unkown_part]
        sum_of_exp_of_last_mat = log(d_tau(tau = ttm_minus_one,
                                           f = f, 
                                           ttm = ttm))
        
        sum_of_unkown_part = sum_of_unkown_part + 
          future_cashflow %*%  exp(-forward_rate*(unkown_part-ttm_minus_one)/365 + sum_of_exp_of_last_mat)
      
         
        
      }
      
      whole_sum = sum_of_unkown_part + sum_of_known_part
      
      return(abs(price_vec[current_bond_row] - whole_sum))
    }

    solved_fw_rate = optimize(function_for_nleq,  c(-1,1))$minimum
    if(i == 1){ # catch first loop
      f[1:sorted_ttm[i]] = solved_fw_rate
    }else{
      f[(sorted_ttm[i-1]+1):sorted_ttm[i]] = solved_fw_rate
    }
    
    ttm_minus_one = sorted_ttm[i]
  }
  return(f)
}
y_true = sample_yield_function(weights_function = weights_function)
plot(y_true)
portfolio = sample_bonds_portfolio(maturity_obj = mat_obj, yield_str = y_true, number_of_bonds = 15)
ttm = apply(portfolio$Cashflow, 1, which.max)
dub = duplicated(ttm)

fb_unique_C = portfolio$Cashflow[!dub,]
fb_unique_P = portfolio$Cashflow[!dub]

fb_ets = fb_fit(fb_unique_C, fb_unique_P)

plot(fb_ets[200:5000])
