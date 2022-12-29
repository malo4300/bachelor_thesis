source("functions/functions_for_dgp.R")
d_tau = function(tau, f, ttm, ttm_minus_one, f_K = 1){
  #ttm_minus_one gives the place to which f is filled 
  #determine the jumps
  sorted_ttm = sort(ttm)
  K = sum(ttm_minus_one>=sorted_ttm) 
  if(K<1){ # get the first loop
    return(exp(-(f_K)*tau/365))
    
  }
  sum_for_output = 0 
  helper_time = 0
  jumps_in_known = sum(ttm_minus_one>=sorted_ttm)
  print(jumps_in_known)
  for(i in 1:jumps_in_known){
    sum_for_output = sum_for_output + f[sorted_ttm[i]]*(sorted_ttm[i] - helper_time)/365
    helper_time = sorted_ttm[i]
  }
  if(tau>ttm_minus_one){#usage of the f_K
    sum_for_output = sum_for_output + f_K*(tau-ttm_minus_one)/365
  }
  return(exp(-sum_for_output))
}

function_for_nleq = function(forward_rate,
                             ttm, #unsorted
                             f, 
                             ttm_minus_one, 
                             c_mat, 
                             current_ttm,
                             price_vec){
  current_bond_row = which(ttm == current_ttm) # only works if there are no dublicates
  payment_days = which(c_mat[current_bond_row, ] != 0)
  discounted_value = 0
  for(j in 1:length(payment_days)){
    discounted_value = discounted_value + 
      c_mat[current_bond_row,payment_days[j]] * d_tau(tau = payment_days[j],
                                                      f= f, ttm = ttm, ttm_minus_one = ttm_minus_one,
                                                      f_K = forward_rate)
    
  }
  return((price_vec[current_bond_row]-discounted_value)^2)
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
    solved_fw_rate = optimize(function(x) { #solve for new forward rate
      function_for_nleq(x,
                        ttm = ttm, 
                        ttm_minus_one = ttm_minus_one,
                        f = f,
                        c_mat = c_mat,
                        current_ttm = current_ttm, 
                        price_vec = price_vec)
      }, interval =  c(-1,1))$minimum
    
    
    f[(ttm_minus_one+1):sorted_ttm[i]] = solved_fw_rate
    ttm_minus_one = sorted_ttm[i]
  }
  return(list(fw_rate = f, 
              g = exp(-cumsum(f)/365)))
}


