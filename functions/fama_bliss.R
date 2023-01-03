#function that calculates the discount factor for solving for the forward rate. 
d_tau = function(tau, f, ttm, ttm_minus_one, f_K = 0){
  #ttm_minus_one indicates the place to which f is filled 
  #f_K is given by the solver 
  #tau is by use smaller or equal than the the current ttm
  sorted_ttm = sort(unique(ttm))
  if(ttm_minus_one == 0 ){ # get the first loop
    return(exp(-(f_K)*tau/365))
    
  }
  sum_for_output = 0 
  helper_time = 0
  whole_jumps = sum(tau>sorted_ttm) # number of jumps over a whole ttm range
  if(whole_jumps>0){ # check if the payment day is before the first ttm
    for(i in 1:whole_jumps){
      sum_for_output = sum_for_output + f[sorted_ttm[i]]*(sorted_ttm[i] - helper_time)/365
      helper_time = sorted_ttm[i]
    }
    if(tau>ttm_minus_one){#usage of the f_K. 
      sum_for_output = sum_for_output + f_K*(tau-ttm_minus_one)/365
    } else{
      sum_for_output = sum_for_output + f[tau]*(tau-sorted_ttm[whole_jumps])/365
    }
  } else{
    sum_for_output = sum_for_output + f[tau]*(tau)/365
  }
  return(exp(-sum_for_output))
}

function_for_nleq = function(forward_rate, # given by the solver
                             ttm, #unsorted 
                             f, 
                             ttm_minus_one, 
                             c_mat, 
                             current_ttm,
                             price_vec){
  bond_rows = as.vector(which(ttm == current_ttm)) # allows for duplicates in ttm
  discounted_values = rep(0,length(bond_rows))
  for (i in 1:length(bond_rows)) {
    current_bond_row = bond_rows[i]
    payment_days = which(c_mat[current_bond_row, ] != 0)
    for(j in 1:length(payment_days)){
      discounted_values[i] = discounted_values[i] + 
        c_mat[current_bond_row,payment_days[j]] * d_tau(tau = payment_days[j],
                                                        f= f, 
                                                        ttm = ttm, 
                                                        ttm_minus_one = ttm_minus_one,
                                                        f_K = forward_rate)
      
    }
  }
  
  return(sum(((price_vec[bond_rows]-discounted_values)^2)))
}



fb_solve = function(c_mat, 
                  price_vec, 
                  max_mat = 30*365){
  ttm = apply(c_mat, 1, which.max) # find maximum in each row
  sorted_unique_ttm = sort(unique(ttm), decreasing = F)
  # first assume uniqueness of time to maturity
  f = rep(0,max_mat)
  ttm_minus_one = 0
  for (i in 1:length(sorted_unique_ttm)) {
    current_ttm = sorted_unique_ttm[i]
    solved_fw_rate = nleqslv::nleqslv(x = 0, 
                                      fn = function(x) { #solve for new forward rate
                                      function_for_nleq(forward_rate = x,
                                                        ttm = ttm, 
                                                        ttm_minus_one = ttm_minus_one,
                                                        f = f,
                                                        c_mat = c_mat,
                                                        current_ttm = current_ttm, 
                                                        price_vec = price_vec)
                                      }, method = "Broyden")$x
    
    
    f[(ttm_minus_one+1):sorted_unique_ttm[i]] = solved_fw_rate
    ttm_minus_one = sorted_unique_ttm[i]
  }
  g = exp(-cumsum(f)/365)
  y = -(1/((seq(1,max_mat)/365)))*log(g)
  return(list(fw_rate = f, 
              g = g, 
              y = y))
}



