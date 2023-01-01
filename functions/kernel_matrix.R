# Here the function for creating the kernel matrix is defined ----

create_kernel_mat = function(alpha, 
              delta, 
              n_row = 3650, # 365 day * 10 years 
              n_col = 0){
  if(alpha < 0 || delta < 0 || delta > 1){
    return(NA)
  }
  #create constants
  if(0<delta && delta <1){
    sqrt_d = sqrt(alpha^2 + 4*delta/(1-delta))
    l1 = (alpha - sqrt_d)/2
    l2 = (alpha + sqrt_d)/2
  } else{
    sqrt_d = l1 = l2 = 0
  }
  
  if(n_col == 0){
    n_col = n_row
  }
  #create empty matrix
  M = matrix(rep(0, n_col*n_row),
             nrow = n_row, 
             ncol = n_col)
  for (i in 1:n_row) {
    x_scaled = i/365
    y_grid = seq(1:n_col)/365
    
    #calculate max and min, vectorized
    max_xy = pmax(x_scaled, y_grid)
    min_xy = pmin(x_scaled, y_grid)
    
    #calculate matrix for different parameters
    # it is assumed that not both alpha and delta equal 0
    if(delta == 0){
      M[i,] = (-min_xy/alpha^2)*exp(-alpha*min_xy)+ 
        (2/alpha^3)*(1-exp(-alpha*min_xy))+
        (-min_xy/alpha^2)*exp(-alpha*max_xy)
    } else if(delta == 1){
      #assume alpha != 0
      M[i,] = (1/alpha)*(1-exp(-alpha*min_xy))
    } else{
      M[i,] = (-alpha/(delta*l2^2))*(1-exp(-l2*x_scaled)-exp(-l2*y_grid))+
        (1/alpha*delta)*(1-exp(-alpha*min_xy)) + 
        (1/(delta*sqrt_d))*(((l1/l2)^2)*exp(-l2*(x_scaled + y_grid)) - exp(-l1*(min_xy) - l2*(max_xy)))
    }

        
  }
  return(M)
}
  




# Solver for the Model, returns discount and yield curve
KR_solve = function(
    C, # the final cashflow matrix
    B, # the prices of the bonds
    ridge, # penalty coefficient
    inv_w, # the inverse weights
    K # the kernel matrix)
){
  #library(tidyverse)
  Nmax = nrow(K)
  Nmax_y = ncol(K)
  #break down cashflow matrix to only those were there is not zero cashflow
  non_zero_casflow_idx = (1:ncol(C))[which(colSums(C) != 0)] #Col names not always provided as integers
  max_time = tail(non_zero_casflow_idx,1)
  
  #scale the penalty as suggested for comparison across the time-series of bonds
  ridge_scaled = ridge/max_time
  
  
  #create matrix with only those days on which cash flow occured
  K_on_c_days = K[non_zero_casflow_idx,non_zero_casflow_idx]
  C_on_c_days = C[,non_zero_casflow_idx]
  number_days = length(non_zero_casflow_idx)
  #scale days to years
  x_sclaed = non_zero_casflow_idx/365
  
  #solve for beta
  CKC = C_on_c_days%*%K_on_c_days%*%t(C_on_c_days) + ridge_scaled*inv_w*diag(1, length(B), length(B))
  CKC_inv = solve(CKC)
  
  beta = (t(C_on_c_days)%*%CKC_inv)%*%(B-C_on_c_days%*%rep(1,number_days))
  
  g_solved = 1+K[, non_zero_casflow_idx]%*%beta
  y_solved = -log(g_solved)/(seq(1,Nmax)/365)
  
  return(data.frame(g = g_solved,
              y = y_solved))
}
