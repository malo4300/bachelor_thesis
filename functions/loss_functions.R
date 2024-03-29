#calculates the true rmse for the estimated and true yield curve
calc_true_rmse = function(y_true, y_est, c_mat, weights = 1){
  number_of_bonds = nrow(c_mat)
  N = ncol(c_mat)
  g_true = exp(-y_true*(1:N)/365)
  g_est = exp(-y_est*(1:N)/365)
  true_price = rep(0,number_of_bonds)
  pred_price = rep(0,number_of_bonds)
  for (i in 1:number_of_bonds) {
    days_with_payment = which(c_mat[i,]!=0)
    true_price[i] = c_mat[i, days_with_payment] %*% g_true[days_with_payment]
    pred_price[i] = c_mat[i, days_with_payment] %*% g_est[days_with_payment]
  }
  return(sqrt(weights%*%(true_price-pred_price)^2))
}
#calculates the observed rmse
calc_obs_rmse = function(prices_obs, y_est, c_mat, weights = 1){
  number_of_bonds = nrow(c_mat)
  N = ncol(c_mat)
  g_est = exp(-y_est*(1:N)/365)
  pred_price = rep(0,number_of_bonds)
  for (i in 1:number_of_bonds) {
    days_with_payment = which(c_mat[i,]!=0)
    pred_price[i] = c_mat[i, days_with_payment] %*% g_est[days_with_payment]
  }
  return(sqrt(weights%*%(prices_obs-pred_price)^2))
}

calc_errors = function(y_true,
                       y_fit, 
                       C, 
                       B,
                       true_inv_weights,
                       obs_inv_w){
  
  Error_true = calc_true_rmse(y_true = y_true,
                              y_est = y_fit,
                              c_mat = C,
                              weights = 1/true_inv_weights)
  
  #Calculate observed RMSE, the difference can be seen as an indicator of overfitting
  Error_obs = calc_obs_rmse(prices_obs = B, 
                               y_est = y_fit,
                               c_mat = C,
                               weights = 1/obs_inv_w)
  
  
  
  return(list("True_RSME" = Error_true,
              "Obs_RSME" = Error_obs))
}
