#calculates the rmse for the estimated and true yield curve
calc_rmse = function(y_true, y_est, c_mat, weights = 1){
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
  return(sqrt(sum(weights%*%(true_price-pred_price)^2)))
}


