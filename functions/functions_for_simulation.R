in_sample_results = function(y_true, 
                             mat_obj,
                             Kernel_Matrix,
                             penalty_for_KR = 1,
                             number_of_bonds = 250, 
                             noise = 1, 
                             max_maturity = 30*365){
  #Sample Bonds
  sample_data = sample_bonds_portfolio(maturity_obj = mat_obj,
                                       yield_str = y_true, 
                                       number_of_bonds = number_of_bonds,
                                       max_maturity = max_maturity, 
                                       noise = noise)
  C = sample_data$Cashflow
  B = sample_data$Price
  number_of_bonds = length(B)
  
  #fit KR
  #get yield and duration for weights
  portfolio_info = get_input_for_weights(C, B)
  true_portfolio_info = get_input_for_weights(C, sample_data$True_price)
  inv_w = get_inv_weights(portfolio_info$Duration, B)
  true_weights = get_inv_weights(true_portfolio_info$Duration, sample_data$True_price)
  #Fit model ----
  KR_Fit= KR_solve(C = C,
                   B = B,
                   ridge = penalty_for_KR,
                   inv_w = inv_w, 
                   K = Kernel_Matrix)
  
  #fit FB
  fb_fit = fb_solve(c_mat = C, 
                    price_vec = B, 
                    max_mat = N)
  
  
  KR_Error = calc_rmse(y_true = y_true,
                       y_est = KR_Fit$y,
                       c_mat = C, 
                       weights = 1/true_weights)
  
  FB_Error = calc_rmse(y_true = y_true,
                       y_est = fb_fit$y,
                       c_mat = C, 
                       weights = 1/true_weights)
  return(list("FB_in_sample_rmse" = FB_Error,
              "KR_in_sample_rmse" = KR_Error))
}
