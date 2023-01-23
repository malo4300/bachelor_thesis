in_sample_results_smooth_FB = function(y_true, 
                             mat_object,
                             Kernel_Matrix,
                             penalty_for_KR = 1,
                             number_of_bonds = 250, 
                             noise = 1, 
                             max_maturity = 30*365,
                             filter_90 = T){
  #Sample Bonds
  sample_data = sample_bonds_portfolio(maturity_obj = mat_object,
                                       yield_str = y_true, 
                                       number_of_bonds = number_of_bonds,
                                       max_maturity = max_maturity, 
                                       noise = noise, 
                                       filter_90 = filter_90)
  C = sample_data$Cashflow
  B = sample_data$Price
  number_of_bonds = length(B)
  
  #fit KR
  #get yield and duration for weights
  portfolio_info = get_input_for_weights(C, B)
  true_portfolio_info = get_input_for_weights(C, sample_data$True_price)
  obs_inv_w = get_inv_weights(portfolio_info$Duration, B)
  true_inv_weights = get_inv_weights(true_portfolio_info$Duration, sample_data$True_price)
  #Fit model ----
  KR_Fit= KR_solve(C = C,
                   B = B,
                   ridge = penalty_for_KR,
                   inv_w = obs_inv_w, #model is fit with observed weights
                   K = Kernel_Matrix)
  
  #fit FB
  fb_fit = fb_solve(c_mat = C, 
                    price_vec = B, 
                    max_mat = N)
  smooth_fb = smooth.spline(x = sample_data$Maturity, 
                            y = fb_fit$y[sample_data$Maturity], 
                            cv = T)
  sm_fb_y = predict(smooth_fb, x = seq(1,N))
  #Calculate RMSE
  in_sample_error_KR = calc_in_sample_error(y_true = y_true,
                                            y_fit = KR_Fit$y, 
                                            C = C,
                                            B = B,
                                            true_inv_weights = true_inv_weights, 
                                            obs_inv_w = obs_inv_w)
  
  in_sample_error_FB = calc_in_sample_error(y_true = y_true,
                                            y_fit = sm_fb_y$y, 
                                            C = C,
                                            B = B,
                                            true_inv_weights = true_inv_weights, 
                                            obs_inv_w = obs_inv_w)
  
  return(list("KR_Results" = in_sample_error_KR,
              "FB_Results" = in_sample_error_FB))
  
  
}

out_sample_results_smooth_FB =function(y_true,
                             y_new, 
                             mat_object,
                             Kernel_Matrix,
                             penalty_for_KR = 1,
                             number_of_bonds = 250, 
                             noise = 1, 
                             max_maturity = 30*365,
                             filter_90 = T){
  #Sample Bonds
  sample_data = sample_bonds_portfolio(maturity_obj = mat_object,
                                       yield_str = y_true, 
                                       number_of_bonds = number_of_bonds,
                                       max_maturity = max_maturity, 
                                       noise = noise, 
                                       filter_90 = filter_90)
  C = sample_data$Cashflow
  B = sample_data$Price
  number_of_bonds = length(B)
  
  #fit KR
  #get yield and duration for weights
  portfolio_info = get_input_for_weights(C, B)
  obs_inv_w = get_inv_weights(portfolio_info$Duration, B)
  
  #Fit model ----
  KR_Fit= KR_solve(C = C,
                   B = B,
                   ridge = penalty_for_KR,
                   inv_w = obs_inv_w, #model is fit with observed weights
                   K = Kernel_Matrix)
  
  #fit FB
  fb_fit = fb_solve(c_mat = C, 
                    price_vec = B, 
                    max_mat = N)
  #smoothing 
  smooth_fb = smooth.spline(x = sample_data$Maturity, 
                            y = fb_fit$y[sample_data$Maturity], 
                            cv = T)
  sm_fb_y = predict(smooth_fb, x = seq(1,N))
  
  shifted_portfolio = shift_portfolio(new_yield_curve = y_new,
                                      portfolio = sample_data,
                                      noise = noise)
  #observed portfolio info
  portfolio_info_obs = get_input_for_weights(C_mat = shifted_portfolio$Cashflow,
                                             B_vec = shifted_portfolio$Price)
  obs_inv_w = get_inv_weights(portfolio_info_obs$Duration, 
                              shifted_portfolio$Price)
  
  #true portfolio info
  true_portfolio_info = get_input_for_weights(C_mat = shifted_portfolio$Cashflow,
                                              B_vec = shifted_portfolio$True_price)
  true_inv_weights = get_inv_weights(true_portfolio_info$Duration, 
                                     shifted_portfolio$True_price)
  
  out_sample_error_KR = calc_in_sample_error(y_true = y_new,
                                             y_fit = KR_Fit$y, 
                                             C = shifted_portfolio$Cashflow,
                                             B = shifted_portfolio$Price,
                                             true_inv_weights = true_inv_weights, 
                                             obs_inv_w = obs_inv_w)
  
  out_sample_error_FB = calc_in_sample_error(y_true = y_new,
                                             y_fit = sm_fb_y$y, 
                                             C = shifted_portfolio$Cashflow,
                                             B = shifted_portfolio$Price,
                                             true_inv_weights = true_inv_weights, 
                                             obs_inv_w = obs_inv_w)
  
  return(list("KR_Results" = out_sample_error_KR,
              "FB_Results" = out_sample_error_FB))
}





