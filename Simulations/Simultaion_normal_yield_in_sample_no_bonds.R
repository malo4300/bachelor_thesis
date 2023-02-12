source("functions/fama_bliss.R")
source("functions/functions_for_dgp.R")
source("functions/kernel_matrix.R")
source("functions/portfolio_characteristics.R")
source("functions/functions_for_simulation.R")
source("functions/loss_functions.R")

#Set Parameters
alpha = 0.05
delta = 0
penalty = 1
N = 30*365


#Create Kernel Matrix
K_Matrix = create_kernel_mat(alpha = alpha, 
                             delta = delta, 
                             n_row = N, 
                             n_col = N)

#Create Maturity object to sample from
maturity_csv = readxl::read_excel("data/treasuries_quotes_03_01_23.xlsx", col_names = F, range = "A1:A500")
mat_object = create_maturity_obj(maturities = maturity_csv,
                                 max_maturity = N, 
                                 filter_90 = T) # 90 day filter


#simulate results for different noise
bond_grid = c(50,100,150,200)
noise = 1 #bonds per simulation that are generated
number_of_simulations = 100
results = data.frame(matrix(0, ncol = length(bond_grid), nrow = 4))
colnames(results) = paste("no_bonds=", bond_grid)
rownames(results) = c("FB_true_RMSE", "FB_obs_RMSE", "KR_true_RMSE", "KR_obs_RMSE")

pb = txtProgressBar(min = 1, max = length(bond_grid), style = 2)
for(i in 1:length(bond_grid)){
  setTxtProgressBar(pb,i)
  # simulation for each noise value
  FB_true_RMSE = rep(0,number_of_simulations)
  FB_obs_RMSE = rep(0,number_of_simulations)
  KR_true_RMSE = rep(0,number_of_simulations)
  KR_obs_RMSE = rep(0,number_of_simulations)
  for(j in 1:number_of_simulations){
    #Sample Yield Curve
    y_true = sample_yield_function(weights_function = weights_function,
                                   max_maturity = N)
    #Get results
    test_output = in_sample_results(y_true = y_true,
                                    mat_obj = mat_object,
                                    Kernel_Matrix = K_Matrix,
                                    penalty_for_KR = penalty,
                                    number_of_bonds = bond_grid[i], 
                                    noise = noise, 
                                    max_maturity = N)
    
    FB_true_RMSE[j] = test_output$FB_Results$True_RSME
    FB_obs_RMSE[j] = test_output$FB_Results$Obs_RSME
    KR_true_RMSE[j] = test_output$KR_Results$True_RSME
    KR_obs_RMSE[j] = test_output$KR_Results$Obs_RSME
  }
  results[1,i] = mean(FB_true_RMSE)
  results[2,i] = mean(FB_obs_RMSE)
  results[3,i] = mean(KR_true_RMSE)
  results[4,i] = mean(KR_obs_RMSE)
}


write.table(results, "data/normal_yield_in_sample_no_bonds.csv")
