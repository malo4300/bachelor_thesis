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
#get constant portfolio
set.seed(4300)
y_true = sample_yield_function(weights_function, N)
sample_data = sample_bonds_portfolio(maturity_obj = mat_object,
                                     yield_str = y_true, 
                                     number_of_bonds = number_of_bonds,
                                     max_maturity = N, 
                                     noise = 0, 
                                     filter_90 = T)
C = sample_data$Cashflow
true_price = sample_data$True_price
true_portfolio_info = get_input_for_weights(C_mat = sample_data$Cashflow,
                                            B_vec = sample_data$True_price)
true_inv_weight = get_inv_weights(true_portfolio_info$Duration, 
                               sample_data$True_price)
ttm = apply(sample_data$Cashflow, 1, function(x) max(which(x!=0)))/365
time_factor = (exp(.5*ttm)/(exp(3)+exp(.5*ttm)))
#simulate results for different noise
noise_grid = c(0,0.5,1,1.5,2)
number_of_bonds = 200 #bonds per simulation that are generated
number_of_simulations = 100
var_grid_fb = rep(0,5)
var_grid_kr = rep(0,5)
bias_grid_fb = rep(0,5)
bias_grid_kr = rep(0,5)




pb = txtProgressBar(min = 1, max = length(noise_grid), style = 2)
for (i in 1:length(noise_grid)) {
  setTxtProgressBar(pb,i)
  price_imp_fb = matrix(data = 0, nrow = number_of_bonds, number_of_simulations)
  price_imp_kr = matrix(data = 0, nrow = number_of_bonds, number_of_simulations)
  for (j in 1:number_of_simulations) {
    #inflict noise 
    B = sample_data$True_price + rnorm(number_of_bonds,0,noise_grid[i])*time_factor
    #get yield and duration for weights
    portfolio_info = get_input_for_weights(C, B)
    obs_inv_w = get_inv_weights(portfolio_info$Duration, B)
    #Fit models
    #fit KR
    KR_Fit= KR_solve(C = C,
                     B = B,
                     ridge = penalty,
                     inv_w = obs_inv_w, #model is fit with observed weights
                     K = K_Matrix)
    
    #fit FB
    fb_fit = fb_solve(c_mat = C, 
                      price_vec = B, 
                      max_mat = N)
    
    # get imp prices
    for (k in 1:number_of_bonds) {
      cashflow_days = which(C[k,]!=0)
      price_imp_fb[k,j] = C[k,cashflow_days] %*% fb_fit$g[cashflow_days]
      price_imp_kr[k,j] = C[k,cashflow_days] %*% KR_Fit$g[cashflow_days]
    }
  }
  #var weighted 
  var_grid_fb[i] = (1/true_inv_weight)%*%apply(price_imp_fb, 1, var)
  var_grid_kr[i] =  (1/true_inv_weight)%*%apply(price_imp_kr, 1, var)
  #bias squared and weighted
  bias = rowMeans(price_imp_fb - true_price)
  bias_grid_fb[i] = (1/true_inv_weight)%*%(bias^2)
  bias = rowMeans(price_imp_kr - true_price)
  bias_grid_kr[i] = (1/true_inv_weight)%*%(bias^2)
}



write.table(results,"data/normal_yield_rmse_disassembled.csv")
