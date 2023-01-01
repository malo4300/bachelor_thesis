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
noise = 0
N = 30*365

K_Matrix = create_kernel_mat(alpha = alpha, 
                             delta = delta, 
                             n_row = N, 
                             n_col = N)

y_true = sample_yield_function(weights_function = weights_function,
                               max_maturity = N)

maturity_csv = readxl::read_excel("data/treasuries_quotes_23_12_22.xlsx", col_names = F, range = "A1:A500")
mat_object = create_maturity_obj(maturities = maturity_csv,
                                 max_maturity = N, 
                                 filter_90 = T) # 90 day filter

in_sample_results(y_true = y_true,
                  mat_obj = mat_object,
                  Kernel_Matrix = K_Matrix,
                  penalty_for_KR = penalty,
                  number_of_bonds = 250, 
                  noise = noise, 
                  max_maturity = N)
