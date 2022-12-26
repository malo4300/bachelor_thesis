source("functions/functions_for_dgp.R")
library(tidyverse)
library(lubridate)
#set parameters
N = 30*365

#generate true yield structure
y_true = sample_yield_function(weights_function = weights_function,
                               a = 0.002, 
                               b = 175, 
                               c = 2000,
                               d = 20*365) #introduces a humped yield curve
g_true = exp(-y_true*(seq(1,N)/365))

#create portfolio
mat_sheet = readxl::read_excel("data/treasuries_quotes_23_12_22.xlsx", 
                               col_names = F,
                              range = "A1:A500")

mat_obj = create_maturity_obj(mat_sheet,
                              max_maturity = N,
                              filter_90 = T)

portfolio = sample_bonds_portfolio(maturity_obj = mat_obj,
                                   yield_str = y_true, 
                                   number_of_bonds = 250,
                                  max_maturity = N, 
                                  noise = 1)

#get yields
source("functions/weights.R")
data_for_weigths = get_input_for_weights(C_mat = portfolio$Cashflow, 
                                        B_vec = portfolio$Price)


inv_weights = get_inv_weights(duration = data_for_weigths$Duration, 
                              B_vec = portfolio$Price)

# fit model
source("functions/kernel_matrix.R")
#Fit model ----
penalty = 1
alpha = 0.05
delta = 0
K = create_kernel_mat(alpha = alpha,
                      delta = delta,
                      n_row = N, n_col = N)

fitted_curves = KR_solv(C = portfolio$Cashflow,
                        B = portfolio$Price,
                        ridge = penalty,
                        inv_w = inv_weights, 
                        K = K)

time_grid = seq(1,N)/365

ggplot(data = data.frame(time_grid,fitted_curves), 
       aes(x = time_grid)) + 
  geom_line(aes(y = y)) + 
  geom_line(aes(y = y_true, col = "red"))

#testing
source("functions/RMSE.R")

calc_rmse(g_true = g_true, 
          g_est = fitted_curves$g, 
          c_mat = portfolio$Cashflow,
          weights = (inv_weights)^-1)
