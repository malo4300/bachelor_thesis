source("functions/kernel_matrix.R")
source("functions/portfolio_characteristics.R")
source("functions/functions_for_dgp.R")
source("functions/loss_functions.R")
#libraries
library(lubridate)
library(tidyverse)
#Setting Parameters ----
alpha = 0.05
delta = 0
penalty = 1
N = 30*365
#make Kernel Matrix
K = create_kernel_mat(alpha, delta, N,N)
#generate yield curve
y_true = sample_yield_function(weights_function = weights_function,
                               max_maturity = N)
plot(y_true)
g_true = exp(-y_true*(1:N)/365)
#create Maturity object
maturity_csv = readxl::read_excel("data/treasuries_quotes_03_01_23.xlsx", col_names = F, range = "A1:A500")
mat_object = create_maturity_obj(maturities = maturity_csv,
                                 max_maturity = N, 
                                 filter_90 = T) # 90 day filter
#generate Data
sample_data = sample_bonds_portfolio(maturity_obj = mat_object,
                                     yield_str = y_true, 
                                     number_of_bonds = 300,
                                     max_maturity = N, 
                                     noise = 1)
C = sample_data$Cashflow
B = sample_data$Price
number_of_bonds = length(B)

#get yield and duration for weights
portfolio_info = get_input_for_weights(C, B)

inv_w = get_inv_weights(portfolio_info$Duration, B)


#Fit model ----


KR_Fit = KR_solve(C = C,
                  B = B,
                  ridge = penalty,
                  inv_w = inv_w, 
                  K = K)

#plot interpolation
max_time_to_mat = max(portfolio_info$Time_to_maturity)
ggplot(KR_Fit[1:max_time_to_mat,],
       aes(x = (1:max_time_to_mat)/365)) + 
  geom_line(mapping = aes(y = g)) +
  scale_y_continuous(labels = scales::label_comma())+
  geom_line(aes(y = g_true[1:max_time_to_mat], col = "red"))


plot(ytm~I(portfolio_info$Time_to_maturity/365))

calc_obs_rmse(prices_obs = B,
              y_est = KR_Fit$y,
              c_mat = C, 
              weights = 1/inv_w)

