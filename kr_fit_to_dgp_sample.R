source("functions/kernel_matrix.R")
source("functions/ytm_and_duration.R")
source("functions/functions_for_dgp.R")
#libraries
library(lubridate)
library(tidyverse)
#Setting Parameters ----
alpha = 0.05
delta = 0
N = 30*365
#make Kernel Matrix
K = KM(alpha, delta, N,N)
#generate yield curve
y_true = sample_yield_function(weights_function = weights_function,
                               max_maturity = N)
g_true = exp(-y_true*(1:N)/365)
#create Maturity object
maturity_csv = readxl::read_excel("data/treasuries_quotes_23_12_22.xlsx", col_names = F, range = "A1:A500")
mat_object = create_maturity_obj(maturities = maturity_csv,
                                 max_maturity = N, 
                                 filter_90 = T) # 90 day filter
#generate Data
sample_data = sample_bonds_portfolio(maturity_obj = mat_object,
                                     yield_str = y_true, 
                                     number_of_bonds = 300,
                                     max_maturity = N, 
                                     noise = 2)
C = sample_data$Cashflow
B = sample_data$Price
number_of_bonds = length(B)

#get yield and duration for weights
ytm = rep(0,number_of_bonds)
dur = rep(0,number_of_bonds)
ttm = rep(0,number_of_bonds) # time to maturity

for (i in 1:number_of_bonds) {
  
  time_to_cashflow = which(C[i,] != 0)
  ytm_and_dur = get_ytm_and_duration(cashflow = C[i, time_to_cashflow],
                                     time_to_cashflow = time_to_cashflow,
                                     B_i =  B[i])
  ytm[i] = ytm_and_dur$ytm_solved
  dur[i] = ytm_and_dur$dur_solved
  ttm[i] = max(time_to_cashflow)
}

inv_w = (dur*B)^2*number_of_bonds


#Fit model ----

penalty = 1
fitted_curves = KR_solv(C = C,
                        B = B,
                        ridge = penalty,
                        inv_w = inv_w, 
                        K = K)

#plot interpolation
max_time_to_mat = max(ttm)
ggplot(fitted_curves[1:max_time_to_mat,],
       aes( x = (1:max_time_to_mat)/365)) + 
  geom_line(mapping = aes(y = g)) +
  scale_y_continuous(labels = scales::label_comma())+
  geom_line(aes(y = g_true[1:max_time_to_mat], col = "red"))


plot(ytm~I(ttm/365))

calc_rmse(g_true = g_true, g_est = fitted_curves$g, 
          weights = (1/inv_w), c_mat = C)
