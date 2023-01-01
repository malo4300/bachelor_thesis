source("functions/functions_for_dgp.R")
source("functions/portfolio_characteristics.R")
source("functions/fama_bliss.R")
library(tidyverse)
library(lubridate)
library(nleqslv)

N = 365*30
y_true = sample_yield_function(weights_function = weights_function, 
                               max_maturity = N, 
                               a = 0.0006, b = 300,c = 250, d = 500, e = 70)

g_true = exp(-y_true*(1:N)/365)


mat_obj = create_maturity_obj(maturities = readxl::read_excel("data/treasuries_quotes_23_12_22.xlsx", col_names = F),
                              max_maturity = N,
                              filter_90 = T)
portfolio = sample_bonds_portfolio(maturity_obj = mat_obj,
                                   yield_str = y_true,
                                   number_of_bonds = 300, 
                                   max_maturity = N,
                                   noise = 1,
                                   filter_90 = T)

#dup = duplicated(portfolio$Maturity)
#sum(duplicated(portfolio$Maturity))
#get characteristics of bonds 
portfolio_info = get_input_for_weights(portfolio$Cashflow, portfolio$Price)
plot(x = portfolio_info$Time_to_maturity, y = portfolio_info$Yield_to_maturity)
#remove dublicated values 


#fb fit 
fb_est = fb_fit(c_mat = portfolio$Cashflow, price_vec = portfolio$Price, max_mat = N)
ttm = portfolio_info$Time_to_maturity
ggplot(data = data.frame(fb_est, y_true), aes(x = seq(1,N)/365))+
  geom_point(aes(y = y), alpha = .1, size = .1) + 
  geom_line(aes(y = y_true),col = "red", linetype = "dotted", size = 1) + 
  geom_point(data = data.frame(x =seq(1:N)[ttm]/365, y = -0.005 ), 
             aes(x = x, y = y), shape ="|", col = "blue", alpha = .5) + 
  geom_smooth(aes(y = y), size = .1, col = "green")


inv_weights = get_inv_weights(portfolio_info$Duration, B_vec = portfolio$Price)
calc_rmse(g_true = g_true, 
          g_est = fb_est$g, 
          c_mat = portfolio$Cashflow, 
          weights = 1/inv_weights)
#fit to real data ----

c_mat = as.matrix(read.csv("data/cashflow_2013-12-31.csv", header = F))
price_vec = as.vector(read.csv("data/price_2013-12-31.csv", header = F)[,1])
ttm = apply(c_mat, 1, which.max)
duplicates = duplicated(ttm)
sum(duplicates)
c_mat_without_dup = c_mat[!duplicates,]
price_vec_without_dup = price_vec[!duplicates]


#fit
fb_est = fb_fit(c_mat = c_mat_without_dup, price_vec = price_vec_without_dup, max_mat = max(ttm))
ggplot(data = data.frame(fb_est, time = seq(1,max(ttm))/365), aes(x = time))+
  geom_line(aes(y = y))

#calc ytm -----


true_ytm_dur = get_input_for_weights(C_mat = portfolio$Cashflow, 
                                    B_vec = portfolio$True_price)

inv_weights = get_inv_weights(true_ytm_dur$Duration, portfolio$True_price)
# in sample error
calc_rmse(g_true, g_est = fb_est$g, weights =1/inv_weights, c_mat = portfolio$Cashflow)

ggplot(data = data.frame(est_ytm_dur), aes(x = Time_to_maturity, y = Yield_to_maturity)) + 
  geom_point() +
  geom_smooth()


#test shifting
y_new = sample_yield_function(weights_function = weights_function, 
                               max_maturity = N)

shifted_portfolio = shift_portfolio(new_yield_curve = y_new,
                                    portfolio = portfolio,
                                    noise = 1)
# out sample error
portfolio_info = get_input_for_weights(shifted_portfolio$Cashflow, shifted_portfolio$True_price)
inv_weights = get_inv_weights(duration = portfolio_info$Duration, shifted_portfolio$True_price)
calc_rmse(y_true = y_new, y_est = fb_est$y, c_mat = shifted_portfolio$Cashflow, weights = 1/inv_weights)
