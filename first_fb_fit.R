source("functions/functions_for_dgp.R")
source("functions/portfolio_characteristics.R")
source("functions/fama_bliss.R")
source("functions/loss_functions.R")
library(tidyverse)
#
N = 365*30
y_true = sample_yield_function(weights_function = weights_function, 
                               max_maturity = N)

g_true = exp(-y_true*(1:N)/365)


mat_obj = create_maturity_obj(maturities = readxl::read_excel("data/treasuries_quotes_03_01_23.xlsx", col_names = F),
                              max_maturity = N,
                              filter_90 = T)#

portfolio = sample_bonds_portfolio(maturity_obj = mat_obj,
                                   yield_str = y_true,
                                   number_of_bonds = 200, 
                                   max_maturity = N,
                                   noise = 1,
                                   filter_90 = T)

#dup = duplicated(portfolio$Maturity)
#sum(duplicated(portfolio$Maturity))
#get characteristics of bonds with and without true prices
portfolio_info_true = get_input_for_weights(portfolio$Cashflow, portfolio$True_price)
portfolio_info_est = get_input_for_weights(portfolio$Cashflow, portfolio$Price)
plot(x = portfolio_info_est$Time_to_maturity, y = portfolio_info_est$Yield_to_maturity)
#remove duplicated values 


#fb fit 
fb_est = fb_solve(c_mat = portfolio$Cashflow, 
                  price_vec = portfolio$Price, 
                  max_mat = N)


ttm = portfolio_info_true$Time_to_maturity
ggplot(data = data.frame(fb_est, y_true), aes(x = seq(1,N)/365))+
  geom_line(aes(y = y), alpha = 1, size = .1) + 
  geom_line(aes(y = y_true),col = "red", linetype = "dotted", size = 1) + 
  geom_point(data = data.frame(x =seq(1:N)[ttm]/365, y = -0.005 ), 
             aes(x = x, y = y), shape ="|", col = "blue", alpha = .5) + 
  geom_smooth(aes(y = y), size = .1, col = "green")


inv_weights = get_inv_weights(portfolio_info_true$Duration, B_vec = portfolio$True_price)
inv_weights_obs = get_inv_weights(portfolio_info_est$Duration, B_vec = portfolio$Price)
calc_in_sample_error(y_true = y_true,
                     y_fit = fb_est$y, 
                     C = portfolio$Cashflow, 
                     B = portfolio$Price, 
                     true_inv_weights = inv_weights,
                     obs_inv_w = inv_weights_obs)

pred_price = rep(0,300)
for (i in 1:300) {
  days_with_payment = which(portfolio$Cashflow[i,]!=0)
  pred_price[i] = portfolio$Cashflow[i, days_with_payment] %*% fb_est$g[days_with_payment]
}
pred_ytm = get_input_for_weights(portfolio$Cashflow, pred_price)$Yield_to_maturity

ggplot(data = data.frame(portfolio_info_true, pred_ytm), 
       aes(x = Time_to_maturity)) + 
         geom_point(aes(y = pred_ytm), size = .1,  shape = 21, col = "darkgreen") +
  geom_line(aes(y = pred_ytm), alpha = .1)+
  geom_point(aes(y = Yield_to_maturity), size = .1,  shape = 21, col = "darkblue") +
  






#fit to real data -----

c_mat = as.matrix(read.csv("data/cashflow_2013-12-31.csv", header = F))
price_vec = as.vector(read.csv("data/price_2013-12-31.csv", header = F)[,1])
ttm = apply(c_mat, 1, which.max)
#duplicates = duplicated(ttm)
#sum(duplicates)
#c_mat_without_dup = c_mat[!duplicates,]
#price_vec_without_dup = price_vec[!duplicates]


#fit
fb_est = fb_solve(c_mat = c_mat, 
                  price_vec = price_vec, 
                  max_mat = max(ttm))
ggplot(data = data.frame(fb_est, time = seq(1,max(ttm))/365), aes(x = time))+
  geom_line(aes(y = y))

#calc ytm -----


true_ytm_dur = get_input_for_weights(C_mat = portfolio$Cashflow, 
                                    B_vec = portfolio$Price)

inv_weights = get_inv_weights(true_ytm_dur$Duration, portfolio$True_price)
# in sample error


ggplot(data = data.frame(true_ytm_dur), aes(x = Time_to_maturity, y = Yield_to_maturity)) + 
  geom_point()


#test shifting
y_new = sample_yield_function(weights_function = weights_function, 
                               max_maturity = N)

shifted_portfolio = shift_portfolio(new_yield_curve = y_new,
                                    portfolio = portfolio,
                                    noise = 1)
# out sample error
portfolio_info_true = get_input_for_weights(shifted_portfolio$Cashflow, shifted_portfolio$True_price)
portfolio_info_true_obs = get_input_for_weights(shifted_portfolio$Cashflow, shifted_portfolio$Price)
true_inv_weights = get_inv_weights(duration = portfolio_info_true$Duration, shifted_portfolio$True_price)
obs_inv_weights =  get_inv_weights(duration = portfolio_info_true_obs$Duration, shifted_portfolio$Price)

calc_in_sample_error(y_true = y_new, 
                    C = shifted_portfolio$Cashflow,
                    B = shifted_portfolio$Price,
                    y_fit = fb_est$y, 
                    true_inv_weights = true_inv_weights,
                    obs_inv_w = obs_inv_weights)



# smooth ----

plot(y_true, type = "l", col = "blue")
lines(fb_est$y, type = "l", col = "red")
k_s = smooth.spline(x = seq(1,N), y = fb_est$y, df = 30)
lines(k_s$y, type = "l", col = "green")



