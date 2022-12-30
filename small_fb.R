source("functions/functions_for_dgp.R")
source("functions/weights.R")
library(tidyverse)
library(readxl)
library(lubridate)
library("nleqslv")
N = 365*10
y_true = sample_yield_function(weights_function = weights_function, max_maturity = N, a = 0.001, b = 150,c = 1000)
plot(y_true, type = "l")

mat_obj = create_maturity_obj(maturities = read_excel("data/treasuries_quotes_23_12_22.xlsx", col_names = F),
                              max_maturity = N,
                              filter_90 = T)
portfolio = sample_bonds_portfolio(maturity_obj = mat_obj,
                       yield_str = y_true, 
                       number_of_bonds = 400, max_maturity = N,
                       noise = 0)
dup = duplicated(portfolio$Maturity)
sum(duplicated(portfolio$Maturity))
#ytm 
ytm = get_input_for_weights(portfolio$Cashflow, portfolio$Price)
plot(x = ytm$Time_to_maturity, y = ytm$Yield_to_maturity)
#remove dublicated values 



#fb fit 
source("functions/fama_bliss.R")
fb_est = fb_fit(c_mat = portfolio$Cashflow[!dup,], price_vec = portfolio$Price[!dup], max_mat = N)
ttm = apply(portfolio$Cashflow, 1, which.max)
ggplot(data = data.frame(fb_est, y_true), aes(x = seq(1,N)/365))+
  geom_line(aes(y = y)) + 
  geom_line(aes(y = y_true),col = "red", linetype = "dotted", size = 1) + 
  geom_point(data = data.frame(x =seq(1:N)[ttm]/365, y = -0.005 ), 
             aes(x = x, y = y), shape ="|", col = "blue", alpha = .5)


# try fit to real data

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

#calc ytm

implied_prices = rep(0, nrow(c_mat_without_dup))

for (i in 1:length(implied_prices)) {
  time_to_cashflow = which(c_mat_without_dup[i,] != 0)
  implied_prices[i] = c_mat_without_dup[i,time_to_cashflow] %*% fb_est$g[time_to_cashflow]
}

est_ytm_dur = get_input_for_weights(C_mat = c_mat_without_dup, 
                                    B_vec = implied_prices)
sqrt(mean((implied_prices-price_vec_without_dup)^2))
est_ytm_dur

ggplot(data = data.frame(est_ytm_dur), aes(x = Time_to_maturity, y = Yield_to_maturity)) + 
  geom_point()


#fama bliss filter
sample_ytm = get_input_for_weights(C_mat = c_mat, 
                                   B_vec = price_vec)

sample_ytm$Time_to_maturity
order_ttm = order(sample_ytm$Time_to_maturity)
plot(x = sample_ytm$Time_to_maturity, 
     y = sample_ytm$Yield_to_maturity)


sample_ytm$Yield_to_maturity[order_ttm]

