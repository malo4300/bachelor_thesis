source("functions/kernel_matrix.R")
source("functions/portfolio_characteristics.R")
source("functions/fama_bliss.R")
source("functions/loss_functions.R")
#Libraries
library(tidyverse)
library(scales)
#Setting Parameters ----
alpha = 0.05
delta = 0
N = 30*365
#make Kernel Matrix
K = create_kernel_mat(alpha, delta, N,N)

#load data and create weights ----

B = as.vector(read.csv(file = "data/price_2013-12-31.csv",header =F)[,1])
C = as.matrix(read.csv("data/cashflow_2013-12-31.csv", header = F))
number_of_bonds = length(B)

#get yield and duration for weights
portfolio_info = get_input_for_weights(C_mat = C, B_vec = B)
inv_w = get_inv_weights(portfolio_info$Duration, B_vec = B)

#plotting ----
ytm = portfolio_info$Yield_to_maturity
ttm = portfolio_info$Time_to_maturity/365
plot(ytm~I(ttm))
#Fit model ----

penalty = 1
#KR
kr_fit = KR_solve(C = C,
                  B = B,
                  ridge = penalty,
                  inv_w = inv_w, 
                  K = K)

#FB
fb_fit = fb_solve(c_mat = C, 
                  price_vec = B, 
                  max_mat = N)


#plot interpolation ----
max_time_to_mat = max(ttm)
ggplot(data = data.frame(kr_fit, fb_fit), aes(x = (1:max(N))/365)) + 
  geom_line(mapping = aes(y = kr_fit$y, colour = "Kernel-Ridge")) +
  geom_line(mapping = aes(y = fb_est$y, colour = "Fama-Bliss")) + 
  scale_y_continuous(labels = scales::label_comma()) + 
  scale_color_manual(name = "Models",
                     breaks =  c("Kernel-Ridge", "Fama-Bliss"), 
                     values  = c("blue", "green")) + 
  ylab("Estimation of the yield curve") +
  xlab("Time to Maturity of zero coupon bond") +
  xlim(c(0,max_time_to_mat/365))

# calc misspricing
kr_prices = rep(0, length(B))
fb_prices = rep(0, length(B))

for(i in 1:length(B)){
  payment_days = which(C[i,]!=0)
  kr_prices[i] = C[i,payment_days] %*% kr_fit$g[payment_days]
  fb_prices[i] = C[i,payment_days] %*% fb_fit$g[payment_days]
}

sqrt(mean((1/inv_w)*(B-kr_prices)^2)) 
sqrt(mean((1/inv_w)*(B-fb_prices)^2))

# fb has better pricing in sample 

