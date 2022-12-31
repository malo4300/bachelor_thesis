source("functions/kernel_matrix.R")
source("functions/ytm_and_duration.R")
source("functions/portfolio_characteristics.R")
source("functions/fama_bliss.R")
library(tidyverse)
library(scales)
#Setting Parameters ----
alpha = 0.05
delta = 0
N = 30*365
#make Kernel Matrix
K = create_kernel_mat(alpha, delta, N,N)

#load data and create weights ----

B = as.vector(read.csv(file = "data/price_sample1.csv",header =F)[,1])
C = as.matrix(read.csv("data/Cashflow_sample1.csv", header = F))
number_of_bonds = length(B)

#get yield and duration for weights
portfolio_info = get_input_for_weights(C_mat = C, B_vec = B)
inv_w = get_inv_weights(portfolio_info$Duration, B_vec = B)

#plotting ----
ytm = portfolio_info$Yield_to_maturity
ttm = portfolio_info$Time_to_maturity
plot(ytm~I(ttm/365))
#Fit model ----

penalty = 1
#KR
fitted_curves = KR_solv(C = C,
                        B = B,
                        ridge = penalty,
                        inv_w = inv_w, 
                        K = K)

#FB
fb_est = fb_fit(c_mat = C, price_vec = B, max_mat = N)


#plot interpolation ----
max_time_to_mat = max(ttm)
ggplot(data = data.frame(fitted_curves, fb_est), aes(x = (1:N)/365)) + 
  geom_line(mapping = aes(y = fitted_curves$y, colour = "Kernel-Ridge")) +
  geom_line(mapping = aes(y = fb_est$y, colour = "Fama-Bliss")) + 
  scale_y_continuous(labels = scales::label_comma()) + 
  scale_color_manual(name = "Models",
                     breaks =  c("Kernel-Ridge", "Fama-Bliss"), 
                     values  = c("blue", "green")) + 
  ylab("Estimation of the yield curve") +
  xlab("Time to Maturity of zero coupon bond")




