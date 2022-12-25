source("functions/kernel_matrix.R")
source("functions/ytm_and_duration.R")
#Setting Parameters ----
alpha = 0.05
delta = 0
N = 30*365
#make Kernel Matrix
K = KM(alpha, delta, N,N)

#load data and create weights ----
date = "1961-06-30"

B = as.vector(read.csv(file = "data/price_sample1.csv",header =F)[,1])
C = as.matrix(read.csv("data/Cashflow_sample1.csv", header = F))
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

#plotting ----
library(tidyverse)
library(scales)
plot(ytm~I(ttm/365))
#Fit model ----

penalty = 1

fitted_curves = KR_solv(C = C,
                        B = B,
                        ridge = penalty,
                        inv_w = inv_w, 
                        K = K)
fitted_curves$g[2]
#plot(fitted_curves$y)
#plot interpolation
max_time_to_mat = max(ttm)
ggplot(fitted_curves[1:max_time_to_mat,]) + 
  geom_point(mapping = aes(x = (1:max_time_to_mat)/365,
                          y = y)) +
  scale_y_continuous(labels = scales::label_comma())

