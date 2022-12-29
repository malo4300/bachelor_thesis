source("functions/functions_for_dgp.R")
library(readxl)
library(lubridate)
N = 365*5
y_true = sample_yield_function(weights_function = weights_function, max_maturity = N, a = 0.00025)
plot(y_true)

mat_obj = create_maturity_obj(maturities = read_excel("data/treasuries_quotes_23_12_22.xlsx", col_names = F),
                              max_maturity = N)
portfolio = sample_bonds_portfolio(maturity_obj = mat_obj,
                       yield_str = y_true, 
                       number_of_bonds = 50, max_maturity = N)
sum(duplicated(portfolio$Maturity))
#remove dublicated values 
c_mat = portfolio$Cashflow[!duplicated(portfolio$Maturity),]
p_vec = portfolio$Price[!duplicated(portfolio$Maturity)]


#fb fit 
source("functions/fama_bliss.R")
g_est = calc_g_from_f(fb_est)
y_est = -(1/((seq(1,N)/365)))*log(g_est)
plot(y_est, type = "l")
lines(y_true, col ="red")
points(x = (1:N)[ttm], y = rep(-0.005, length(ttm)), pch = "|", col = "lightgrey", cex = 0.8)
