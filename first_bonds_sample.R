library(lubridate)
source("functions/functions_for_dgp.R")
maturities = readxl::read_excel("data/treasury_quotes_21_12_22.xlsx", col_names = F, range = "A1:A500")
mat_obj = create_maturity_obj(maturities = maturities)
y_true = sample_yield_function(weights_function = weights_function)


max_mat = 30*365
m = 300 # number of bonds
b = rep(0,m)
c = matrix(0, nrow = m, ncol = max_mat)
mat = rep(0,m)
for (i in 1:m) {
  bond = sample_bond(mat_obj, y_true)
  b[i] = bond$Price[1]
  c[i,] = bond$Cashflow_Vector
  mat[i] = bond$maturity[1]
}
plot(y_true)
write.table(c, file = "data/Cashflow_sample1.csv", sep=",",  
            col.names=FALSE, 
            row.names = FALSE)
write.table(b, file = "data/price_sample1.csv", sep=",",  
            col.names=FALSE, 
            row.names = FALSE)

