y = rep(0,365*30)


weights_function = function(x, a = 0.005, b = 80, c = 1000){
  a*(exp(-(.1*x)^1/b) - c*dlnorm(x = x, mean = log(365*10),sd = log(sqrt(100))))
}
#a decides the spike/convergence
#b where the spike is
#c how the tail is shaped
#normal yield curve a [0.0005,0.001]

y[1] = 0
for (i in 2:length(y)) {
  y[i] = y[i-1] + runif(1,0,0.1)*weights_function(i,a = 0.0005, b = 200 , c = 0)
}
plot(1:length(y)/365, y, type = "l")



# sample a bond given the yield structure

sample_bond = function(max_maturity = 30*365){
  maturity = round(12.7*365,0) #need to implement random maturity
  C_vec = rep(0,maturity)
  no_payments = ceiling((maturity/365)*2)
  cupon = runif(n = 1,
                min = 2,
                max = 4) #need to implement random payment structure
  C_vec[maturity] = 100 + cupon
  print(maturity)
  for (i in 2:no_payments) {
    if(i%%2 != 0){
      pay_date = maturity - ((i-1)/2)*365
      
      C_vec[pay_date] =  cupon
    } else{
     
      pay_date = maturity - floor((i-1)/2)*365 - 182
      
      C_vec[pay_date] =  cupon
    }
  }
  return(C_vec)
} 

#c_samle = sample_bond()
#plot(which(c_samle!=0), c_samle[which(c_samle!=0)])

get_bond_price = function(C_vec, yield_str, noise = 1){
  #get dates on which payment occurs
  payable_dates = which(C_vec!=0)
  discount_curve = exp(-yield_str*seq(1,length(yield_str)/365))
  price = C_vec[payable_dates] %*% discount_curve[payable_dates] + rnorm(1,0,noise)
  return(price)
}


get_bond_price(c_samle, y)


# distribution of maturity
library(lubridate)
maturities = readxl::read_excel("data/treasury_quotes_21_12_22.xlsx", col_names = F, range = "A1:A500")
maturity_obj = function(maturities, max_maturity = 30*365){
  maturities = na.omit(maturities)
  maturities = mdy(maturities$...1)
  days_left = as.numeric(maturities - today())
  density_maturities = density(days_left, bw = "SJ")
  return(list(density = density_maturities,
              days_left = days_left))
}
# function to sample from the estimated density of maturities
sample_maturity = function(maturity_obj){
  bw = maturity_obj$density$bw # bw is variance
  mean = sample(maturity_obj$days_left, 1)
  estimation = round(rnorm(1, mean =mean , sd = sqrt(bw)),0)
  while (estimation <0) {
    estimation = round(rnorm(1, mean =mean , sd = sqrt(bw)),0)
  }
  return(estimation)
}

mat_obj = maturity_obj(maturities = maturities)

test = rep(0,300)
for(i in 1:300){
  test[i] = sample_maturity(mat_obj)
}
max(test)
plot(density(test))
sum(test < 0)
