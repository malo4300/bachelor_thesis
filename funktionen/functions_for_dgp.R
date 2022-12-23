#weight function for the random yield walk
weights_function = function(x, a = 0.005, b = 80, c = 1000){
  a*(exp(-(0.1*x)^1/b) - c*dlnorm(x = x, mean = log(365*10),sd = log(sqrt(100))))
}
#a decides the spike/convergence
#b where the spike is
#c how the tail is shaped
#normal yield curve a [0.0005,0.001]

sample_yield_function = function(weights_function, max_maturity = 30*365){
  y = rep(0,max_maturity)
  for (i in 2:length(y)) {
    y[i] = y[i-1] + runif(1,0,0.1)*weights_function(i,a = 0.0005, b = 550 , c = 0)
  }
  return(y)
}



# distribution of maturity
# requires lubridate package
create_maturity_obj = function(maturities, max_maturity = 30*365, filter_90 = T){
  maturities = na.omit(maturities)
  maturities = mdy(maturities$...1)
  days_left = as.numeric(maturities - dmy("22/12/22")) #deadline date
  days_left = days_left[days_left <= max_maturity]
  if(filter_90){
    days_left = days_left[90 < days_left]
    }
  density_maturities = density(days_left, bw = "SJ")
  return(list(density = density_maturities,
              days_left = days_left))
}
# function to sample from the estimated density of maturities
sample_maturity = function(maturity_obj, max_maturity = 30*365, filter_90 = T){
  bw = maturity_obj$density$bw # bw is variance
  mean = sample(maturity_obj$days_left, 1)
  sample = round(rnorm(1, mean =mean , sd = sqrt(bw)),0)
  if(filter_90){
    lower_bound = 90
  } else{
    lower_bound = 0
  }
  while (sample <= lower_bound || sample > max_maturity) {
    sample = round(rnorm(1, mean =mean , sd = sqrt(bw)),0)
  }
  return(sample)
}

# sample a bond given the yield structure

sample_bond_cashflow = function(maturity_obj, max_maturity = 30*365){
  
  maturity = sample_maturity(maturity_obj, max_maturity)
  C_vec = rep(0,max_maturity)
  no_payments = ceiling((maturity/365)*2)
  cupon = round(runif(n = 1,
                min = 2,
                max = 4),2) #random cupon payments rounded to two decimal points
  C_vec[maturity] = 100 + cupon
  if(no_payments > 1){
    for (i in 2:no_payments) {
      if(i%%2 != 0){
        pay_date = maturity - ((i-1)/2)*365
        C_vec[pay_date] =  cupon
      } else{
        pay_date = maturity - floor((i-1)/2)*365 - 182
        C_vec[pay_date] =  cupon
      }
    }
  }
  return(C_vec)
} 

# function that computes the bond price with noise
# noise is depending on the maturiy, otherwise the outliers are to big
get_bond_price = function(C_vec, yield_str, noise = 1){
  #get dates on which payment occurs
  payable_dates = which(C_vec!=0)
  discount_curve = exp(-yield_str*seq(1,length(yield_str))/365)
  price = C_vec[payable_dates] %*% discount_curve[payable_dates]
  # add noice
  price = price +  rnorm(1,0,noise)*(log(max(payable_dates))/(1+log(max(payable_dates))))
  #noise depends on maturity 
  return(price)
}



sample_bond = function(maturity_obj, yield_str, max_maturity = 30*365,noise = 1){
  c_vec  = sample_bond_cashflow(maturity_obj, max_maturity)
  price = get_bond_price(c_vec, yield_str = yield_str,noise = noise)
  return(list(
    Cashflow_Vector = c_vec,
    Price = price,
    maturity = which.max(c_vec)
  ))
}

