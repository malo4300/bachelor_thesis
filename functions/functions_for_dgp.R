#weight function for the random yield walk
weights_function = function(x, 
                            a = 0.0006, 
                            b = 300, 
                            c = 250, 
                            d = 500,
                            e = 70){ #introduce normal yield curve
  a*(exp(-(0.15*x)^1/b) - c*dlnorm(x = x, mean = log(d),sd = log(sqrt(e))))
}

sample_yield_function = function(weights_function, 
                                 max_maturity = 30*365,
                                 a = 0.0006, 
                                 b = 300, 
                                 c = 250, 
                                 d = 500,
                                 e = 70){
  y = rep(0,max_maturity)
  y_minus_one = 0
  for (i in 1:length(y)) {
    y[i] = y_minus_one + runif(1,0,0.1)*weights_function(i, a = a, b = b, c = c, d = d, e = e)
    y_minus_one = y[i]
  }
  return(y)
}

# distribution of maturity
# requires lubridate package
create_maturity_obj = function(maturities, max_maturity = 30*365, filter_90 = T){
  maturities = na.omit(maturities)
  maturities = lubridate::mdy(maturities[[1]])
  days_left = as.numeric(maturities - lubridate::dmy("03/01/23")) #deadline date
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
  bw = maturity_obj$density$bw # bw is standard deviation
  mean = sample(maturity_obj$days_left, 1)
  sample = round(rnorm(1, mean =mean , sd = bw),0)
  if(filter_90){
    lower_bound = 90
  } else{
    lower_bound = 1 # allows for shifting of the portfolio
  }
  while (sample <= lower_bound || sample > max_maturity) {
    sample = round(rnorm(1, mean = sample(maturity_obj$days_left, 1) , sd = bw),0)
  }
  return(sample)
}

# sample a bond given the yield structure

sample_bond_cashflow = function(maturity_obj, max_maturity = 30*365, filter_90 = T){
  
  maturity = sample_maturity(maturity_obj, max_maturity, filter_90 = filter_90)
  C_vec = rep(0,max_maturity)
  no_payments = ceiling((maturity/365)*2)
  cupon = round(runif(n = 1,
                min = 2,
                max = 4),2) #random cupon payments rounded to two decimal points
  #cupon = 0 # for zero coupon 
  C_vec[maturity] = 100 + cupon
  if(no_payments>1){
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
  ttm = max(payable_dates)/365
  time_factor = (exp(.5*ttm)/(exp(3)+exp(.5*ttm)))
  
  price = price + rnorm(1,0,noise) * time_factor
  #noise depends on maturity 
  return(round(price, 2))
}



sample_bond = function(maturity_obj, 
                       yield_str, 
                       max_maturity = 30*365,
                       noise = 1, 
                       filter_90 = T){
  c_vec  = sample_bond_cashflow(maturity_obj, max_maturity, filter_90 = filter_90)
  price = get_bond_price(c_vec, yield_str = yield_str,noise = noise)
  true_price = get_bond_price(c_vec, yield_str = yield_str,noise = 0)
  return(list(
    Cashflow_Vector = c_vec,
    Price = price,
    True_price = true_price,
    Maturity = which.max(c_vec)
  ))
}

sample_bonds_portfolio = function(maturity_obj, 
                                  yield_str,
                                  number_of_bonds = 50,
                                  max_maturity = 30*365, 
                                  noise = 1, 
                                  filter_90 = T){
  C = matrix(0, nrow = number_of_bonds, ncol = max_maturity)
  B = rep(0,number_of_bonds)
  B_true = rep(0,number_of_bonds)
  Mat = rep(0,number_of_bonds)
  for (i in 1:number_of_bonds) {
    bond = sample_bond(maturity_obj = maturity_obj, 
                       yield_str = yield_str ,
                       max_maturity = max_maturity,  
                       noise = noise,
                       filter_90 = filter_90)
    C[i,] = bond$Cashflow_Vector
    B[i] = bond$Price[1]
    B_true[i] = bond$True_price[1]
    Mat[i] = bond$Maturity[1]
  }
  return(list(
    Cashflow = C, 
    Price = B, 
    True_price = B_true,
    Maturity = Mat
  ))
  
}

#function shifts portfolio one day and calculates new prices based on the new yield curve
shift_portfolio = function(new_yield_curve, portfolio, noise = 1){
  c_mat = portfolio$Cashflow
  max_mat = ncol(c_mat)
  c_mat[,1:(max_mat-1)] = c_mat[,2:max_mat]
  c_mat[,max_mat] = 0
  n_bonds = length(portfolio$Price)
  price = rep(0,n_bonds)
  true_price = rep(0,n_bonds)
  for(i in 1:n_bonds){
    price[i] = get_bond_price(C_vec = c_mat[i,],
                              yield_str =new_yield_curve,
                              noise = noise)
    true_price[i] = get_bond_price(C_vec = c_mat[i,],
                                   yield_str =new_yield_curve,
                                   noise = 0)
  }
  return(list(
    Cashflow = c_mat, 
    Price = price, 
    True_price = true_price,
    Maturity = portfolio$Maturity-1
  ))
}
