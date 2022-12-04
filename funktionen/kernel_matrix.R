## Here the function for creating the kernal matrix is defined

KM = function(alpha, 
              delta, 
              n_row = 3650, # 365 day * 10 years 
              n_col = 0){
  if(alpha < 0 || delta < 0 || delta > 1){
    retrun(NA)
  }
  #create constants
  if(0<delta && delta <1){
    sqrt_d = sqrt(alpha^2 + 4*delta/(1-delta))
    l1 = (alpha - sqrt_d)/2
    l2 = (alpha + sqrt_d)/2
  } else{
    sqrt_d = l1 = l2 = 0
  }
  
  if(n_col == 0){
    n_col = n_row
  }
  #create empty matrix
  M = matrix(rep(0, n_col*n_row),
             nrow = n_row, 
             ncol = n_col)
  for (i in 1:n_row) {
    x_scaled = i/365
    y_grid = seq(1:n_col)/365
    
    #calculate max and min, vectorized
    max_xy = pmax(x_scaled, y_grid)
    min_xy = pmin(x_scaled, y_grid)
    
    #calculate matrix for different parameters
    # it is assumed that not both alpha and delta equal 0
    if(delta == 0){
      (-min_xy/alpha^2)*exp(-alpha*min_xy)+ 
        (2/alpha^3)*(1-exp(-alpha*min_xy))+
        (-min_xy/alpha^2)*exp(-alpha*max_xy)
    } else if(delta == 1){
      
    }
        
        
        
      
      
    }
  }
  
  



M = matrix(rep(0,100), nrow =10, ncol = 10)
M[1,] = seq(1:10)
