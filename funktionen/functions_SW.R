
#Calculate Function to compute the Wilson Matrix for the Smith-Wilson method
SW_Kernel = function(alpha, 
                     n_row = 3650, # 365 day * 10 years 
                     n_col = 0,
                     UFR = 0.03){
  if(n_col == 0){
    n_col = n_row
  }
  W = matrix(0,
             rep(0, n_col*n_row),
             nrow = n_row,
             ncol = n_col)
  
  for(i in 1:n_row){
    x_scaled = i/365
    y_grid = seq(1:n_col)/365
    
    #calculate max and min, vectorized
    max_xy = pmax(x_scaled, y_grid)
    min_xy = pmin(x_scaled, y_grid)
  
    #Def 1
    W[i,] = exp(-UFR*(x_scaled+y_grid))*(alpha*min_xy-0.5*exp(-alpha*max_xy)*(exp(alpha*min_xy)-exp(-alpha*min_xy)))
    
    #Def 2
    #W[i,] = exp(-UFR*(x_scaled+y_grid))*(alpha*min_xy-exp(-alpha*max_xy)*sinh(alpha*min_xy))
    }
  return(W)
}


# function to produce discount curve

SW_Solver = function(C, # Cashflowmatrix
                     B, # prices of the bonds
                     W, # Wilson Matrix
                     UFR = 0.03
){
  #break down cashflow matrix to only those were there is not zero cashflow
  non_zero_casflow_idx = (1:ncol(C))[which(colSums(C) != 0)] #Col names not always provided as integers
 
  #create matrix with only those days on which cash flow occured
  W_on_c_days = W[non_zero_casflow_idx,non_zero_casflow_idx]
  C_on_c_days = C[,non_zero_casflow_idx]
  number_days = length(non_zero_casflow_idx)
 
  
  CWC = C_on_c_days%*%W_on_c_days%*%t(C_on_c_days)
  CWC_inv = solve(CWC)
  mu = exp(-UFR*non_zero_casflow_idx/365)
  zeta = CWC_inv %*% (B-C_on_c_days%*%mu)
  
  P = exp(-UFR*non_zero_casflow_idx/365) + W[,non_zero_casflow_idx]%*%t(C_on_c_days)%*%zeta
  return(P)
}



#test
B = as.vector(read.csv("data/price_1961-06-30.csv",header =F)[,1])
C = as.matrix(read.csv("data/cashflow_1961-06-30.csv", header = F))
number_of_bonds = length(B)
N = 30*365
#make Kernel Matrix
W = SW_Kernel(alpha = 0.1, n_row = N , UFR= 0.02)

g_solved = SW_Solver(C = C,
                     B = B, 
                     W = W,
                     UFR = 0.02)













#test from paper

W = SW_Kernel(alpha = 0.1, n_row = 365*5 , UFR= 0.02)

non_zero_casflow_idx = seq(1,5)*365
W_on_c_days = W[non_zero_casflow_idx,non_zero_casflow_idx]
C = matrix(rep(0,20), ncol = 5, nrow = 4)

C[4,1:4] = 0.034
C[4,5] = 1.034
C[1,1] = 1.01
C[2,1] =0.02
C[2,2] = 1.02
C[3,1] = 0.026
C[3,2] = 0.026
C[3,3] = 1.026

solved = SW_Solver(C = C,
                   W = W_on_c_days, 
                  B = rep(1,4),
                  UFR = 0.02)
plot(non_zero_casflow_idx)
