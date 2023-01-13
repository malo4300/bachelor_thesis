source("functions/functions_for_dgp.R")
new_weight_fun = function(x,
                          a = 0.0006, 
                          b = 300, 
                          c = 250, 
                          d = 500,
                          e = 70){
  if(x %% 7 == 0){
    return(7*weights_function(x))
  }
  return(0)
}


