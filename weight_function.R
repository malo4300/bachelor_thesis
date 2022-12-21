
y = rep(0,365*30)


weights_function = function(x, a = 0.005, b = 80, c = 1000){
  a*(exp(-(.1*x)^1/b) - c*dlnorm(x = x, mean = log(365*10),sd = log(sqrt(100))))
}
#a decides the spike
#b where the spike is
#c how the tail is shaped

y[1] = 0
for (i in 2:length(y)) {
  y[i] = y[i-1] + runif(1,0,0.1)*weights_function(i,  c = 600)
}
plot(1:length(y)/365, y, type = "l")

