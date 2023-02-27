source("functions/functions_for_dgp.R")
library(tidyverse)
library(reshape2)
y_frame = matrix(0, nrow = 25, ncol = 30*365)
for(i in 1:25){
  y_frame[i,] = sample_yield_function(weights_function, max_maturity = 30*365)
}
y_m <- melt(y_frame)
mean_yield = colMeans(y_frame)
g = ggplot() + geom_line(data = y_m, aes(x = Var2/365, y = value, group = Var1),
                              col = "darkblue",
                              size = .3,
                              lty = "dotted", 
                              alpha = .4) +
  geom_line(aes(x = seq(1,365*30)/365, y = mean_yield), col= "darkblue")+ 
  labs(x = "Time-to-maturity (ttm) in years",y = "True yield curve" ) +
  theme_bw()+
  scale_x_continuous(expand = c(0.02,0,0.02,0))+
  scale_y_continuous(expand = c(0.02,0,0.1,0)) + 
  #ylim(0,0.06)+
  theme(text = element_text(size = 18)) 

path = "C:/Users/malo4/Documents/BA_tex/3_Graphics/"
ggsave(filename = "multiple_yield_curves.png",
       plot = g,
       path = path, device='png', dpi=1200, width = 12, height = 7)



