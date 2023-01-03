library(tidyverse)
library(ggtech)

library(ggthemes)
library(bbplot)
source("functions/functions_for_dgp.R")
maturity_csv = readxl::read_excel("data/treasuries_quotes_23_12_22..xlsx", col_names = F, range = "A1:A500")
mat_object = create_maturity_obj(maturities = maturity_csv,
                                 max_maturity = 365*30, 
                                 filter_90 = T) # 90 day filter

extrafont::loadfonts(device="win")
ggplot(data = data.frame(days_left = mat_object$days_left)) + 
  geom_density(aes(x = days_left/365), 
               kernel = "gaussian",
               bw = "SJ-dpi", col = "darkblue", fill = "darkblue", alpha = .1) +
  labs(title = "Density of time to maturities",
       subtitle = "Bandwidth =  245.41",
       x = "Time to maturity (ttm) in years",
       y = "Density") +
  scale_colour_brewer(type = "seq", palette = "Spectral") + 
  geom_point(mapping = aes(x = days_left/365, y = 0), shape = "|", col = "darkblue", alpha =1)


stats::bw.SJ(mat_object$days_left)
