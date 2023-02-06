library(tidyverse)
library(ggtech)
library(ggthemes)
library(gtable)
library(bbplot)
library(gridExtra)
library(scales)
source("functions/functions_for_dgp.R")
path = "C:/Users/malo4/Documents/BA_tex/3_Graphics/"

#results ----
results = read.table("data/normal_yield_out_sample_bonds.csv")
filename = "R_normal_out_sample_bonds.png"
noise_grid = c(0, .5,1, 1.5, 2)
bonds_grid = c(50,100,150,200)

g1 = ggplot(data = data.frame(bonds_grid = bonds_grid),aes(x = bonds_grid))+ 
  geom_point(aes(y = unlist(as.vector(results[1,])), col = "fb"),size = 3, shape = 3) +
  geom_line(aes(y = unlist(as.vector(results[1,])), colour = "fb"), alpha = .2)+
  geom_point(aes(y = unlist(as.vector(results[3,])), col = "kr"),size = 3, shape = 4) +
  geom_line(aes(y = unlist(as.vector(results[3,])), colour = "kr"), alpha = .2)+
  scale_color_manual(name = "",
                     labels = c("Fama-Bliss", "Kernel-Ridge"),
                     breaks = c("fb", "kr"),
                     values = c("darkgreen", "darkred")) +
  guides(colour = guide_legend(override.aes = list(size = 3,
                                                   shape = c(3,4), 
                                                   lty = "blank"))) +
  labs(x = "",
       y = "Average true RMSE") +
  theme_bw() +
  scale_y_continuous(labels = comma)+
  theme(text = element_text(size = 20), 
        legend.text=element_text(size=15),
        axis.title.x=element_blank()) 
g1


g2 = ggplot(data = data.frame(bonds_grid = bonds_grid),aes(x = bonds_grid))+ 
  geom_point(aes(y = unlist(as.vector(results[2,])), col = "fb"),size = 3, shape = 3) +
  geom_line(aes(y = unlist(as.vector(results[2,])), colour = "fb"), alpha = .2)+
  geom_point(aes(y = unlist(as.vector(results[4,])), col = "kr"),size = 3, shape = 4) +
  geom_line(aes(y = unlist(as.vector(results[4,])), colour = "kr"), alpha = .2)+
  scale_color_manual(name = "",
                     labels = c("Fama-Bliss", "Kernel-Ridge"),
                     breaks = c("fb", "kr"),
                     values = c("darkgreen", "darkred")) +
  labs(x = "Number of bonds",
       y = "Average observed RMSE") +
  theme_bw() + 
  scale_y_continuous(labels = comma) +
  theme(text = element_text(size = 20))



legend = gtable_filter(ggplot_gtable(ggplot_build(g1 + theme(legend.position=c(.54, .7),
                                                             legend.direction = "horizontal",
                                                             legend.text=element_text(size=15)))), 
                       "guide-box")
g3 = gridExtra::grid.arrange(g1 + theme(legend.position="none"),
                        g2 + theme(legend.position="none")+
                          theme(text = element_text(size = 20),
                                legend.text=element_text(size=15),),
                        #layout_matrix= cbind(c(1,2),c(3,3)),
                        nrow = 3,
                        ncol  = 1 ,
                        legend,
                        heights=c(0.45, .45, 0.1))
g3

ggsave(filename = filename , 
       plot= g3, 
       path = path, 
       device='png', 
       dpi=1000, 
       width = 8,
       height = 10)
## in case of non allignment
library(cowplot)
g4 = plot_grid(g1 + theme(legend.position="none", text = element_text(size = 20)),
          g2 + theme(legend.position="none", text = element_text(size = 20)),
          legend,
          ncol = 1, 
          align = "v",
          axis = "lb",
          rel_heights =  c(0.45, 0.45, 0.1))

ggsave(filename = filename, 
       plot= g4 + theme_bw() + theme(panel.border = element_blank()) , 
       path = path, 
       device='png', 
       dpi=1000, 
       width = 8,
       height = 10)          
