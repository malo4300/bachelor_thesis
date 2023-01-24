library(tidyverse)
library(ggtech)
library(ggthemes)
library(gtable)
library(bbplot)
library(gridExtra)
library(scales)
source("functions/functions_for_dgp.R")
path = "C:/Users/malo4/Documents/BA_tex/3_Graphics/"



results = read.table("data/normal_yield_rmse_disassembled.csv")
noise_grid = c(0, .5,1, 1.5, 2)

g1 = ggplot(data = data.frame(noise = noise_grid),aes(x = noise))+ 
  geom_point(aes(y = unlist(as.vector(results[,1])), col = "fb"),size = 3, shape = 3) +
  geom_line(aes(y = unlist(as.vector(results[,1])), colour = "fb"), alpha = .2)+
  geom_point(aes(y = unlist(as.vector(results[,2])), col = "kr"),size = 3, shape = 4) +
  geom_line(aes(y = unlist(as.vector(results[,2])), colour = "kr"), alpha = .2)+
  scale_color_manual(name = "",
                     labels = c("Fama-Bliss", "Kernel-Ridge"),
                     breaks = c("fb", "kr"),
                     values = c("darkgreen", "darkred")) +
  guides(colour = guide_legend(override.aes = list(size = 3,
                                                   shape = c(3,4), 
                                                   lty = "blank"))) +
  labs(x = "Level of noise",
       y = "Average weighted squared Bias") +
  theme_bw() +
  #scale_y_continuous(labels = comma)+
  theme(text = element_text(size = 20), 
        legend.text=element_text(size=15),
        legend.position = "bottom") 
g1
ggsave(filename = "R_normal_bias_squared.png", 
       plot= g1, 
       path = path, 
       device='png', 
       dpi=1000, 
       width = 8,
       height = 10)



g2 = ggplot(data = data.frame(noise = noise_grid),aes(x = noise))+ 
  geom_point(aes(y = unlist(as.vector(results[,3])), col = "fb"),size = 3, shape = 3) +
  geom_line(aes(y = unlist(as.vector(results[,3])), colour = "fb"), alpha = .2)+
  geom_point(aes(y = unlist(as.vector(results[,4])), col = "kr"),size = 3, shape = 4) +
  geom_line(aes(y = unlist(as.vector(results[,4])), colour = "kr"), alpha = .2)+
  scale_color_manual(name = "",
                     labels = c("Fama-Bliss", "Kernel-Ridge"),
                     breaks = c("fb", "kr"),
                     values = c("darkgreen", "darkred")) +
  guides(colour = guide_legend(override.aes = list(size = 3,
                                                   shape = c(3,4), 
                                                   lty = "blank"))) +
  labs(x = "Level of noise",
       y = "Average weighted variance") +
  theme_bw() +
  #scale_y_continuous(labels = comma)+
  theme(text = element_text(size = 20), 
        legend.text=element_text(size=15),
        legend.position = "bottom") 
g2

ggsave(filename = "R_normal_variance.png", 
       plot= g2, 
       path = path, 
       device='png', 
       dpi=1000, 
       width = 8,
       height = 10)
