library(tidyverse)
library(ggtech)
library(ggthemes)
library(bbplot)
library(gtable)
library(gridExtra)
library(scales)
source("functions/functions_for_dgp.R")
source("functions/fama_bliss.R")
source("functions/kernel_matrix.R")
source("functions/portfolio_characteristics.R")
path = "C:/Users/malo4/Documents/BA/3_Graphics/"


# Maturity Distribution -----
maturity_csv = readxl::read_excel("data/treasuries_quotes_03_01_23.xlsx", col_names = F, range = "A1:A500")
mat_object = create_maturity_obj(maturities = maturity_csv,
                                 max_maturity = 365*30, 
                                 filter_90 = T) # 90 day filter

extrafont::loadfonts(device="win")
g = ggplot(data = data.frame(days_left = mat_object$days_left)) + 
  geom_density(aes(x = days_left/365), 
               kernel = "gaussian",
               bw = "SJ-dpi", col = "darkblue", fill = "darkblue", alpha = .2) +
  labs(x = "Time-to-maturity (ttm) in years",
       y = paste("Density estimation (bw = ", 
                 round(mat_object$density$bw/365,2), ")", sep  = "")) +
  geom_point(mapping = aes(x = days_left/365, y = 0), shape = "|", col = "darkblue", alpha =1) +
  theme_bw() +
  scale_x_continuous(limits = c(0,30), expand = c(0,0,.01,0))  +
  theme(text = element_text(size = 18)) 



g
name = "density.png"
ggsave(filename = name,
      plot = g,
       path = path, device='png', dpi=900)


# Normal Curve ----
set.seed(1)
y_true = sample_yield_function(weights_function, max_maturity = 30*365)

g2 = ggplot(data = data.frame(y = y_true, ttm = seq(1,30*365)/365), aes(x = ttm, y = y)) +
  geom_line(col = "darkblue") +
  labs(x = "Time-to-maturity (ttm) in years",y = "True yield curve" ) +
  theme_bw()+
  scale_x_continuous(expand = c(0.02,0,0.02,0)) +
  ylim(0,0.06)+
  theme(text = element_text(size = 18)) 
g2
name = "true_yield_normal.png"
ggsave(filename = name,
       plot= g2,
       path = path, device='png', dpi=700)

# Humped curve -----
set.seed(1)
y_true_humped = sample_yield_function(weights_function = weights_function,
                                      a = 0.001,
                                      b = 200, 
                                      c = 600, 
                                      d = 5000, 
                                      e = 2,
                                      max_maturity = 30*365)
g3 = ggplot(data = data.frame(y = y_true_humped, ttm = seq(1,30*365)/365),
            aes(x = ttm, y = y)) + 
  geom_line(col = "darkblue") + 
  labs(x = "Time-to-maturity (ttm) in years", 
       y = "True yield curve" ) +
  theme_bw()  +
  scale_x_continuous(expand = c(0.02,0,0.02,0)) +
  ylim(0,0.06)+
  theme(text = element_text(size = 18)) 

g3
name = "true_yield_humped.png"
ggsave(filename = name,
       plot= g3,
       path = path, device='png', dpi=700) 


# ytm for different noise of zero coupon bonds -----
max_maturity = 30*365
noise_grid = c(0,1,2)
number_of_bonds = 200
ytm_results = data.frame(obs_ytm = rep(0,number_of_bonds*length(noise_grid)),
                         ttm = rep(0,number_of_bonds*length(noise_grid)),
                         noise = 0)
set.seed(1) 
y_true = sample_yield_function(weights_function = weights_function,
                               max_maturity = 30*365)
# change coupon manually in file
for (i in 1:length(noise_grid)) {
  set.seed(1) #sample the same bonds with different noise
  portfolio = sample_bonds_portfolio(maturity_obj = mat_object,
                                  yield_str = y_true,
                                  number_of_bonds = number_of_bonds,
                                  max_maturity = max_maturity, 
                                  noise = noise_grid[i],
                                  filter_90 = T)
  ytm = get_input_for_weights(portfolio$Cashflow, portfolio$Price)
  ytm_results$obs_ytm[((i-1)*number_of_bonds + 1):(i*number_of_bonds)] = ytm$Yield_to_maturity
  ytm_results$ttm[((i-1)*number_of_bonds + 1):(i*number_of_bonds)] = ytm$Time_to_maturity
  ytm_results$noise[((i-1)*number_of_bonds + 1):(i*number_of_bonds)] = noise_grid[i]
}

g4 = ggplot(data = ytm_results, aes(x = ttm/365)) + 
  geom_point(aes(x = ttm/365, y  = obs_ytm), shape = 1, col = "darkblue") + 
  facet_wrap(.~noise, nrow= 3, labeller = "label_both") +
  labs(x = "Time-to-maturity (ttm) in years", 
       y = "Yield-to-maturity (YTM)") +
scale_x_continuous(expand = c(0.02,0,0.02,0))+
scale_y_continuous(expand = c(0.02,0,0.02,0)) + 
  #geom_line(aes(x = ttm/365, y = y_true[ttm])) + 
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.text.x = element_text(size = 20))
g4
name = "noise_grid_zero_coupon_ytm.png"
ggsave(filename = name, plot= g4, path = path, device='png', dpi=900, width = 8, height = 8)

# ytm fit plot -----

alpha = 0.05
delta = 0
max_maturity = 30*365
number_of_bonds = 200
N =30*365
K = create_kernel_mat(alpha = alpha,
                      delta = delta, 
                      n_row = max_maturity, n_col = max_maturity)

set.seed(1)
y_true = sample_yield_function(weights_function = weights_function,
                               max_maturity = N)
portfolio = sample_bonds_portfolio(maturity_obj = mat_object,
                                   yield_str = y_true,
                                   number_of_bonds = number_of_bonds, 
                                   max_maturity = max_maturity,
                                   noise = 1,
                                   filter_90 = T)

fb_est = fb_solve(c_mat = portfolio$Cashflow, 
                  price_vec = portfolio$Price, 
                  max_mat = N)
portfolio_info_true = get_input_for_weights(portfolio$Cashflow, portfolio$True_price)
portfolio_info_obs = get_input_for_weights(portfolio$Cashflow, portfolio$Price)
obs_inv_weigths = get_inv_weights(portfolio_info_obs$Duration, portfolio$Price)
kr_fit = KR_solve(portfolio$Cashflow, portfolio$Price, ridge = 1, inv_w =obs_inv_weigths,K = K)


pred_price_fb = rep(0,number_of_bonds)
for (i in 1:number_of_bonds) {
  days_with_payment = which(portfolio$Cashflow[i,]!=0)
  pred_price_fb[i] = portfolio$Cashflow[i, days_with_payment] %*% fb_est$g[days_with_payment]
}
fb_ytm = get_input_for_weights(portfolio$Cashflow, pred_price_fb)$Yield_to_maturity

pred_price_kr = rep(0,number_of_bonds)
for (i in 1:number_of_bonds) {
  days_with_payment = which(portfolio$Cashflow[i,]!=0)
  pred_price_kr[i] = portfolio$Cashflow[i, days_with_payment] %*% kr_fit$g[days_with_payment]
}
kr_ytm = get_input_for_weights(portfolio$Cashflow, pred_price_kr)$Yield_to_maturity
g5 = ggplot(data = data.frame(portfolio_info_true, fb_ytm, kr_ytm), 
       aes(x = Time_to_maturity/365)) + 
  geom_point(aes(y = fb_ytm, colour = "fb"), size = 1, shape = 3) +
  geom_line(aes(y = fb_ytm, colour = "fb"), alpha = .2)+
  geom_point(aes(y = kr_ytm, colour = "kr"), size = 1, shape = 4) +
  geom_line(aes(y = kr_ytm, colour = "kr"), alpha = .2)+
  geom_point(aes(y = Yield_to_maturity, colour = "true"), size = 1,  shape = 21) +
  scale_color_manual(name = "",
                     labels = c("Fama-Bliss", "True YTM", "Kernel-Ridge"),
                     breaks = c("fb", "true", "kr"),
                     values = c("darkgreen", "darkblue", "darkred")) +
  labs(x = "Time-to-maturity (ttm) in years",
         y = "Yield-to-maturity (YTM)")+
  guides(colour = guide_legend(override.aes = list(size = 3,
                                                   shape = c(3,21,4), 
                                                   lty = "blank")))+
  scale_x_continuous(expand = c(0.02,0,0.02,0))+
  scale_y_continuous(expand = c(0.02,0,0.1,0)) + 
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.text=element_text(size=15),
        legend.position = "bottom",
        strip.text.x = element_text(size = 20)) 
g5

ggsave(filename = "comp_fit.png", 
       plot= g5, 
       path = path, 
       device='png',
       dpi=900, 
       width  = 12, 
       height = 7)
       
g5       
# ytm humped fit ----

alpha = 0.05
delta = 0
max_maturity = 30*365
number_of_bonds = 200
K = create_kernel_mat(alpha = alpha,
                      delta = delta, 
                      n_row = max_maturity, n_col = max_maturity)

set.seed(1)
y_true = sample_yield_function(weights_function = weights_function,
                               a = 0.001,
                               b = 200,
                               c = 600,
                               d = 5000,
                               e = 2,
                               max_maturity = N)
portfolio = sample_bonds_portfolio(maturity_obj = mat_object,
                                   yield_str = y_true,
                                   number_of_bonds = number_of_bonds, 
                                   max_maturity = max_maturity,
                                   noise = 1,
                                   filter_90 = T)

fb_est = fb_solve(c_mat = portfolio$Cashflow, 
                  price_vec = portfolio$Price, 
                  max_mat = N)
portfolio_info_true = get_input_for_weights(portfolio$Cashflow, portfolio$True_price)
portfolio_info_obs = get_input_for_weights(portfolio$Cashflow, portfolio$Price)
obs_inv_weigths = get_inv_weights(portfolio_info_obs$Duration, portfolio$Price)
kr_fit = KR_solve(portfolio$Cashflow, portfolio$Price, ridge = 1, inv_w =obs_inv_weigths,K = K)

pred_price_fb = rep(0,number_of_bonds)
for (i in 1:number_of_bonds) {
  days_with_payment = which(portfolio$Cashflow[i,]!=0)
  pred_price_fb[i] = portfolio$Cashflow[i, days_with_payment] %*% fb_est$g[days_with_payment]
}
fb_ytm = get_input_for_weights(portfolio$Cashflow, pred_price_fb)$Yield_to_maturity

pred_price_kr = rep(0,number_of_bonds)
for (i in 1:number_of_bonds) {
  days_with_payment = which(portfolio$Cashflow[i,]!=0)
  pred_price_kr[i] = portfolio$Cashflow[i, days_with_payment] %*% kr_fit$g[days_with_payment]
}
kr_ytm = get_input_for_weights(portfolio$Cashflow, pred_price_kr)$Yield_to_maturity
g9 = ggplot(data = data.frame(portfolio_info_true, fb_ytm, kr_ytm), 
            aes(x = Time_to_maturity/365)) + 
  geom_point(aes(y = fb_ytm, colour = "fb"), size = 1, shape = 3) +
  geom_line(aes(y = fb_ytm, colour = "fb"), alpha = .2)+
  geom_point(aes(y = kr_ytm, colour = "kr"), size = 1, shape = 4) +
  geom_line(aes(y = kr_ytm, colour = "kr"), alpha = .2)+
  geom_point(aes(y = Yield_to_maturity, colour = "true"), size = 1,  shape = 21) +
  scale_color_manual(name = "",
                     labels = c("Fama-Bliss", "True YTM", "Kernel-Ridge"),
                     breaks = c("fb", "true", "kr"),
                     values = c("darkgreen", "darkblue", "darkred")) +
  labs(x = "Time-to-maturity (ttm) in years",
       y = "Yield-to-maturity (YTM)")+
  guides(colour = guide_legend(override.aes = list(size = 3,
                                                   shape = c(3,21,4), 
                                                   lty = "blank")))+
  scale_x_continuous(expand = c(0.02,0,0.02,0))+
  scale_y_continuous(expand = c(0.02,0,0.1,0)) + 
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.text=element_text(size=15),
        legend.position = "bottom",
        strip.text.x = element_text(size = 20)) 

g9
ggsave(filename = "comp_fit_humped.png", 
       plot= g9 , 
       path = path, 
       device='png',
       dpi=900, 
       width  = 12, 
       height = 7)


# normal fit -----
alpha = 0.05
delta = 0
max_maturity = 30*365
number_of_bonds = 200
K = create_kernel_mat(alpha = alpha,
                      delta = delta, 
                      n_row = max_maturity, n_col = max_maturity)


set.seed(1)
y_true_normal = sample_yield_function(weights_function = weights_function,
                               max_maturity = max_maturity)
portfolio = sample_bonds_portfolio(maturity_obj = mat_object,
                                   yield_str = y_true_normal,
                                   number_of_bonds = number_of_bonds, 
                                   max_maturity = max_maturity,
                                   noise = 1,
                                   filter_90 = T)

fb_est = fb_solve(c_mat = portfolio$Cashflow, 
                  price_vec = portfolio$Price, 
                  max_mat = max_maturity)
portfolio_info_true = get_input_for_weights(portfolio$Cashflow, portfolio$True_price)
portfolio_info_obs = get_input_for_weights(portfolio$Cashflow, portfolio$Price)
obs_inv_weigths = get_inv_weights(portfolio_info_obs$Duration, portfolio$Price)
kr_fit = KR_solve(portfolio$Cashflow, 
                  portfolio$Price, 
                  ridge = 1, 
                  inv_w =obs_inv_weigths,
                  K = K)

g10 = ggplot(data = data.frame(y_true_normal,
                               time = seq(1:max_maturity)/365,
                               fb = fb_est$y, 
                               kr = kr_fit$y))+ 
            aes(x = time) + 
  geom_line(aes(y = fb, colour = "fb"), alpha = 1, lty = "solid")+
  geom_line(aes(y = kr, colour = "kr"), alpha = 1, lty = "solid")+
  geom_line(aes(y = y_true_normal, colour = "true"), alpha = .7, lty = "dotted") +
  scale_color_manual(name = "",
                     labels = c("Fama-Bliss", "True yield curve", "Kernel-Ridge"),
                     breaks = c("fb", "true", "kr"),
                     values = c("darkgreen", "darkblue", "darkred")) +
  labs(x = "Time-to-maturity (ttm) in years",
       y = "Yield curve")+
  guides(colour = guide_legend(override.aes = list(size = 5,
                                                   lty = c("solid",
                                                           "dotted", 
                                                           "solid"))))+
  scale_x_continuous(expand = c(0.02,0,0.02,0))+
  scale_y_continuous(expand = c(0.02,0,0.1,0)) + 
  ylim(0,0.06)+
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.text=element_text(size=15),
        legend.position = "bottom",
        strip.text.x = element_text(size = 20)) 
g10
ggsave(filename = "fit.png", 
       plot= g10 , 
       path = path, 
       device='png',
       dpi=900, 
       width  = 12, 
       height = 7)

set.seed(1)
y_true_humped = sample_yield_function(weights_function = weights_function,
                               a = 0.001,
                               b = 200,
                               c = 600,
                               d = 5000,
                               e = 2,
                               max_maturity = max_maturity)
portfolio = sample_bonds_portfolio(maturity_obj = mat_object,
                                   yield_str = y_true_humped,
                                   number_of_bonds = number_of_bonds, 
                                   max_maturity = max_maturity,
                                   noise = 1,
                                   filter_90 = T)

fb_est = fb_solve(c_mat = portfolio$Cashflow, 
                  price_vec = portfolio$Price, 
                  max_mat = max_maturity)
kr_fit = KR_solve(portfolio$Cashflow, 
                  portfolio$Price, 
                  ridge = 1, 
                  inv_w =obs_inv_weigths,
                  K = K)



g11 = ggplot(data = data.frame(y_true_humped, 
                               fb = fb_est$y, 
                               kr = kr_fit$y), 
             aes(x = seq(1:max_maturity)/365)) + 
  geom_line(aes(y = fb, colour = "fb"), alpha = 1, lty = "solid")+
  geom_line(aes(y = kr, colour = "kr"), alpha = 1, lty = "solid")+
  geom_line(aes(y = y_true_humped, colour = "true"), alpha = .7, lty = "dotted") +
  scale_color_manual(name = "",
                     labels = c("Fama-Bliss", "True yield curve", "Kernel-Ridge"),
                     breaks = c("fb", "true", "kr"),
                     values = c("darkgreen", "darkblue", "darkred")) +
  labs(x = "Time-to-maturity (ttm) in years",
       y = "")+
  guides(colour = guide_legend(override.aes = list(size = 5,
                                                   lty = c("solid",
                                                           "dotted", 
                                                           "solid"))))+
  scale_x_continuous(expand = c(0.02,0,0.02,0))+
  scale_y_continuous(expand = c(0.02,0,0.1,0)) +
  ylim(0,0.06)+
  theme_bw()

g11

legend = gtable_filter(ggplot_gtable(ggplot_build(g10 + theme(legend.position=c(.55, .65),
                                                             legend.direction = "horizontal"))), "guide-box")

g12 = gridExtra::grid.arrange(g10 + theme(legend.position="none"),
                             g11 + theme(legend.position="none")+
                               theme(text = element_text(size = 20),
                                     legend.text=element_text(size=15)),
                             layout_matrix= cbind(c(1,3),c(2,3)),
                             nrow = 2,
                             ncol  = 2 ,
                             legend,
                             heights=c(0.8, .1))
ggsave(filename = "fit_comp.png", 
       plot= g12 , 
       path = path, 
       device='png',
       dpi=1000, 
       width  = 13, 
       height = 7)

