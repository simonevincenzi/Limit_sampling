library(tidyverse)
library(cowplot)

res_df_mean = readRDS("data/res_df_mean.RDS")

leg.x = 0.1
leg.y = 0.25
source("theme_plot.r")


mean_dens_loidri_gg = ggplot(filter(res_df_mean, Stream == "LIdri_MT"), aes(x = year_max, y = mean_dens)) +
  geom_point(position = position_dodge(width = 0.40), stroke = 0.8, size = size.point) +
  geom_errorbar(aes(x = year_max, ymin = mean_25, ymax = mean_975), width = 0.35, lty = 2) +
  geom_hline(yintercept = filter(obs_pop, Pop == "LIdri_MT")$mean_dens, linetype = 2, lwd = line.lwd) +
  theme.pop +
  scale_y_continuous(limits = c(0,1000)) +
  scale_x_continuous(limits = c(2005.5,2014.5), breaks = c(2006,2008,2010,2012,2014)) +
  labs(y = bquote(paste("Fish ",ha^-1))) +
  labs(x = bquote(Y[f])) + 
  ggtitle("LIdri_MT")
mean_dens_loidri_gg


mean_dens_uppidri_gg = ggplot(filter(res_df_mean, Stream == "UIdri_MT"), aes(x = year_max, y = mean_dens)) +
  geom_point(position = position_dodge(width = 0.40), stroke = 0.8, size = size.point) +
  geom_errorbar(aes(x = year_max, ymin = mean_25, ymax = mean_975), width = 0.35, lty = 2) +
  geom_hline(yintercept = filter(obs_pop, Pop == "UIdri_MT")$mean_dens, linetype = 2, lwd = line.lwd) +
  theme.pop +
  scale_y_continuous(limits = c(0,1000)) +
  scale_x_continuous(limits = c(2005.5,2014.5), breaks = c(2006,2008,2010,2012,2014)) +
  labs(y = bquote(paste("Fish ",ha^-1))) +
  labs(x = bquote(Y[f])) + 
  ggtitle("UIdri_MT")
mean_dens_uppidri_gg


mean_dens_rtidri_gg = ggplot(filter(res_df_mean, Stream == "LIdri_RT"), aes(x = year_max, y = mean_dens)) +
  geom_point(position = position_dodge(width = 0.40), stroke = 0.8, size = size.point) +
  geom_errorbar(aes(x = year_max, ymin = mean_25, ymax = mean_975), width = 0.35, lty = 2) +
  geom_hline(yintercept = filter(obs_pop, Pop == "LIdri_RT")$mean_dens, linetype = 2, lwd = line.lwd) +
  theme.pop +
  scale_y_continuous(limits = c(0,300)) +
  scale_x_continuous(limits = c(2005.5,2014.5), breaks = c(2006,2008,2010,2012,2014)) +
  labs(y = bquote(paste("Fish ",ha^-1))) +
  labs(x = bquote(Y[f])) + 
  ggtitle("LIdri_RT")
mean_dens_rtidri_gg


mean_dens_uppvol_gg = ggplot(filter(res_df_mean, Stream == "UVol_BT"), aes(x = year_max, y = mean_dens)) +
  geom_point(position = position_dodge(width = 0.40), stroke = 0.8, size = size.point) +
  geom_errorbar(aes(x = year_max, ymin = mean_25, ymax = mean_975), width = 0.35, lty = 2) +
  geom_hline(yintercept = filter(obs_pop, Pop == "UVol")$mean_dens, linetype = 2, lwd = line.lwd) +
  theme.pop +
  scale_y_continuous(limits = c(3000,6000)) +
  scale_x_continuous(limits = c(2005.5,2014.5), breaks = c(2006,2008,2010,2012,2014)) +
  labs(y = bquote(paste("Fish ",ha^-1))) +
  labs(x = bquote(Y[f])) + 
  ggtitle("UVol_BT")
mean_dens_uppvol_gg  




Plot_dens_sim = plot_grid(mean_dens_loidri_gg,
                          mean_dens_uppidri_gg,
                          mean_dens_rtidri_gg,
                          mean_dens_uppvol_gg,
                          labels = c("A", "B","C","D"),
                          nrow = 2, align = "v",hjust = -2.5)

save_plot("Plots/Plot_dens_sim.pdf", Plot_dens_sim,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.7)
