library(tidyverse)
library(cowplot)

## read data ## 

rtidri_mod_all = readRDS("data/rtidri_mod_all.RDS")
loidri_mod_all = readRDS("data/loidri_mod_all.RDS")
uppidri_mod_all = readRDS("data/uppidri_mod_all.RDS")
uppvol_mod_all = readRDS("data/uppvol_mod_all.RDS")

### PLOT

size.title = 15
line.lwd = 1.2
size.label.x = 18
size.text.x = 14
size.point = 6
size.label.y = 18
size.text.y = 14
size.legend.text = 15
size.legend.title = 20
unit.legend.h = 1.8
unit.legend.w = 1.8
size.ann = 10
colour.axis = "gray20"
colour.theme = "black"
colour.axis.line = "gray20"
colour.line = "gray50"
max_size_dot = 5

## Theme to be used for all plots

theme.surv =  theme(plot.title = element_text(lineheight=.8, face="bold", size = size.title,hjust = 0.5), 
                     plot.background = element_blank()
                     ,panel.grid.major = element_blank()
                     ,panel.grid.minor = element_blank()
                     ,panel.border = element_blank()
                     ,panel.background = element_blank(),
                     axis.line = element_line(color = 'black'),
                     plot.margin = unit(c(1,2,1,1), "cm"),
                     axis.title.x = element_text(size=size.label.x,vjust=-80),
                     axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
                     axis.title.y = element_text(size=size.label.x, vjust = 2),
                     axis.text.y  = element_text(size=size.text.x),
                     legend.title = element_blank(),
                     legend.text = element_text(size = size.legend.text),
                    legend.position = c(0.9, 0.9)
) 


mod_all = rtidri_mod_all

rtidri_surv_gg = ggplot(mod_all, aes(x = final_year, y = estimate, group = type)) +
  geom_point(aes(x = final_year, y = estimate, size = samples, shape = type, alpha = type),position = position_dodge(width = 0.30), stroke = 0.8) +
  geom_errorbar(aes(x = final_year, ymin = lcl, ymax = ucl,lty = type), width = 0.3, position = position_dodge(width = 0.30),lwd = 0.2) +
  geom_line(aes(x = final_year, y = estimate, lty = type)) +
  scale_linetype_manual(values = c(1,2,2)) +
  scale_shape_manual(values=c(19, 2, 6)) +
  scale_alpha_manual(values=c(0.3, 0.6, 0.6)) +
  expand_limits(x = c(-0.2,1)) +
  ggtitle("LIdri_RT") +
  scale_size_area(breaks = c(100,200,300),"Samples", max_size=max_size_dot) + 
  theme.surv +
  scale_y_continuous(limits = c(-0.1,1)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(phi)) +
  labs(x = bquote(Y[f])) +
  guides(lty = F) +
  guides(shape = F) +
  guides(alpha = F) +
  guides(size = guide_legend(override.aes = list(alpha = 0.2)))
  
  rtidri_surv_gg

mod_all = uppidri_mod_all

uppidri_surv_gg = ggplot(mod_all, aes(x = final_year, y = estimate, group = type)) +
  geom_point(aes(x = final_year, y = estimate, size = samples, shape = type, alpha = type),position = position_dodge(width = 0.30), stroke = 0.8) +
  geom_errorbar(aes(x = final_year, ymin = lcl, ymax = ucl,lty = type), width = 0.3, position = position_dodge(width = 0.30),lwd = 0.2) +
  geom_line(aes(x = final_year, y = estimate, lty = type)) +
  scale_linetype_manual(values = c(1,2,2)) +
  scale_shape_manual(values=c(19, 2, 6)) +
  scale_alpha_manual(values=c(0.3, 0.6, 0.6)) +
  expand_limits(x = c(-0.2,1)) +
  ggtitle("UIdri_MT") +
  scale_size_area(breaks = c(200,400,600),"Samples", max_size=max_size_dot) + 
  theme.surv +
  scale_y_continuous(limits = c(-0.1,1)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(phi)) +
  labs(x = bquote(Y[f])) +
  guides(lty = F) +
  guides(shape = F) +
  guides(alpha = F) +
  guides(size = guide_legend(override.aes = list(alpha = 0.2)))


mod_all = loidri_mod_all

loidri_surv_gg = ggplot(mod_all, aes(x = final_year, y = estimate, group = type)) +
  geom_point(aes(x = final_year, y = estimate, size = samples, shape = type, alpha = type),position = position_dodge(width = 0.30), stroke = 0.8) +
  geom_errorbar(aes(x = final_year, ymin = lcl, ymax = ucl,lty = type), width = 0.3, position = position_dodge(width = 0.30),lwd = 0.2) +
  geom_line(aes(x = final_year, y = estimate, lty = type)) +
  scale_linetype_manual(values = c(1,2,2)) +
  scale_shape_manual(values=c(19, 2, 6)) +
  scale_alpha_manual(values=c(0.3, 0.6, 0.6)) +
  expand_limits(x = c(-0.2,1)) +
  ggtitle("LIdri_MT") +
  scale_size_area(breaks = c(400,600,800),"Samples", max_size=max_size_dot) + 
  theme.surv +
  scale_y_continuous(limits = c(-0.1,1)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(phi)) +
  labs(x = bquote(Y[f])) +
  guides(lty = F) +
  guides(shape = F) +
  guides(alpha = F) +
  guides(size = guide_legend(override.aes = list(alpha = 0.2)))


mod_all = uppvol_mod_all

uppvol_surv_gg = ggplot(mod_all, aes(x = final_year, y = estimate, group = type)) +
  geom_point(aes(x = final_year, y = estimate, size = samples, shape = type, alpha = type),position = position_dodge(width = 0.30), stroke = 0.8) +
  geom_errorbar(aes(x = final_year, ymin = lcl, ymax = ucl,lty = type), width = 0.3, position = position_dodge(width = 0.30),lwd = 0.2) +
  geom_line(aes(x = final_year, y = estimate, lty = type)) +
  scale_linetype_manual(values = c(1,2,2)) +
  scale_shape_manual(values=c(19, 2, 6)) +
  scale_alpha_manual(values=c(0.3, 0.6, 0.6)) +
  expand_limits(x = c(-0.2,1)) +
  ggtitle("UVol_BT") +
  scale_size_area(breaks = c(1000,1500,2000),"Samples", max_size=max_size_dot) + 
  theme.surv +
  scale_y_continuous(limits = c(-0.1,1)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(phi)) +
  labs(x = bquote(Y[f])) +
  guides(lty = F) +
  guides(shape = F) +
  guides(alpha = F) +
  guides(size = guide_legend(override.aes = list(alpha = 0.2)))


Plot_surv_all = plot_grid(loidri_surv_gg,
                          uppidri_surv_gg,
                          rtidri_surv_gg,
                          uppvol_surv_gg,
                          labels = c("A", "B","C","D"),
                          nrow = 2, align = "v",hjust = -2.5)

save_plot("Plots/Plot_surv_all.pdf", Plot_surv_all,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.7
)

