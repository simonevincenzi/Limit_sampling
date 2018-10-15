library(tidyverse)
library(cowplot)
density.idri = readRDS("data/density.all.idri.RDS")
density.uppvol = readRDS("data/density.uppvol.RDS")
density.uppvol$Pop = "UVol"
density.uppidri.vol.df = bind_rows(density.idri,density.uppvol)

### PLOT

size.title = 15
line.lwd = 1.2
size.label.x = 18
size.text.x = 14
size.point = 3
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

theme.dens =  theme(plot.title = element_text(lineheight=.8, face="bold", size = size.title,hjust = 0.5), 
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
                    legend.position = c(0.1, 0.9)
) 



loidri_dens_gg = ggplot(filter(density.uppidri.vol.df,Age == 1, Season == "Autumn",Pop == "LIdri_MT"), aes(x = Year, y = Dest)) +
  geom_point(size =size.point ) +
  geom_errorbar(aes(x = Year, ymin = DLCI, ymax = DUCI), width = 0.6, lwd = 0.5,lty = 2) +
  geom_line() +
  #expand_limits(x = c(-0.2,1)) +
  ggtitle("LIdri_MT") +
  theme.dens +
  scale_y_continuous(limits = c(0,1500)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(paste("Fish ",ha^-1)))  +
  labs(x = "Year") +
  guides(lty = F) +
  guides(shape = F) +
  guides(alpha = F) 
 # guides(size = guide_legend(override.aes = list(alpha = 0.2)))
loidri_dens_gg

uppidri_dens_gg = ggplot(filter(density.uppidri.vol.df,Age == 1, Season == "Autumn",Pop == "UIdri_MT"), aes(x = Year, y = Dest)) +
  geom_point(size =size.point ) +
  geom_errorbar(aes(x = Year, ymin = DLCI, ymax = DUCI), width = 0.6, lwd = 0.5,lty = 2) +
  geom_line() +
  #expand_limits(x = c(-0.2,1)) +
  ggtitle("UIdri_MT") +
  theme.dens +
  scale_y_continuous(limits = c(0,1500)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(paste("Fish ",ha^-1)))  +
  labs(x = "Year") +
  guides(lty = F) +
  guides(shape = F) +
  guides(alpha = F) 
# guides(size = guide_legend(override.aes = list(alpha = 0.2)))
uppidri_dens_gg


rtidri_dens_gg = ggplot(filter(density.uppidri.vol.df,Age == 1, Season == "Autumn",Pop == "LIdri_RT"), aes(x = Year, y = Dest)) +
  geom_point(size =size.point ) +
  geom_errorbar(aes(x = Year, ymin = DLCI, ymax = DUCI), width = 0.6, lwd = 0.5,lty = 2) +
  geom_line() +
  #expand_limits(x = c(-0.2,1)) +
  ggtitle("LIdri_RT") +
  theme.dens +
  scale_y_continuous(limits = c(0,300)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(paste("Fish ",ha^-1)))  +
  labs(x = "Year") +
  guides(lty = F) +
  guides(shape = F) +
  guides(alpha = F) 
# guides(size = guide_legend(override.aes = list(alpha = 0.2)))
rtidri_dens_gg

uppvol_dens_gg = ggplot(filter(density.uppidri.vol.df,Age == 1, Season == "Autumn",Pop == "UVol"), aes(x = Year, y = Dest)) +
  geom_point(size =size.point ) +
  geom_errorbar(aes(x = Year, ymin = DLCI, ymax = DUCI), width = 0.6, lwd = 0.5,lty = 2) +
  geom_line() +
  #expand_limits(x = c(-0.2,1)) +
  ggtitle("UVol_BT") +
  theme.dens +
  scale_y_continuous(limits = c(0,9000),breaks = c(0,3000,6000,9000)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(paste("Fish ",ha^-1)))  +
  labs(x = "Year") +
  guides(lty = F) +
  guides(shape = F) +
  guides(alpha = F) 
# guides(size = guide_legend(override.aes = list(alpha = 0.2)))
uppvol_dens_gg


Plot_dens_all = plot_grid(loidri_dens_gg,
                          uppidri_dens_gg,
                          rtidri_dens_gg,
                          uppvol_dens_gg,
                          labels = c("A", "B","C","D"),
                          nrow = 2, align = "v",hjust = -2.5)

save_plot("Plots/Plot_dens_all.pdf", Plot_dens_all,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.7)
  