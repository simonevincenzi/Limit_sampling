library(tidyverse)
library(cowplot)
library(data.table)

## Read data ##

loidri_df =  as.data.frame(fread("https://raw.githubusercontent.com/simonevincenzi/Heter/master/raw_data/loidri_df_pieced.csv")) 
loidri_df$Date = as.Date(loidri_df$Date,format = "%m/%d/%Y") # Y is year with century
loidri_df = loidri_df %>%
  arrange(.,Mark_cor,Date)


uppidri_df =  as.data.frame(fread("https://raw.githubusercontent.com/simonevincenzi/Heter/master/raw_data/uppidri_df_pieced.csv")) 
uppidri_df$Date = as.Date(uppidri_df$Date,format = "%m/%d/%Y") # Y is year with century
uppidri_df = uppidri_df %>%
  arrange(.,Mark_cor,Date)


rtidri_df = as.data.frame(fread("https://raw.githubusercontent.com/simonevincenzi/Heter/master/raw_data/rtidri_df_pieced.csv"))


uppvol.df = as.data.frame(fread("https://raw.githubusercontent.com/simonevincenzi/Heter/master/raw_data/uppvol_2015_complete.csv")) 


loidri_growth_all_df = readRDS("data/loidri_growth_all_df.RDS")
uppidri_growth_all_df = readRDS("data/uppidri_growth_all_df.RDS")
rtidri_growth_all_df = readRDS("data/rtidri_growth_all_df.RDS")
uppvol_growth_all_df = readRDS("data/uppvol_growth_all_df.RDS")

### PLOT

size.title = 15
line.lwd = 1.2
size.label.x = 18
size.text.x = 14
size.point = 4
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
label.T = "Heterozygosity"
max_size_dot = 8

## Theme to be used for all plots

theme.growth =  theme(plot.title = element_text(lineheight=.8, face="bold", size = size.title,hjust = 0.5), 
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
                    legend.position = c(0.8, 0.9),
                    legend.key = element_rect(fill = "white")) 




loidri_gr_gg = ggplot(loidri_growth_all_df, aes(x = final_year, y = linf, group = type)) +
  geom_point(aes(x = final_year, y = linf, size = Samples, shape = type),
             alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_point(aes(x = final_year, y = linf, size = size.point), position = position_dodge(width = 0.50)) +
  geom_errorbar(aes(x = final_year, ymin = linf_lcl, ymax = linf_ucl), width = 0.3,position = position_dodge(width = 0.50), lty = 2) +
  scale_size_area(breaks=c(200, 400, 600), "Samples", max_size=max_size_dot) + 
  guides(shape = F) +
  guides(size = guide_legend(override.aes = list(alpha = 0.2))) +
  ggtitle("LIdri_MT") +

  
  theme.growth +
  scale_y_continuous(limits = c(150,800)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(L[infinity](mm))) +
  labs(x = bquote(Y[f]))


uppidri_gr_gg = ggplot(uppidri_growth_all_df, aes(x = final_year, y = linf, group = type)) +
  geom_point(aes(x = final_year, y = linf, size = Samples, shape = type), alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_point(aes(x = final_year, y = linf, size = size.point), position = position_dodge(width = 0.50)) +
  geom_errorbar(aes(x = final_year, ymin = linf_lcl, ymax = linf_ucl), width = 0.3,position = position_dodge(width = 0.50), lty = 2) +
  scale_size_area(breaks=c(200, 400, 600), "Samples", max_size=max_size_dot) + 
  guides(shape = F) +
  guides(size = guide_legend(override.aes = list(alpha = 0.2))) +
  ggtitle("UIdri_MT") +
  
  
  theme.growth +
  scale_y_continuous(limits = c(150,800)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(L[infinity](mm))) +
  labs(x = bquote(Y[f])) 


rtidri_gr_gg = ggplot(rtidri_growth_all_df, aes(x = final_year, y = linf, group = type)) +
  geom_point(aes(x = final_year, y = linf, size = Samples, shape = type), alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_point(aes(x = final_year, y = linf, size = size.point), position = position_dodge(width = 0.50)) +
  geom_errorbar(aes(x = final_year, ymin = linf_lcl, ymax = linf_ucl), width = 0.3,position = position_dodge(width = 0.50), lty = 2) +
  scale_size_area(breaks = c(20,50,80),"Samples", max_size=max_size_dot) + 
  guides(shape = F) +
  guides(size = guide_legend(override.aes = list(alpha = 0.2))) +
  ggtitle("LIdri_RT") +
  theme.growth +
  scale_y_continuous(limits = c(150,800)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(L[infinity](mm))) +
  labs(x = bquote(Y[f])) 

rtidri_gr_gg

uppvol_gr_gg = ggplot(uppvol_growth_all_df, aes(x = final_year, y = linf, group = type)) +
  geom_point(aes(x = final_year, y = linf, size = Samples, shape = type), alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_point(aes(x = final_year, y = linf, size = size.point), position = position_dodge(width = 0.50)) +
  geom_errorbar(aes(x = final_year, ymin = linf_lcl, ymax = linf_ucl), width = 0.3,position = position_dodge(width = 0.50), lty = 2) +
  scale_size_area(breaks=c(1000, 1500, 2000),"Samples", max_size=max_size_dot) + 
  guides(shape = F) +
  guides(size = guide_legend(override.aes = list(alpha = 0.2))) +
  ggtitle("UVol_BT") +
  theme.growth +
  scale_y_continuous(limits = c(100,500)) +
  scale_x_continuous(limits = c(2004,2016), breaks = seq(2006,2015,2)) +
  labs(y = bquote(L[infinity](mm))) +
  labs(x = bquote(Y[f])) 

uppvol_gr_gg

Plot_linf_all = plot_grid(loidri_gr_gg,
                          uppidri_gr_gg,
                          rtidri_gr_gg,
                          uppvol_gr_gg,
                          labels = c("A", "B","C","D"),
                                nrow = 2, align = "v",hjust = -2.5)

save_plot("Plots/Plot_linf_all.pdf", Plot_linf_all,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.7
)




############

### PLOT

size.title = 15
line.lwd = 1.5
size.label.x = 18
size.text.x = 14
size.point = 2
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
label.T = "Heterozygosity"
max_size_dot = 8
alpha.min = 0.35

## Theme to be used for all plots

theme.tr =  theme(plot.title = element_text(lineheight=.8, face="bold", size = size.title,hjust = 0.5), 
                      plot.background = element_blank()
                      ,panel.grid.major = element_blank()
                      ,panel.grid.minor = element_blank()
                      ,panel.border = element_blank()
                      ,panel.background = element_blank(),
                      axis.line = element_line(color = 'black'),
                      plot.margin = unit(c(1,2,1,1), "cm"),
                      axis.title.x = element_text(size=size.label.x,vjust=-1),
                      axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
                      axis.title.y = element_text(size=size.label.x, vjust = 2),
                      axis.text.y  = element_text(size=size.text.x),
                      legend.title = element_blank(),
                      legend.text = element_text(size = size.legend.text),
                      legend.position = c(0.1, 0.9),
                      legend.key = element_rect(fill = "white")) 


######

require(MASS)

VB.f = function (Linf,k,t0,Age) {
  
  Length_age = Linf*(1-exp(-k*(Age-t0)))
  
  return(Length_age)
}


#### 

rm(data_growth)

first_y = 2006
last_y = 2014
max_age = 10

pop.growth.prep = arrange(loidri_df,Mark_cor,Year,Month)

data_growth = pop.growth.prep %>%
  filter(.,!is.na(Mark_cor), Month == 9 , Year <= first_y, Age_cor >=1) %>%
  dplyr::select(.,-Mark, -Cohort, -Age, -Date) %>%
  add_column(final_year = rep(as.character((first_y)),nrow(.))) %>%
  bind_rows(., pop.growth.prep %>%
              filter(.,!is.na(Mark_cor), Month == 9 , Year <= last_y, Age_cor >=1) %>%
              dplyr::select(.,-Mark, -Cohort, -Age, -Date) %>%
              add_column(final_year = rep(as.character((last_y)),nrow(.))))


loidri_first_tr = tibble(Length = VB.f(filter(loidri_growth_all_df, final_year == first_y,
                                               type == "rand_eff")$linf,
                                        filter(loidri_growth_all_df, final_year == first_y,
                                               type == "rand_eff")$k,
                                        filter(loidri_growth_all_df, final_year == first_y,
                                               type == "rand_eff")$t0,1:max_age), Age = 1:max_age, Year = as.character(first_y))

loidri_last_tr = tibble(Length = VB.f(filter(loidri_growth_all_df, final_year == last_y,
                                              type == "rand_eff")$linf,
                                       filter(loidri_growth_all_df, final_year == last_y,
                                              type == "rand_eff")$k,
                                       filter(loidri_growth_all_df, final_year == last_y,
                                              type == "rand_eff")$t0,1:max_age), Age = 1:max_age, Year = as.character(last_y))

loidri_tr_df = bind_rows(loidri_first_tr,loidri_last_tr)

loidri_tr_gg = ggplot(loidri_tr_df, aes(x = Age, y = Length, group = Year)) +
  #geom_point(aes(shape = Year), size = size.point, alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_line(aes(lty = Year, col = Year), lwd = line.lwd) + 
  geom_point(data = data_growth, aes (x = Age_cor, y = Length, col = final_year, alpha = final_year), position = position_dodge(width = 0.5), size = size.point) +
  scale_color_manual(values = c("black","gray50")) +
  scale_alpha_manual(values = c(1,alpha.min)) +
  scale_linetype_manual(values = c(1,1)) +
  #guides(col = F) +
  guides(alpha = F) +
  #guides(lty = F) +

  ggtitle("LIdri_MT") +
  
  theme.tr +
  scale_y_continuous(limits = c(100,500)) +
  scale_x_continuous(limits = c(0,(max_age+1)), breaks = seq(1,max_age,1)) +
  labs(y = "Length (mm)") +
  labs(x = "Age") 
loidri_tr_gg



## tag-recapture data

rm(data_growth)

first_y = 2006
last_y = 2014
max_age = 10

pop.growth.prep = arrange(uppidri_df,Mark_cor,Year,Month)

data_growth = pop.growth.prep %>%
  filter(.,!is.na(Mark_cor), Month == 9 , Year <= first_y, Age_cor >=1) %>%
  dplyr::select(.,-Mark, -Cohort, -Age, -Date) %>%
  add_column(final_year = rep(as.character((first_y)),nrow(.))) %>%
  bind_rows(., pop.growth.prep %>%
              filter(.,!is.na(Mark_cor), Month == 9 , Year <= last_y, Age_cor >=1) %>%
              dplyr::select(.,-Mark, -Cohort, -Age, -Date) %>%
              add_column(final_year = rep(as.character((last_y)),nrow(.))))


uppidri_first_tr = tibble(Length = VB.f(filter(uppidri_growth_all_df, final_year == first_y,
                                              type == "rand_eff")$linf,
                                       filter(uppidri_growth_all_df, final_year == first_y,
                                              type == "rand_eff")$k,
                                       filter(uppidri_growth_all_df, final_year == first_y,
                                              type == "rand_eff")$t0,1:max_age), Age = 1:max_age, Year = as.character(first_y))

uppidri_last_tr = tibble(Length = VB.f(filter(uppidri_growth_all_df, final_year == last_y,
                                             type == "rand_eff")$linf,
                                      filter(uppidri_growth_all_df, final_year == last_y,
                                             type == "rand_eff")$k,
                                      filter(uppidri_growth_all_df, final_year == last_y,
                                             type == "rand_eff")$t0,1:max_age), Age = 1:max_age, Year = as.character(last_y))

uppidri_tr_df = bind_rows(uppidri_first_tr,uppidri_last_tr)

uppidri_tr_gg = ggplot(uppidri_tr_df, aes(x = Age, y = Length, group = Year)) +
  #geom_point(aes(shape = Year), size = size.point, alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_line(aes(lty = Year, col = Year), lwd = line.lwd) + 
  geom_point(data = data_growth, aes (x = Age_cor, y = Length, col = final_year, alpha = final_year), position = position_dodge(width = 0.5), size = size.point) +
  scale_color_manual(values = c("black","gray50")) +
  scale_alpha_manual(values = c(1,alpha.min)) +
  scale_linetype_manual(values = c(1,1)) +
  guides(col = F) +
  guides(alpha = F) +
  guides(lty = F) +
  ggtitle("UIdri_MT") +
  
  theme.tr +
  scale_y_continuous(limits = c(100,500)) +
  scale_x_continuous(limits = c(0,(max_age+1)), breaks = seq(1,max_age,1)) +
  labs(y = "Length (mm)") +
  labs(x = "Age") 
uppidri_tr_gg





## tag-recapture data

rm(data_growth)

first_y = 2006
last_y = 2014
max_age = 10

pop.growth.prep = arrange(rtidri_df,Mark_cor,Year,Month)

data_growth = pop.growth.prep %>%
  filter(.,!is.na(Mark_cor), Month == 9 , Year <= first_y, Age_cor >=1) %>%
  dplyr::select(.,-Mark, -Cohort, -Age, -Date) %>%
  add_column(final_year = rep(as.character((first_y)),nrow(.))) %>%
  bind_rows(., pop.growth.prep %>%
              filter(.,!is.na(Mark_cor), Month == 9 , Year <= last_y, Age_cor >=1) %>%
              dplyr::select(.,-Mark, -Cohort, -Age, -Date) %>%
              add_column(final_year = rep(as.character((last_y)),nrow(.))))


rtidri_first_tr = tibble(Length = VB.f(filter(rtidri_growth_all_df, final_year == first_y,
                                               type == "rand_eff")$linf,
                                        filter(rtidri_growth_all_df, final_year == first_y,
                                               type == "rand_eff")$k,
                                        filter(rtidri_growth_all_df, final_year == first_y,
                                               type == "rand_eff")$t0,1:max_age), Age = 1:max_age, Year = as.character(first_y))

rtidri_last_tr = tibble(Length = VB.f(filter(rtidri_growth_all_df, final_year == last_y,
                                              type == "rand_eff")$linf,
                                       filter(rtidri_growth_all_df, final_year == last_y,
                                              type == "rand_eff")$k,
                                       filter(rtidri_growth_all_df, final_year == last_y,
                                              type == "rand_eff")$t0,1:max_age), Age = 1:max_age, Year = as.character(last_y))

rtidri_tr_df = bind_rows(rtidri_first_tr,rtidri_last_tr)

rtidri_tr_gg = ggplot(rtidri_tr_df, aes(x = Age, y = Length, group = Year)) +
  #geom_point(aes(shape = Year), size = size.point, alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_line(aes(lty = Year, col = Year), lwd = line.lwd) + 
  geom_point(data = data_growth, aes (x = Age_cor, y = Length, col = final_year, alpha = final_year), position = position_dodge(width = 0.5), size = size.point) +
  scale_color_manual(values = c("black","gray50")) +
  scale_alpha_manual(values = c(1,alpha.min)) +
  scale_linetype_manual(values = c(1,1)) +
  guides(col = F) +
  guides(alpha = F) +
  guides(lty = F) +
  ggtitle("LIdri_RT") +
  
  theme.tr +
  scale_y_continuous(limits = c(100,500)) +
  scale_x_continuous(limits = c(0,(max_age+1)), breaks = seq(1,max_age,1)) +
  labs(y = "Length (mm)") +
  labs(x = "Age") 
rtidri_tr_gg







## tag-recapture data

rm(data_growth)

first_y = 2006
last_y = 2014
max_age = 10

pop.growth.prep = arrange(uppvol.df,Mark,Year,Month)

data_growth = pop.growth.prep %>%
  filter(.,!is.na(Mark), Month == 9 , Year <= first_y, Age >=1) %>%
  add_column(final_year = rep(as.character((first_y)),nrow(.))) %>%
  bind_rows(., pop.growth.prep %>%
              filter(.,!is.na(Mark), Month == 9 , Year <= last_y, Age >=1) %>%
              add_column(final_year = rep(as.character((last_y)),nrow(.))))


uppvol_first_tr = tibble(Length = VB.f(filter(uppvol_growth_all_df, final_year == first_y,
                                              type == "rand_eff")$linf,
                                       filter(uppvol_growth_all_df, final_year == first_y,
                                              type == "rand_eff")$k,
                                       filter(uppvol_growth_all_df, final_year == first_y,
                                              type == "rand_eff")$t0,1:max_age), Age = 1:max_age, Year = as.character(first_y))

uppvol_last_tr = tibble(Length = VB.f(filter(uppvol_growth_all_df, final_year == last_y,
                                             type == "rand_eff")$linf,
                                      filter(uppvol_growth_all_df, final_year == last_y,
                                             type == "rand_eff")$k,
                                      filter(uppvol_growth_all_df, final_year == last_y,
                                             type == "rand_eff")$t0,1:max_age), Age = 1:max_age, Year = as.character(last_y))

uppvol_tr_df = bind_rows(uppvol_first_tr,uppvol_last_tr)

uppvol_tr_gg = ggplot(uppvol_tr_df, aes(x = Age, y = Length, group = Year)) +
  #geom_point(aes(shape = Year), size = size.point, alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_line(aes(lty = Year, col = Year), lwd = line.lwd) + 
  geom_point(data = data_growth, aes (x = Age, y = Length, col = final_year, alpha = final_year), position = position_dodge(width = 0.5), size = size.point) +
  scale_color_manual(values = c("black","gray50")) +
  scale_alpha_manual(values = c(1,alpha.min)) +
  scale_linetype_manual(values = c(1,1)) +
  guides(col = F) +
  guides(alpha = F) +
  guides(lty = F) +
  ggtitle("UVol_BT") +
  
  theme.tr +
  scale_y_continuous(limits = c(100,500)) +
  scale_x_continuous(limits = c(0,(max_age+1)), breaks = seq(1,max_age,1)) +
  labs(y = "Length (mm)") +
  labs(x = "Age") 
uppvol_tr_gg




Plot_tr_all = plot_grid(loidri_tr_gg,
                          uppidri_tr_gg,
                          rtidri_tr_gg,
                          uppvol_tr_gg,
                          labels = c("A", "B","C","D"),
                          nrow = 2, align = "v",hjust = -2.5)

save_plot("Plots/Plot_tr_all.pdf", Plot_tr_all,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.7
)
