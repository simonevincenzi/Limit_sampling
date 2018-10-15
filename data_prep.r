library(tidyverse)
library(data.table)

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
  




size.title = 15
line.lwd = 0.5
size.label.x = 18
size.text.x = 14
size.point = 5
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

theme.pop =  theme(plot.title = element_text(lineheight=.8, face="bold", size = size.title,hjust = 0.5), 
                   plot.background = element_blank()
                   ,panel.grid.major = element_blank()
                   ,panel.grid.minor = element_blank()
                   ,panel.border = element_blank()
                   ,panel.background = element_blank(),
                   axis.line = element_line(color = 'black'),
                   plot.margin = unit(c(1,2,1,1), "cm"),
                   axis.title.x = element_text(size=size.label.x,vjust=-80),
                   axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
                   axis.title.y = element_text(size=size.label.x, vjust = 8),
                   axis.text.y  = element_text(size=size.text.x),
                   legend.title = element_blank(),
                   legend.text = element_text(size = size.legend.text),
                   legend.spacing.y = unit(5,"cm"),
                   legend.position = c(0.15, 0.9),
                   legend.key = element_rect(fill = "white", size = 5),
                   legend.key.size = unit(1.5,"lines")) 


m_idri_low = dplyr::select(loidri_df, - Date, -adc, -Mark_cor) 
m_idri_upp = dplyr::select(uppidri_df, - Date, -adc, -Mark_cor) 
b_vol = dplyr::select(uppvol.df, - Date, -adc)
b_vol$Pop = "UVol_BT"
r_idri_low = dplyr::select(rtidri_df, - Date, -adc, -Mark_cor)

all_df = bind_rows(m_idri_low,m_idri_upp, b_vol, r_idri_low) %>%
  filter(., Month == 9) %>%
  group_by(Pop, Year) %>%
  summarise(n = n()) %>%  
  group_by(Pop) %>%
  mutate(cn = cumsum(n)) %>%
  ggplot(aes(x = Year, y = cn, group = Pop,  pch = Pop)) +
  geom_point(size = size.point) + 
  geom_line(lwd = line.lwd, lty = 2) + 
  theme.pop +
  scale_x_continuous(breaks = c(2004,2006,2008,2010,2012,2014)) +
  scale_y_continuous(limits = c(0,6000), breaks = c(0,1000,2000,3000,4000,5000,6000)) +
  scale_shape_manual(values = c(24,25,21,22)) +
  labs(y = "Cumulative sum of tagged fish", x = bquote(Y[f]))

all_df

ggsave(filename = "cum_sum.pdf",plot = all_df, width = 8, height = 7)
save_plot("cum_sum.pdf", all_df
)


