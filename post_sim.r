# read the RDS files with simulations results (already done, so they are in data/)

res_list_tot = c(readRDS("data/res_list1.rds"),readRDS("data/res_list2.rds"),readRDS("data/res_list3.rds"),readRDS("data/res_list4.rds"),readRDS("data/res_list5.rds"),readRDS("data/res_list6.rds"),readRDS("data/res_list7.rds"),readRDS("data/res_list8.rds"),readRDS("data/res_list9.rds"),readRDS("data/res_list10.rds"))


## I only keep some results in the output list of the function

col_sel = which(names(res_list_tot[[1]]) %in% c("extinct","yearextinct","Stream", "year_max","quasi_ext_100","quasi_ext_100_cont","quasi_ext_200","quasi_ext_200_cont","quasi_ext_50","quasi_ext_50_cont","dens_cv","dens_mean"))                                                


## maybe there is a faster way to subset lists, but I could not find it

for (i in 1:length(res_list_tot)) {
  
  res_list_tot[[i]] = res_list_tot[[i]][col_sel]
  
}                                                                                            


library(plyr)
res_df =  ldply (res_list_tot, data.frame) %>% 
  as.tibble()

# res_df$strategy = as.factor((res_df$strategy))
# res_df$set_closure = as.factor((res_df$set_closure))
# res_df$mult_targ = as.factor((res_df$mult_targ))
# 
# # I call the strategies Mean, Min, and G_mean (geometric mean) 
# 
# res_df$strategy = mapvalues(res_df$strategy,from = c("2", "3", "4"), to = c("Mean", "Min", "G_mean"))
# 
# # it conflicts with dplyr 

detach(package:plyr)

# I compute means and CIs of variables of interest

res_df_mean = 
  filter(res_df, extinct == 0) %>%
  group_by(year_max,Stream) %>%
  summarise(mean_dens = mean(dens_mean, na.rm =T),
            n = n(),
            mean_cv = mean(dens_cv, na.rm = T)) %>%
  left_join(., filter(res_df, extinct == 0) %>%
              group_by(year_max,Stream) %>%
              do(data.frame(t(quantile(.$dens_mean, probs = c(0.025, 0.975)))))) %>%
  rename(mean_25 = `X2.5.`, mean_975 = `X97.5.`) %>%
  left_join(., filter(res_df, extinct == 0) %>%
              group_by(year_max,Stream) %>%
              do(data.frame(t(quantile(.$dens_cv, probs = c(0.025, 0.975)))))) %>%
  rename(cv_25 = `X2.5.`, cv_975 = `X97.5.`) %>%
  
  full_join(., filter(res_df) %>%
              group_by(year_max,Stream) %>%
              summarise(
              prop_ext =  sum(extinct)/100, 
              prop_ext_50 = sum(quasi_ext_50)/100,
              prop_ext_100 = sum(quasi_ext_100)/100,
              prop_ext_200 = sum(quasi_ext_200)/100)) # %>%


obs_pop = filter(density.uppidri.vol.df, Age == 1 & Season == "Autumn") %>%
  group_by(Pop) %>%
  summarise(n = n(),
            mean_dens = mean(Dest),
            min_dens = min(Dest),
            max_dens = max(Dest),
            cv = sd(Dest)/mean(Dest))





