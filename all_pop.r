library(tidyverse)
library(data.table)
library(parallel)
library(brms)

loidri_df =  fread("https://raw.githubusercontent.com/simonevincenzi/Heter/master/raw_data/loidri_df_pieced.csv") 
loidri_df$Date = as.Date(loidri_df$Date,format = "%m/%d/%Y") # Y is year with century
loidri_df = loidri_df %>%
  arrange(.,Mark_cor,Date) %>% 
  filter(., !is.na(Mark_cor))

loidri_mark_map = loidri_df %>% distinct(Mark_cor)

loidri_mark_map$Mark_ind = seq(1000,(1000 + nrow(loidri_mark_map) - 1),1)

loidri_mark_map$Pop = "LIdri_MT"

loidri_df = loidri_df %>% left_join(., loidri_mark_map)

loidri_df = dplyr::select(loidri_df, Species, Mark_ind, Date, Year, Month, Run, Sector, Length, Weight, Sex, Cohort_cor, Age_cor, Stream, Pop, Pop_ind) %>% 
  rename(., Cohort = Cohort_cor,
            Age = Age_cor,
         Mark = Mark_ind)


uppidri_df =  fread("https://raw.githubusercontent.com/simonevincenzi/Heter/master/raw_data/uppidri_df_pieced.csv")


uppidri_df$Date = as.Date(uppidri_df$Date,format = "%m/%d/%Y") # Y is year with century
uppidri_df = uppidri_df %>%
  arrange(.,Mark_cor,Date) %>% 
  filter(., !is.na(Mark_cor))

uppidri_mark_map = uppidri_df %>% distinct(Mark_cor)

uppidri_mark_map$Mark_ind = seq(3000,(3000 + nrow(uppidri_mark_map) - 1),1)

uppidri_mark_map$Pop = "UIdri_MT"

uppidri_df = uppidri_df %>% left_join(., uppidri_mark_map)


uppidri_df = dplyr::select(uppidri_df, Species, Mark_ind, Date, Year, Month, Run, Sector, Length, Weight, Sex, Cohort_cor, Age_cor, Stream, Pop, Pop_ind) %>% 
  rename(., Cohort = Cohort_cor,
            Age = Age_cor,
         Mark = Mark_ind)

rtidri_df = read_csv("rtidri_df_pieced.csv")
rtidri_df$Date = as.Date(rtidri_df$Date,format = "%m/%d/%Y")

rtidri_df = rtidri_df %>% 
  arrange(.,Mark_cor,Date) %>% 
  filter(., !is.na(Mark_cor))

rtidri_mark_map = rtidri_df %>% distinct(Mark_cor)

rtidri_mark_map$Mark_ind = seq(5000,(5000 + nrow(rtidri_mark_map) - 1),1)

rtidri_mark_map$Pop =  "LIdri_RT"

rtidri_df = rtidri_df %>% left_join(., rtidri_mark_map)

rtidri_df = dplyr::select(rtidri_df, Species, Mark_ind, Date, Year, Month, Run, Sector, Length, Weight, Sex, Cohort_cor, Age_cor, Stream, Pop, Pop_ind) %>% 
  rename(., 
         Cohort = Cohort_cor,
         Age = Age_cor,
         Mark = Mark_ind)

uppvol_df = read.csv("uppvol_2015_complete.csv",header=T, # read dataset of the population from csv
                     stringsAsFactors = FALSE,na.strings ="") 

uppvol_df$Date = as.Date(uppvol_df$Date,format = "%m/%d/%y")

uppvol_df = uppvol_df %>% 
  mutate(., Mark_cor = Mark) %>% 
  arrange(.,Mark_cor,Date) %>% 
  filter(., !is.na(Mark_cor))
 

uppvol_df$Pop = "UVol_BT"
uppvol_df$Stream = "UVol"
uppvol_df$Species = "BT"
uppvol_df$Pop_ind = 4

uppvol_mark_map = uppvol_df %>% distinct(Mark_cor)

uppvol_mark_map$Mark_ind = seq(8000,(8000 + nrow(uppvol_mark_map) - 1),1)

uppvol_mark_map$Pop =  "UVol_BT"

uppvol_df = uppvol_df %>% left_join(., uppvol_mark_map)

uppvol_df = dplyr::select(uppvol_df, Species, Mark_ind, Date, Year, Month, Run, Sector, Length, Weight, Age,Sex, Stream, Pop, Pop_ind) %>% 
  rename(.,
         Mark = Mark_ind)


# all_pop_df = bind_rows(loidri_df,
#                        uppidri_df,
#                        rtidri_df,
#                        uppvol_df)

all_pop_df = bind_rows(loidri_df,
                       
                       
                       uppvol_df)


data_region_list = list()
cont = 1
data_region_list[[cont]] = as.list(data.frame(data_region_df = NA, age_cut = NA, cont = NA, data_compl = NA))
data_region_list[[cont]]$data_region_df =  as.data.frame(all_pop_df)
#data_region_list[[cont]]$score_28_df =  as.data.frame(score_28_df)
data_region_list[[cont]]$age_cut =  0
data_region_list[[cont]]$cont =  cont
data_region_list[[cont]]$data_compl =  "full"

model = "Species + Pop + Cohort"

data_region_list[[cont]]$rand_eff_n = 3
data_region_list[[cont]]$linf_var = model
data_region_list[[cont]]$k_var = model
data_region_list[[cont]]$t0_var = model
data_region_list[[cont]]$mod_id = 1

source("vB_TMB_parall_validation_choice_rand_choice_cov.r")


# data_region_df = data_region_list[[1]]$data_region_df
# age_cut = data_region_list[[1]]$age_cut
# cont = data_region_list[[1]]$cont
# data_compl = data_region_list[[1]]$data_compl
# rand_eff_n = data_region_list[[1]]$rand_eff_n
# linf_var = data_region_list[[1]]$linf_var
# k_var = data_region_list[[1]]$k_var
# t0_var = data_region_list[[1]]$t0_var
# mod_id = data_region_list[[1]]$mod_id

#test = mclapply(1,function (x) do.call(vB_TMB_parall_validation_choice_rand_choice_cov.f,data_region_list[[x]]),
#                mc.cores = 4, mc.preschedule = F)
                
 
# fit_loss <- brm(
#   bf(Length ~ Linf*(1-exp(-k*(Age-t0))),
#      Linf ~ 1 + Species + (1|Mark),
#      k ~ 1 + Species + (1|Mark), 
#      t0 ~ 1 + Species + (1|Mark), 
#      nl = TRUE),
#   data = all_pop_df, family = gaussian(),
#   prior = c(
#     prior(normal(300, 200), nlpar = "Linf"),
#     prior(normal(0.5, 2), nlpar = "k"),
#     prior(normal(0, 10), nlpar = "t0")
#   ),
#   sample_prior = "only",
#   chains=1,iter=100
# )                
#                
                

all_pop_df$Length = all_pop_df$Length/100 


fit_loss <- brm(
  bf(Length ~ Linf*(1-exp(-k*(Age-t0))),
     Linf ~ 1 + Species + (1|Mark),
     k ~ 1 + Species + (1|Mark), 
     t0 ~ 1 + Species + (1|Mark), 
     nl = TRUE),
  data = all_pop_df, family = gaussian(),
  prior = c(
    prior(normal(3, 2), nlpar = "Linf"),
    prior(normal(0.5, 2), nlpar = "k"),
    prior(normal(0, 5), nlpar = "t0")
  ),
  control = list(adapt_delta = 0.9)
)                