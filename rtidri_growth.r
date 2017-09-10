library(tidyverse)
library(nlstools)

rtidri_df = read_csv("rtidri_df_pieced.csv")
pop.growth.prep = arrange(rtidri_df,Mark_cor,Year,Month)

# 
# ### Order cohort levels chronologically  
# Cohort.levels = c("C96","C97","C98","C99","C00","C01","C02","C03","C04","C05","C06","C07",
#                   "C08","C09","C10","C11","C12","C13")



########## September

max_year_v = c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)

rtidri_growth_list = list()

rm(rtidri_growth_nls_df)
rtidri_growth_nls_df = tibble(linf = rep(0,length(max_year_v)),
                       linf_lcl = rep(0,length(max_year_v)), 
                       linf_ucl = rep(0,length(max_year_v)),
                       k = rep(0,length(max_year_v)),
                       k_lcl = rep(0,length(max_year_v)),
                       k_ucl = rep(0,length(max_year_v)),
                       t0 = rep(0,length(max_year_v)),
                       t0_lcl = rep(0,length(max_year_v)),
                       t0_ucl = rep(0,length(max_year_v)),
                       Samples = rep(0,length(max_year_v)),
                       n_data = rep(0,length(max_year_v)),
                       expL1 = rep(0,length(max_year_v)),
                       expL2 = rep(0,length(max_year_v)),
                       expL3 = rep(0,length(max_year_v)),
                       final_year = rep(0, length(max_year_v)),
                       tot_year = rep(0, length(max_year_v)),
                       type = rep("nls",length(max_year_v)))



for (j_n in 1:length(max_year_v)) {

if ("synth.list.3" %in% ls()) rm(synth.list.3)
rm(data_growth)  

data_growth = pop.growth.prep %>%
  filter(.,!is.na(Mark_cor), Month == 9 , Year <= max_year_v[j_n], Age_cor >=1) %>%
  select(.,-Mark, -Cohort, -Age, -Date)

# data_growth = rtidri.growth.prep %>% 
#    filter(.,!is.na(Mark), Month == 9 , Year <= max_year_v[j_n], Age >=1) # %>%
#    #select(.,-Mark, -Cohort, -Age, -Date)



colnames(data_growth)[which(colnames(data_growth) == "Mark_cor")] = "Mark"
colnames(data_growth)[which(colnames(data_growth) == "Age_cor")] = "Age"
colnames(data_growth)[which(colnames(data_growth) == "Cohort_cor")] = "Cohort"
data_growth$Cohort = factor(data_growth$Cohort)


linf_var = "Const"
k_var = "Const"
source("./scripts/m_grow3_gen.r")  #model with no predictors

rtidri_growth_list[[j_n]] = synth.list.3$coh.par %>%
  add_column(final_year =  max_year_v[j_n], tot_year = 1 + (max_year_v[j_n] - min(data_growth$Year)), type = "rand_eff")

nls_fit <- NULL
try(nls_fit <- nls(Length ~ linf*(1-exp(-k*(Age-t0))),data = data_growth,
              start = list(linf = 300, k = 0.5, t0 = -0.3)))

if(length(nls_fit)>0) {
nls_confint = nls_fit %>%
  confint2()

rtidri_growth_nls_df$linf[j_n] = summary(nls_fit)$coefficients[which(rownames(summary(nls_fit)$coefficients) == "linf"),1]
rtidri_growth_nls_df$k[j_n] = summary(nls_fit)$coefficients[which(rownames(summary(nls_fit)$coefficients) == "k"),1]
rtidri_growth_nls_df$t0[j_n] = summary(nls_fit)$coefficients[which(rownames(summary(nls_fit)$coefficients) == "t0"),1]

rtidri_growth_nls_df$linf_lcl[j_n] = nls_confint[which(rownames(nls_confint) == "linf"),1]
rtidri_growth_nls_df$linf_ucl[j_n] = nls_confint[which(rownames(nls_confint) == "linf"),2]
rtidri_growth_nls_df$k_lcl[j_n] = nls_confint[which(rownames(nls_confint) == "k"),1]
rtidri_growth_nls_df$k_ucl[j_n] = nls_confint[which(rownames(nls_confint) == "k"),2]
rtidri_growth_nls_df$t0_lcl[j_n] = nls_confint[which(rownames(nls_confint) == "t0"),1]
rtidri_growth_nls_df$t0_ucl[j_n] = nls_confint[which(rownames(nls_confint) == "t0"),2]
rtidri_growth_nls_df$Samples[j_n] = length(unique(data_growth$Mark))
rtidri_growth_nls_df$n_data[j_n] = nrow(data_growth)
rtidri_growth_nls_df$expL1[j_n] = predict(nls_fit, newdata = data.frame("Age" = 1))
rtidri_growth_nls_df$expL2[j_n] = predict(nls_fit, newdata = data.frame("Age" = 2))
rtidri_growth_nls_df$expL3[j_n] = predict(nls_fit, newdata = data.frame("Age" = 3))
rtidri_growth_nls_df$final_year[j_n] = max_year_v[j_n]
rtidri_growth_nls_df$tot_year[j_n] = 1 + (max_year_v[j_n] - min(data_growth$Year))
}


}

rtidri_growth_df = as_tibble(bind_rows(rtidri_growth_list))

rtidri_growth_all_df = bind_rows(rtidri_growth_nls_df,rtidri_growth_df)


