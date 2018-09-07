
# check if required pacakges are present, otherwise install them
list.of.packages <- c("Rlab", "MASS", "parallel", "tidyverse","MCMCpack","boot","logitnorm","mgcv","rlist")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(Rlab)
library(MASS)
library(parallel)
library(tidyverse)
library(MCMCpack)
library(boot)
library(logitnorm)
library(mgcv)
library(splines)
library(rlist)

################################


# uppidri_surv_06_14.list = readRDS("uppidri_surv_06_14.list.RDS")
# loidri_surv_06_14.list = readRDS("loidri_surv_06_14.list.RDS")
# rtidri_surv_06_14.list = readRDS("rtidri_surv_06_14.list.RDS")
# uppvol_surv_06_14.list = readRDS("uppvol_surv_06_14.list.RDS")


stock.r.mod = readRDS("data/stock.gam.RDS")
stock.uppvol = readRDS("data/uppvol_recruit.RDS")
uppvol.first.year = readRDS("data/uppvol.first.year.RDS")
rtidri.first.year = readRDS("data/rtidri.first.year.RDS")


demo_limit_sim.f <- function (S = 1000,N = 300,iter = 30, Stream = "UIdri_MT", mort.list = uppidri_surv_06_14.list, year_max = 2008)
  
  
{
  
  extinct = 0
  options(error=recover)
  
  options(warn = 0) ### stop when there is warning when warn = 2
  
  ## create the data frame of sampling-specific survival probabilities from the list of survival models 
  pos_year = list.which(mort.list, mod_cost$final_year == year_max)
  mort.df = predict(mort.list[[pos_year]]$mod[[mort.list[[pos_year]]$p_b_time]],ddl = mort.list[[pos_year]]$ddl, se = T)$Phi

## paramters of logit distribution from confidence intervals  
  mort.df$mu = 0
  mort.df$sigma = 0
  
  for (x in 1:nrow(mort.df)) {
    theta <- twCoefLogitnormMLE(mort.df$estimate[x], mort.df$ucl[x])
    mort.df$mu[x] =  theta[,1]
    mort.df$sigma[x] =  theta[,2]
  }
  

  ############ Define values of some model parameters not included in the function
  
  
  ## the population stays on a matrix (area.pop), each column is an individual. Here below I define
  # the position of entities in the column
  
  pheno.pos = 1  # this means that the first row is the phenotype of the individuals
  age.pos = 2  # second row is the age of the individual

  length_prop = 2 #  length of entities of individual vector excluding loci (phenotipic value, age, maturity status, and size are the top rows)
  
  area.pop <- matrix(0,length_prop,S) ##  matrix space area.pop of S elements, 
  
  init_pos = sample(1:ncol(area.pop), size = N, replace = F)
  
  initial_ages = sample(x = 0:5, size = N,replace = T)
  
  area.pop[age.pos,init_pos] = initial_ages # assign ages to individuals
  area.pop[pheno.pos,init_pos] = 1
  
  # multiplication factor for density stream (passing from numbers to densities in number per hectar)
  
  if(Stream == "LIdri_MT" | Stream == "LIdri_RT") {
    mult_fact_stream = 6.61
    } 
  if (Stream == "UIdri_MT") {
      mult_fact_stream = 6.01 
      } 
  if (Stream == "UVol_BT") {
        mult_fact_stream = 13.4
      } 
  if (!Stream %in% c("LIdri_MT","LIdri_RT","UIdri_MT","UVol_BT")) {
        print("No suitable stream. Using LIdri_MT as default")  
        mult_fact_stream = 6.61}
  
 ##### 
  
  
  #####create vectors and matrix for later use

  mean.age = rep(0,iter) # vector with mean age at each time step
  recruitment = rep(0,iter) # vector with escapement for each year
  dens = rep(0,iter) # vector of escapement (observed)
  dens[1] = N * mult_fact_stream
  
  
  vettad = rep(0,iter)    #######population size post_mortality at each time step
  mean.var.age = matrix(0,iter,2)  ## matrix with iter row and 2 cols (1 = mean age, 2 = sd of age every year)
 
  ##################### START FOR TIME LOOP ########################
  
  
  for(i in 1:iter)  { ##time-steps or cohorts or year
    
    print(i)  # prints time-steps
    
    
    ### check if the population went extinct - if it is, break ####
    
    if(length(area.pop[1,area.pop[1,]!=0])<2){  ##is population extinct?  
      print(paste("Extinct at year", i)) #prints the year of extinction
      extinct = 1 # when an individual is dead the first column is 0
      break}   #close for to check for extinction
    
    ######################################  
    
  
    
    ##### compute mean age , only 2 years old and older ######
    
    mean.age.who = which(area.pop[age.pos,]>=2 & area.pop[1,]!=0)
    
    mean.age[i] = mean(area.pop[2,mean.age.who],na.rm = T)
    
    
    ############ increase age by 1 if t>1 ##########
    
    if (i>1) {
      area.pop[age.pos,area.pop[pheno.pos,]!=0] = 
        area.pop[age.pos,area.pop[pheno.pos,]!=0] + 1 
     }
    
    ###########
    
    
    ############### MEAN AND SD OF AGE in the whole population #####
    
    who = which(area.pop[pheno.pos,]!=0)  # where the individuals are (because empty columns have zero in first position)
    mean.var.age[i,1] = mean(area.pop[age.pos,who]) #mean age
    mean.var.age[i,2] = sd(area.pop[age.pos,who]) #sd of age
    
    
    
    ############ mortality ##############
    
    
    who_tag = as.matrix(which(area.pop[pheno.pos,]!=0 & area.pop[age.pos,]>=2)) # where the individuals are (because empty columns have zero in first position) 
    
    ### random number for each adult for bernoulli trials, then random draw of survival probabiliies
    
    if (length(who_tag) > 0) {
      who_tag = as.tibble(cbind(who_tag,runif(nrow(who_tag))))
      mort.year.df = mort.df[sample(1:nrow(mort.df), size = 1),]
      r =  1 - rlogitnorm(n = 1, mu = mort.year.df$mu, sigma = mort.year.df$sigma)
      who_tag <- who_tag[which(who_tag[,2] < r),]
      if (length(who_tag) > 0) {
        
        area.pop[,who_tag$V1] = 0
  
      }
    }
    
    rm(who_tag)
    
  # survival from 0+ to 1+  
  
    who_tag = as.matrix(which(area.pop[pheno.pos,]!=0 & area.pop[age.pos,]==1)) # where the individuals are (because empty columns have zero in first position) 
    if (length(who_tag) > 0) {
      who_tag = as.tibble(cbind(who_tag,runif(nrow(who_tag))))
      if (Stream %in% c("LIdri_MT", "UIdri_MT")) {
      r =  1 - exp(3.2341 - 0.7426 * log(dens[max((i-1),1)] + 0.1))  # 1- survival = mortality (hard coded the log-linear model)
      
      } else if (Stream == "UVol_BT") {
      r = 1 - sample(uppvol.first.year, size = 1) # 1- survival = mortality
      } else if (Stream == "LIdri_RT") {
      r = 1 - sample(rtidri.first.year, size = 1) # 1- survival = mortality
      } else if (!Stream %in% c("LIdri_MT", "UIdri_MT", "UVol_BT","LIdri_RT")) {
        stop("Choose a stream among LIdri_MT, UIdri_MT, UVol_BT, LIdri_RT")}
      who_tag <- who_tag[which(who_tag[,2] < r),]
      if (length(who_tag) > 0) {
        
        area.pop[,who_tag$V1] <- 0
      
      }
    }
    rm(who_tag)
    
    # I double of average the number of 1+ to include in UVol fish coming from upstrea
    
    if (Stream == "UVol_BT") {
    
      
      coh_size = max(nrow(as.matrix(which(area.pop[pheno.pos,]!=0 & area.pop[age.pos,]==1))),100)
      mult_size = runif(n = 1,min = 0.85, max = 1.5)
      empty = which(area.pop[pheno.pos,]==0)
        area.pop[pheno.pos, empty[1:round(coh_size * mult_size)]] = 1
        area.pop[age.pos, empty[1:round(coh_size * mult_size)]] = 1
    }
    ####
    
    vettad[i] = length(area.pop[pheno.pos,(area.pop[pheno.pos,]!=0 & area.pop[age.pos,]>=1)]) # 
    dens[i] = vettad[i] * mult_fact_stream
    
    
    
    ########## close mortality ##############
    
    
    ####################   REPRODUCTION ##########################
    
    ### 
    if (Stream %in% c("LIdri_MT", "UIdri_MT","LIdri_RT")) {
    pot_repro = length(area.pop[pheno.pos,(area.pop[pheno.pos,]!=0 & area.pop[age.pos,]>=2)])
    } else {
    pot_repro = length(area.pop[pheno.pos,(area.pop[pheno.pos,]!=0 & area.pop[age.pos,]>=1)])
    }
    
    stock = round(pot_repro) # stock
    if (Stream %in% c("LIdri_MT", "UIdri_MT", "LIdri_RT")) {
    recruit = round(predict(stock.r.mod, newdata = data.frame(Stream = "LIdri", Density.sp = stock * mult_fact_stream))/mult_fact_stream) # recruitment
    } else if (Stream == "UVol_BT") {
    recruit = (sample(x = stock.uppvol, size = 1)*3.3)/mult_fact_stream
    }
    
   
    if (recruit >1 )   { #open if for reproduction, that is there is at least one recruit
      
      empty = which(area.pop[pheno.pos,]==0)  ### identify the available spots for newborns in the matrix. If there are more recruits than empty spots. some recruits die (the number of columns in the matrix is basically the carrying capacity of the system)
      if (length(empty) > recruit) {
        area.pop[pheno.pos, empty[1:recruit]] = 1
        area.pop[age.pos, empty[1:recruit]] = -1
      } else {
        area.pop[pheno.pos, empty] = 1
        area.pop[age.pos, empty] = -1
      }
    }
    
    
    ####################   CLOSE REPRODUCTION ##########################
    
    
   
    year = i  # counter for plot, I cannot use iter because year < iter if the population went extinct   
    
    ##################### CLOSE FOR TIME LOOP ########################
    
  }  
  
  
  
  ### PLOT
  
  # define parameters 
  
  size.title = 15
  line.lwd = 1.2
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
  max_size_dot = 8
  
  ## define theme to be used 
  
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
                     axis.title.y = element_text(size=size.label.x, vjust = 2),
                     axis.text.y  = element_text(size=size.text.x),
                     legend.title = element_blank(),
                     legend.text = element_text(size = size.legend.text),
                     legend.position = c(0.15, 0.95),
                     legend.key = element_rect(fill = "white")) 
  
  ## create the data.frame for ggplot
  
  pop_df = data.frame(abund = c(vettad[1:year],dens[1:year]), 
                      type = rep(c("numbers","density"),each = year), year = 1:year) %>%
    filter(.,year>=10)
  
  
  if (year > 10) {
    pop_gg = ggplot(dplyr::filter(pop_df, type %in% c("numbers","density")), aes(x = year, y = abund, group = type, shape = type, linetype = type)) +
      geom_point(alpha = 0.4, size = size.point) +
      geom_line() +
      theme.pop +
      guides(size = guide_legend(override.aes = list(alpha = 0.2))) +
      scale_y_continuous(limits = c(0,(max(pop_df$abund) + max(pop_df$abund)*0.1))) +
      scale_x_continuous(limits = c(0,year+2)) +
      labs(y = "#") +
      labs(x = "Year") 
  
  } else {
    pop_gg = "Not enough simulation years for plot (must be > 10)"}
  
  
  
  
  ### For the extinct populations, I want only the vectors up to year of exinction ####
  
  dens = dens[1:year]
  vettad = vettad[1:year]
  if (year > 10) {
    if (min(dens[10:year]) < 100 ) {
      quasi_ext_100 = 1
      quasi_ext_100_cont = sum(dens[10:110]<100)
    } else {
        quasi_ext_100 = 0
        quasi_ext_100_cont = 0}
  } else {quasi_ext = NA}
  
  if (year > 10) {
    if (min(dens[10:year]) < 50 ) {
      quasi_ext_50 = 1
      quasi_ext_50_cont = sum(dens[10:110]<50)
    } else {
      quasi_ext_50 = 0
      quasi_ext_50_cont = 0}
  } else {quasi_ext_50 = NA}
  
  if (year > 10) {
    if (min(dens[10:year]) < 200 ) {
      quasi_ext_200 = 1
      quasi_ext_200_cont = sum(dens[10:110]<200)
    } else {
      quasi_ext_200 = 0
      quasi_ext_200_cont = 0}
  } else {quasi_ext_200 = NA}
  
  
  if (year > 10) {
    dens_cv = sd(dens[10:year])/mean(dens[10:year])} else {dens_cv = NA}
  if (year > 10) {
    dens_mean = mean(dens[10:year])
    dens_max = max(dens[10:year])
    dens_min = min(dens[10:year])} else {
      dens_mean = NA
      dens_max = NA
      dens_min = NA}
  
 # list of results, uncomment "plot_pop" = pop_gg only for single simulations - the object created is otherwise too big
  
   ris.list = list("extinct"=extinct, 
                  "yearextinct" = i-1,
                  "popsize"= vettad, 
                  "dens" = dens, 
                  #"plot_pop" = pop_gg,
                  #"pop" = pop_df,
                  "Stream" = Stream,
                  "year_max" = year_max,
                  "quasi_ext_100" = quasi_ext_100,
                  "quasi_ext_100_cont" = quasi_ext_100_cont,
                  "quasi_ext_200" = quasi_ext_200,
                  "quasi_ext_200_cont" = quasi_ext_200_cont,
                  "quasi_ext_50" = quasi_ext_50,
                  "quasi_ext_50_cont" = quasi_ext_50_cont,
                  "dens_cv" = dens_cv,
                  "dens_mean" = dens_mean,
                  "dens_max" = dens_max,
                  "dens_min" = dens_min
  )
  
  return(ris.list)
  
  
  
}   # close all

