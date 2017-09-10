
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


uppidri_surv_06_14.list = readRDS("uppidri_surv_06_14.list.RDS")
loidri_surv_06_14.list = readRDS("loidri_surv_06_14.list.RDS")


# uppidri_surv_08_14.list = readRDS("uppidri_surv_08_14.list.RDS")
# loidri_surv_08_14.list = readRDS("loidri_surv_08_14.list.RDS")
# 
# 
# 
# uppidri_surv_08.df = predict(uppidri_surv_08_14.list[[1]]$mod[[uppidri_surv_08_14.list[[1]]$p_b_time]],ddl = uppidri_surv_08_14.list[[1]]$ddl, se = T)$Phi
# uppidri_surv_14.df = predict(uppidri_surv_08_14.list[[2]]$mod[[uppidri_surv_08_14.list[[2]]$p_b_time]],ddl = uppidri_surv_08_14.list[[2]]$ddl, se = T)$Phi
# 
# loidri_surv_08.df = predict(loidri_surv_08_14.list[[1]]$mod[[loidri_surv_08_14.list[[1]]$p_b_time]],ddl = loidri_surv_08_14.list[[1]]$ddl, se = T)$Phi
# loidri_surv_14.df = predict(loidri_surv_08_14.list[[2]]$mod[[loidri_surv_08_14.list[[2]]$p_b_time]],ddl = loidri_surv_08_14.list[[2]]$ddl, se = T)$Phi
# 
# uppidri_surv_08.df$mu = 0
# uppidri_surv_08.df$sigma = 0
# 
# for (x in 1:nrow(uppidri_surv_08.df)) {
#   theta <- twCoefLogitnormMLE(uppidri_surv_08.df$estimate[x], uppidri_surv_08.df$ucl[x])
#   uppidri_surv_08.df$mu[x] =  theta[,1]
#   uppidri_surv_08.df$sigma[x] =  theta[,2]
# }
# 
# uppidri_surv_14.df$mu = 0
# uppidri_surv_14.df$sigma = 0
# 
# for (x in 1:nrow(uppidri_surv_14.df)) {
#   theta <- twCoefLogitnormMLE(uppidri_surv_14.df$estimate[x], uppidri_surv_14.df$ucl[x])
#   uppidri_surv_14.df$mu[x] =  theta[,1]
#   uppidri_surv_14.df$sigma[x] =  theta[,2]
# }
# 
# 
# loidri_surv_08.df$mu = 0
# loidri_surv_08.df$sigma = 0
# 
# for (x in 1:nrow(loidri_surv_08.df)) {
#   theta <- twCoefLogitnormMLE(loidri_surv_08.df$estimate[x], loidri_surv_08.df$ucl[x])
#   loidri_surv_08.df$mu[x] =  theta[,1]
#   loidri_surv_08.df$sigma[x] =  theta[,2]
# }
# 
# loidri_surv_14.df$mu = 0
# loidri_surv_14.df$sigma = 0
# 
# for (x in 1:nrow(loidri_surv_14.df)) {
#   theta <- twCoefLogitnormMLE(loidri_surv_14.df$estimate[x], loidri_surv_14.df$ucl[x])
#   loidri_surv_14.df$mu[x] =  theta[,1]
#   loidri_surv_14.df$sigma[x] =  theta[,2]
# }


stock.r.mod = readRDS("stock.gam.RDS")


pp_sim.f <- function (S = 1000,N = 300,iter = 30,num.loci = 5, num.alleles = 2,sd.alleles = 0.1,recomb = 1, L_inf = 3750, k_vb = 0.057, t0_vb = -0.21, alpha_w = 1.44, beta_w = 3.12, sd_e = 0.2, Stream = "LIdri_MT", var.env = 1, mort.list = uppidri_surv_06_14.list, year_max = 2008)
  
  # S = maximum (and initial number of individuals in the population)
  # N = not used
  # iter = total number of simulation steps/year
  # num.loci = number of genes
  # plotorno = 1 if plot of phenotype/optimum/population
  # num.alleles = num alleles for each locus (randomly drawn at the start of the simulation)
# sd.alleles = standard deviation of the allelic effects (not used, as it is defined in the function)
# recomb = 1 for full recombination, 0 for no recombination


{
  
  extinct = 0
  options(error=recover)
  
  options(warn = 0) ### stop when there is warning when warn = 2
  
  ### I define the standard deviation of distribution of single alleles, which depends on heritability at time 1 and environmental variance this can be adapted to other traits that have means != 0 the correct way is to proceed from a phenotypic variance and heritability and then obtain the starting additive genetic variance
  
  heritability = 0.3
  mat_prop = rep(0.9,7)
  age_repr = 3:9
  stock.r.mod = readRDS("stock.gam.RDS")
  
  pos_year = list.which(mort.list, mod_cost$final_year == 2014)
  mort.df = predict(mort.list[[pos_year]]$mod[[mort.list[[pos_year]]$p_b_time]],ddl = mort.list[[pos_year]]$ddl, se = T)$Phi
  ### check if some year estimates are very imprecise and delete those
  test = sapply(1:nrow(mort.df), function (x) (ifelse((mort.df[x,"ucl"]-mort.df[x,"lcl"])>0.9 | (mort.df[x,"ucl"]-mort.df[x,"lcl"])<0.08,0,1)))
  mort.df = mort.df[which(test==1),]
  ###
 
  mort.df$mu = 0
  mort.df$sigma = 0
  for (x in 1:nrow(mort.df)) {
    theta <- twCoefLogitnormMLE(mort.df$estimate[x], mort.df$ucl[x])
    mort.df$mu[x] =  theta[,1]
    mort.df$sigma[x] =  theta[,2]
  }
  
  
  starting_genetic_variance = (heritability * var.env)/(1-heritability)
  
  # sd.alleles is the standard deviation of the normal distribution of allelic effects 
  
  sd.alleles = sqrt(starting_genetic_variance/(2*num.loci))
  
  ############ Define values of some model parameters not included in the function
  
  
  ## the population stays on a matrix (area.pop), each column is an individual. Here below I define
  # the position of entities in the column
  
  pheno.pos = 1  # this means that the first row is the phenotype of the individuals
  age.pos = 2  # second row is the age of the individual
  matur.pos = 3 # third row is maturity status
  linf.pos = 4
  k.pos = 5
  t0.pos = 6
  size.pos = 7 # length of fish
  weight.pos = 8 
  
  length_prop = 8 #  length of entities of individual vector excluding loci (phenotipic value, age, maturity status, and size are the top rows)
  
  
  
  heteroz.mat = matrix(0,num.loci*num.alleles,(iter%/%50))  ##### define the matrix for heterozigosyis, same as all.freq100 but with a col less
  
  area.pop <- matrix(0,length_prop+(num.loci*2),S) ##  matrix space area.pop of S elements, 
  # for each col the first row is the pheno value, second is age, third is maturity status 
  #the other num.loci*2 rows are the values for each gene
  #the first num.loci (2-num.loci+1)for the first chromosome, the second num.loci (num.loci+2-(2*num.loci)+2) for the second chromosome.
  
  fchrom = seq((length_prop+1),num.loci+length_prop,1) ##identifier of rows for the 1st chromosome
  schrom = seq(num.loci+(length_prop+1),num.loci*2+(length_prop),1) ##identifier of rows for the 2nd chromosome
  
  
  ## define initial ages for the S individuals in the population at time = 1
  
  # initial_ages = round(runif(S,min = 0, max = 5))
  
  init_pos = sample(1:ncol(area.pop), size = N, replace = F)
  
  initial_ages = sample(x = 0:5, size = N,replace = T)
  
  area.pop[age.pos,init_pos] = initial_ages # assign ages to individuals
  
  area.pop[linf.pos,init_pos] = rnorm(n = N, mean = L_inf, sd = L_inf/3)
  area.pop[k.pos,init_pos] = rnorm(n = N, mean = k_vb, sd = k_vb/3)
  area.pop[t0.pos,init_pos] = rnorm(n = N, mean = t0_vb, sd = -t0_vb/3)


  # multiplication factor for density stream
  
  if(Stream == "LIdri_MT" | Stream == "LIdri_RT") {
    mult_fact_stream = 6.61} else if (Stream == "UIdri_MT") {
      mult_fact_stream = 6.01 } else if (Stream == "UVol_BT") {
        mult_fact_stream = 13.4} else {mult_fact_stream = 6.61}
    
  
  
  
  #####CREATE THE MATRIX OF ALLELES IN THE POPULATION##########
  
  alleles.mat = matrix(rnorm(num.loci*num.alleles,0,sd.alleles), 
                       num.loci,num.alleles)  
  
  #in matrix alleles.mat I extract randomly, for each of the num.loci, the num.alleles number of alleles 
  
  alleles.mat.keep = alleles.mat  
  
  alleles.mat.gen = matrix(0, num.loci,num.alleles)
  
  for (nl in 1:num.loci) {
    
    alleles.mat.gen[nl,] = seq(1:num.alleles)
    
  } ###I assign a number from 1 to num.alleles to each allele (for Fst since I need the genotype)
  

  #double the matrix alleles.mat for ease of extraction of alleles for the diploid organism 
  
  doublealleles.mat = rbind(alleles.mat,alleles.mat)
  
  ##########assign alleles to individuals at the start of simulation
  
  for (a in 1:(num.loci*2)) { #assign alleles to loci in the population matrix area.pop
    
    area.pop[length_prop+a,init_pos] = sample(doublealleles.mat[a,],N,replace=T) } #end assignment 
  
  area.pop[pheno.pos,init_pos] = colSums(area.pop[(length_prop+1):nrow(area.pop),init_pos]) #the first row of the population matrix is the sum of the genetic values of each alleles 
  #of an individual
  ############################################ 
  pheno = rep(0,S)
  pheno[init_pos] = area.pop[pheno.pos,init_pos] + rnorm(N,0,sqrt(var.env)) 
  # phenotypic value = genotypic value + environmental deviate from N(0,sqrt(var.env)) (all stored in pheno)
  
  ### assign length and weight at age based on von Bertalanffy's growth parameters ####
  
  area.pop[size.pos,which(area.pop[pheno.pos,]!=0)] = round(sapply(which(area.pop[pheno.pos,]!=0), function(x) {
    area.pop[size.pos,x] = area.pop[linf.pos,x] * (1 - exp(-area.pop[k.pos,x]*(area.pop[age.pos,x]-area.pop[t0.pos,x])))
  }))
  
  area.pop[weight.pos,which(area.pop[pheno.pos,]!=0)] = sapply(which(area.pop[pheno.pos,]!=0), function(x) {
    area.pop[weight.pos,x] = alpha_w * 10^(-8) * area.pop[size.pos,x]^(beta_w)
  })
  
  
  #####create vectors and matrix for later use
  mediaphen = rep(0,iter) # vector with mean of the phenotype (one value each year)
  sdphen = rep(0,iter)  # vector with standard deviation of the phenotype (one value each year)
  mean.age = rep(0,iter) # vector with mean age at each time step
  recruitment = rep(0,iter) # vector with escapement for each year
  dens = rep(0,iter) # vector of escapement (observed)
  dens[1] = N * mult_fact_stream
  
  
  vettad = rep(0,iter)    #######population size post_mortality at each time step
  vettad_pre = rep(0,iter)  ###### population size pre_mortality at each time step
  addgenvar = rep(0,iter)   ####additive genetic variance of the whole population at each time step
  addgenmean = rep(0,iter)  ### mean genetic value in the whole population at each time step
  phenomean = rep(0,iter)  #### mean of the phenotype pre_selection, including the newborns of the year i-1
  mean.var.age = matrix(0,iter,2)  ## matrix with iter row and 2 cols (1 = mean age, 2 = sd of age every year)
  varphen = rep(0,iter)
  
  w_gen_mean = rep(0,iter)  ###### mean of the phenotype in a new generation at each time step
  w_gen_var = rep(0,iter)		###### sampling variance of the phenotype in a new generation at each time step
  
  num.sons.check = matrix(0,2,iter) #check the number of offspring recruited in the population
  
  test.pop = list() # list of area matrix for testing
  
  
  ###########
  
  all.freq100 = matrix(0,num.loci*num.alleles,(iter%/%50)+3) ## initialize matrix with 
  #frequencies of alleles during simulation, every 50 years I record the value
  dim(alleles.mat) = c(num.loci*num.alleles,1) ##unravel matrix with alleles (alleles.mat) in order to have a vector
  
  heter_mat_year = matrix(0,num.loci*num.alleles,iter) # saves allelic frequency each year
  heter_vect_mean_year = rep(0,iter) #saves mean heterozygosity each year
  heter_vect_sd_year = rep(0,iter) #saves sd of heterozygosity each year
  
  col.freq = 2 # counter for column number when checking for frequencies of alleles at different time steps  
  
  a = 0 ## counter for the creation of matrix of allelic values, starts from 0
  
  ##################### START FOR TIME LOOP ########################
  
  
  for(i in 1:iter)  { ##time-steps or cohorts or year
    
    print(i)  # prints time-steps
    
    ###### Compute mean and variance of the phenotype for the first generation
    
    if (i==1) {
      w_gen_mean[i] = mean(pheno)
      w_gen_var[i]  = var(pheno)
    }
    
    #####################################
    
    #########check if the population went extinct####
    
    
    if(length(area.pop[1,area.pop[1,]!=0])<2){  ##is population extinct?  
      print(paste("Extinct at year", i)) #prints the year of extinction
      extinct = 1 # when an individual is dead the first column is 0
      
      if(iter>=50) {
        
        all.freq100[,dim(all.freq100)[2]] = all.freq100[,col.freq] - all.freq100[,2]  #in case of population going extinct, it fills the 
        #last col of all.freq100 with the difference between 
        heteroz = 0		
      }#frequency at time 50 and frequency at the last year to be divided
      #by 50 before extinction
      break}   #close for to check for extinction
    
    ######################################  
    
    ### here below I create the matrix of allele frequencies every 50 years starting from year 1
    
    if (i==1) { #if first year of the simulation
      
      all.freq100[,1] = alleles.mat  #first column is the vector of alleles
      
      for (zz in (1:(num.loci*num.alleles))) {  #for each alleles in the population (present at time 1)
        
        all.freq100[zz,col.freq] = length(area.pop[area.pop==alleles.mat[zz]])/
          (2*length(area.pop[1,area.pop[1,]!=0])) #divide the number of alleles in the 
        #population by 2 times the number of individuals alive
        # (diploid organism)
      }
      
    }
    
    
    if (i%%50==0) {  #if year is a multiple of 50
      
      col.freq=col.freq+1  #update counter of rows

      for (zz in (1:(num.loci*num.alleles))) {  #for each alleles in the population (present at time 1)
        
        all.freq100[zz,col.freq] = length(area.pop[area.pop==alleles.mat[zz]])/(2*length(area.pop[1,area.pop[1,]!=0])) #divide the number of alleles in the 
        #population by 2 times the number of individuals alive
      }
      
    }
    
    
    
    if (i==iter & iter>=50){ #if last year of the simulation
      
      all.freq100[,dim(all.freq100)[2]] = all.freq100[,(dim(all.freq100)[2]-1)] - all.freq100[,2] #it fills the 
      #last col of all.freq100 with the difference between 
      #allelic frequency at time 50 and frequency at the last year to be divided
      #by 50 before extinction
      
      heteroz.mat = all.freq100[,(dim(all.freq100)[2]-1)]   #matrix for heterozigosity 
      
    }
    
    a = a+1   ## counter for the creation of matrix of allelic values
    
    
    #### allelic frequency each year
    
    for (zz in (1:(num.loci*num.alleles))) {  #for each alleles in the population (present at time 1)
      
      
      heter_mat_year[zz,i] = length(area.pop[area.pop==alleles.mat[zz]])/(2*length(area.pop[1,area.pop[1,]!=0])) #divide the number of alleles in the 
      #population by 2 times the number of individuals alive because it is a diploid organism
    }
    
    
    
    
    ##### compute mean age of ocean salmon, only 2 years old and older ######
    
    mean.age.who = which(area.pop[age.pos,]>=2 & area.pop[1,]!=0)
    
    mean.age[i] = mean(area.pop[2,mean.age.who],na.rm = T)
    
    
    ############ increase age by 1 if t>1 ##########
    
    if (i>1) {
      area.pop[age.pos,area.pop[pheno.pos,]!=0] = area.pop[age.pos,area.pop[pheno.pos,]!=0] + 1 
      
     ########### assign length and weight ###########
      
       area.pop[size.pos,which(area.pop[pheno.pos,]!=0)] = round(sapply(which(area.pop[pheno.pos,]!=0), function(x) {
        area.pop[size.pos,x] = area.pop[linf.pos,x] * (1 - exp(-area.pop[k.pos,x]*(area.pop[age.pos,x]-area.pop[t0.pos,x])))
      }))
      
      area.pop[weight.pos,which(area.pop[pheno.pos,]!=0)] = sapply(which(area.pop[pheno.pos,]!=0), function(x) {
        area.pop[weight.pos,x] = alpha_w * 10^(-8) * area.pop[size.pos,x]^(beta_w)
      })
    }
    ###########
    
    
    ####vector of optimum and mean and sd of phenotype in the population ######
    
    
    mediaphen[i] = mean(area.pop[pheno.pos,area.pop[pheno.pos,]!=0]) # only alive individuals have value at row pheno.pos > 0
    
    sdphen[i] = sd(area.pop[pheno.pos,area.pop[pheno.pos,]!=0])
    
    
    #### close vector of optimum and mean and sd of phenotype in the population ######
  
    
    addgenvar[i] <- var(area.pop[pheno.pos,area.pop[pheno.pos,]!=0]) # variance of phenotypes
    addgenmean[i] <- mean(area.pop[pheno.pos,area.pop[pheno.pos,]!=0]) # mean of phenotypes
    phenomean[i] <- mean(pheno[area.pop[pheno.pos,]!=0])
    varphen[i] = var(pheno[pheno!=0])
    
    
    vettad_pre[i] = length(area.pop[pheno.pos,area.pop[pheno.pos,]!=0]) ######## population size pre_selection
    
    ############### MEAN AND SD OF AGE #####
    
    who = which(area.pop[pheno.pos,]!=0)  # where the individuals are (because empty columns have zero in first position)
    mean.var.age[i,1] = mean(area.pop[age.pos,who]) #mean age
    mean.var.age[i,2] = sd(area.pop[age.pos,who]) #sd of age
    
    
    
    ############mortality##############
    
  
    who_tag = as.matrix(which(area.pop[pheno.pos,]!=0 & area.pop[age.pos,]>=2)) # where the individuals are (because empty columns have zero in first position) 

    vettad_pre[i] = length(area.pop[pheno.pos,(area.pop[pheno.pos,]!=0 & area.pop[age.pos,]>=2)]) ######## population size in the ocean pre_mortality including, only 2 years old and older
    
    ## mortality is sequential, natural and then harvest
    
    ### random number for each adult 
    
    if (length(who_tag) > 0) {
    who_tag = as.tibble(cbind(who_tag,runif(nrow(who_tag))))
    mort.year.df = mort.df[sample(1:nrow(mort.df), size = 1),]
    r =  1 - rlogitnorm(n = 1, mu = mort.year.df$mu, sigma = mort.year.df$sigma)
    who_tag <- who_tag[which(who_tag[,2] < r),]
    if (length(who_tag) > 0) {
    #area.pop[,who_tag[,1]] <- 0 ##remove who dies, ie all columns gets 0
    area.pop[,who_tag$V1] <- 0
    #  pheno[who_tag[,1]] <- 0  #phenotypes of the dead (thus empty cols) are 0
    pheno[who_tag$V1] <- 0
    }
    }
    rm(who_tag)
    who_tag = as.matrix(which(area.pop[pheno.pos,]!=0 & area.pop[age.pos,]==1)) # where the individuals are (because empty columns have zero in first position) 
    if (length(who_tag) > 0) {
      who_tag = as.tibble(cbind(who_tag,runif(nrow(who_tag))))
      r =  1 - exp(3.2341 - 0.7426 * log(dens[max((i-1),1)] + 0.1))
      who_tag <- who_tag[which(who_tag[,2] < r),]
      if (length(who_tag) > 0) {
      #area.pop[,who_tag[,1]] <- 0
      area.pop[,who_tag$V1] <- 0
      #pheno[who_tag[,1]] <- 0
      pheno[who_tag$V1] <- 0
      }
    }
    rm(who_tag)
    ####
    
    vettad[i] = length(area.pop[pheno.pos,(area.pop[pheno.pos,]!=0 & area.pop[age.pos,]>=1)]) # populations size in the ocean post-natural mortality and post-harvest
    dens[i] = vettad[i] * mult_fact_stream
    ########population size post_mortality includig juveniles  
    
    
    ########## close mortality ##############
    
    
    ####################   REPRODUCTION ##########################
    
    ### 
    
    area.pop[matur.pos,] = 0  # set 0 maturity for every fish
    
    for (r_rep in 1:length(mat_prop)) {
    pot_rep = 0
    pp_mat = 0
    pot_rep = which(area.pop[age.pos,]==age_repr[r_rep]) # where are fish of age age_repr[r_rep]
    repr = runif(length(pot_rep))
    pp_mat <- which(repr < mat_prop[r_rep]) # which are the mature fish
    area.pop[matur.pos,pot_rep[pp_mat]] <- 1 ## mature fish get one
    }
    
    
    nsonscont = 0
    
    cosons = 0
    
    vettsons = rep(100,S*2)  # define with 100s and then delete those 100s when manipulating the vector
    #matsons = rbind(rep(100,Rp_stock*2),matrix(0,(length_prop+(num.loci*2)-1),Rp_stock*2))# define with 100s the first and then delete the remaining 100s 
    
    matsons = matrix(0,(length_prop+(num.loci*2)),S*2)
    
    matsons[1,] = 100
    
    #when manipulating the vector
    pheno_parents = rep(100,S*2)  ##empty vector that will contain the mean phenotype of parents
    #sex.from.parents = sample(c(0,1),S*2,replace=T)
    #matsons[(sex),] = sex.from.parents 
    matsons[age.pos,] = -1  # age of newborns is -1, so next year they will be 0 
    
    reproductor = which(area.pop[matur.pos,] == 1)       ##### vector of position in the matrix of reproductors
    stock = length(reproductor)  # stock
    
    recruit = round(predict(stock.r.mod, newdata = data.frame(Stream = "LIdri", Density.sp = stock * mult_fact_stream))/mult_fact_stream) # recruitment


    test.pop[[i]] = area.pop  # save area.pop matrix, mostly for testing
    
    
    ## Sexual reproduction
    
    
    gene.from.parents = rbind(rep(100,S*2),matrix(0,(num.loci*2*2)-1,S*2))
    
    number.of.sons = rep(100,S*2)
    
    if (recruit >1 & stock >=2)   { #open if for reproduction, that is there is at least one recruit
      
      
      ######### open control for odd number of reproductors
      
      if (length(reproductor)%%2 != 0) {		#open and close control for odd number of reproductors, since I assume that a female mates with only one male, if there is odd number of reproductors, I delete one of them
        
        reproductor <- reproductor[-sample(1:length(reproductor),1)]
      }  # close for odd number of reproductors 
      
      
      ############## close control for odd number of reproductors #########
      
      repro.matrix <- matrix(sample(reproductor,length(reproductor)),2,length(reproductor)/2) 
      ## produce the matrix of reproductors, each column is a couple. There is no male/female, but it could done by adding sex and change the assignment of mating pairs
      
      ## only 20% of pairs reproduce, typical for salmonids
      
      if(length(reproductor) > 5) { ## just by chance it can happen that when numbers are low, we then have zero reproductors
      who_repr = rbern(n = length(reproductor)/2, prob = 0.3) 
      if (sum(who_repr) == 0) {# at least 2 reproduce
      who_repr[sample(1:length(who_repr),1)] = 1
      }
      repro.matrix = repro.matrix[,which(who_repr==1),drop=FALSE] # keep the matrix!
      }
      rel_repr = rgamma(n = ncol(repro.matrix), alpha = 1, beta = 4) # gamma distribution of recruit per pair
      if (sum(rel_repr) == 0) {# at least 2 reproduce
        rel_repr[sample(1:length(rel_repr),1)] = 1
      }
      
      sons_rel = round((rel_repr/sum(rel_repr)) * recruit) # number of recruit per pair
      # at least one fish is produced
      
      ######### begin loop for reproductors ###########
      
      
      for(rr in 1:ncol(repro.matrix)) {   #begin loop for of reproduction, go on for all the couples
        
        nsonscouple <- sons_rel[rr]   ### this is the number of newborns for a couple, 
        #it is a deviate from a poisson distribution with sons.mean mean and variance
        
        
        if(nsonscouple>0) {  # open if for couples with at least one offspring
          
          
          gene.from.father = matrix(0,num.loci*2,nsonscouple)  # alleles coming from father (empty) used in case of recombination
          gene.from.mother = matrix(0,num.loci*2,nsonscouple) # alleles coming from mother (empty) used in case of recombination
          
          gene.sons = matrix(0,num.loci*2,nsonscouple)  #create a matrix with cols = number of offspring of the couple and rows = num.loci * 2
          
          ####### open options for recombination####
          if (recomb==0) {  # no recombination
            
            for (aa in 1:nsonscouple) {   #cicle for number of offspring
              
              sampleseq = sample(c(1,2),2,replace=T)    #draw the chromosome to be passed to offspring (First for father, second for mother)
              
              if (sampleseq[1]==1) {gene.sons[1:num.loci,aa] = area.pop[fchrom,repro.matrix[1,rr]]}  ## if 1 for father I pick the first chromosome (fchrom = 2:num.loci+1, schrom = num.loci + 2:max) 
              else if (sampleseq[1]==2) {gene.sons[1:num.loci,aa] = area.pop[schrom,repro.matrix[1,rr]]} # if 2 for father I pick the second chromosome
              
              if (sampleseq[2]==1) {gene.sons[(num.loci+1):(num.loci*2),aa] = area.pop[fchrom,repro.matrix[2,rr]]} ## if 1 for mother I pick the first chromosome (fchrom = 2:num.loci+1, schrom = num.loci + 2:max) 
              else if (sampleseq[2]==2) {gene.sons[(num.loci+1):(num.loci*2),aa] = area.pop[schrom,repro.matrix[2,rr]]} # if 2 for mother I pick the second chromosome
              
            }
          }  else if (recomb==1) { #### full recombination
            
            
            for (aa in 1:nsonscouple) {   #cicle for number of offspring
              
              rchrom.f = 0
              rchrom.m = 0
              
              
              sampleseq = sample(c(1,2),num.loci,replace=T)    #draw the chromosome to be passed to offspring (First for father, second for mother)
              rchrom.f = diag(cbind(fchrom,schrom)[,sampleseq])
              gene.sons[1:num.loci,aa] = area.pop[rchrom.f,repro.matrix[1,rr]]  ## if 1 for father I pick the first chromosome (fchrom = 2:num.loci+1, schrom = num.loci + 2:max) 
              
              
              gene.from.father[,aa] = area.pop[rchrom.f,repro.matrix[1,rr]] 
              
              
              sampleseq = sample(c(1,2),num.loci,replace=T)    #draw the chromosome to be passed to offspring (First for father, second for mother)
              rchrom.m = diag(cbind(fchrom,schrom)[,sampleseq])
              gene.sons[(num.loci+1):(num.loci*2),aa] = area.pop[rchrom.m,repro.matrix[2,rr]] ## if 1 for mother I pick the first chromosome (fchrom = 2:num.loci+1, schrom = num.loci + 2:max) 
              # if 2 for mother I pick the second chromosome
              
              gene.from.mother[,aa] = area.pop[rchrom.m,repro.matrix[2,rr]]
              
            }
          }  
          
        
          ##### close recombination options #########
          
          ########
          
           matsons[((length_prop+1):nrow(matsons)),(cosons+1):(cosons + nsonscouple)] = gene.sons
          
          matsons[1,(cosons+1):(cosons + nsonscouple)] <- 0
          pheno_parents[(cosons+1):(cosons + nsonscouple)] = mean(pheno[repro.matrix[1,rr]],pheno[repro.matrix[1,rr]])
          
          gene.from.parents[,(cosons+1):(cosons + nsonscouple)] = rbind(gene.from.father,gene.from.mother)
          
          number.of.sons[(cosons+1):(cosons + nsonscouple)] = nsonscouple
          
          cosons <- length(matsons[1,matsons[1,]!=100])      
          
        }  # close if  for couples with at least one offspring
      }   #close loop for reproduction in one sector
      
    } ### close for if (length(reproductor) >1)
    
    
    empty = which(area.pop[pheno.pos,]==0)  ### identify the available spots for newborns in the matrix. If there are more recruits than empty spots. some recruits die (the number of columns in the matrix is basically the carrying capacity of the system)
  
    
    ##### open if there are offspring produced ########
    
    
    if (length(matsons[1,matsons[1,]!=100]) >1) ##if there are offspring produced, those have value in pheno.pos row > 0
    { 
      
      matsons = matsons[,matsons[1,]!=100]  ## delete the empty places in matsons
      pheno_parents = pheno_parents[pheno_parents!=100]  ##delete the empty places in pheno_parents
      number.of.sons = number.of.sons[number.of.sons!=100]  ##delete the empty places in number.of.sons
      gene.from.parents = gene.from.parents[,gene.from.parents[1,]!=100] ##delete the empty places in gene.from.parents
      

      
      sons = 1  ## this is related to if (length(matsons[1,matsons[1,]!=100]) >1)
    } else # if there are no newborns produced
    { matsons[,] == 0  
      sons=0
    }
    
    ####### end of if (length(matsons[1,matsons[1,]!=100]) >1)  (including the else)
    
    
    
    
    vettpheno = rep(50,S*2) #### I prepare the empty vector of newborn phenotypes (50 means there is nothing)
    
    
    #### vector of offspring is empty if sons == 0 or no space is available in the matrix #####
    
    if(sons==0 | length(empty)==0){
      vettpheno=0
      ####### close of offspring is empty if sons == 0 or no space is available #####
    } else if (sons==1){
      
      if(length(empty)>=ncol(matsons) ){
        
        area.pop[1:nrow(area.pop),empty[1:ncol(matsons)]] = matsons
        
        area.pop[1,empty[1:ncol(matsons)]] = colSums(area.pop[(length_prop+1):nrow(area.pop),
                                                                empty[1:ncol(matsons)]])
        vettpheno[1:ncol(matsons)] <- area.pop[1,empty[1:ncol(matsons)]] + 
          rnorm(ncol(matsons),0,sqrt(var.env)) 
        
        vettpheno = vettpheno[vettpheno!=50]
        
        pheno[empty[1:ncol(matsons)]] <- vettpheno
        
        pheno_parents_unique = unique(pheno_parents)
        
        off.mean = rep(0,length(pheno_parents_unique))
        
        sons.for.couple = rep(0,length(pheno_parents_unique))
        
        

        
      
        
        num.sons.check[1,i] = 1   ##this tells me that there were more empty spots than offspring (when 0 is more offspring than spots)
        num.sons.check[2,i] = length(vettpheno) ##this tells me how many offspring were introduced
        #herit.coef[i] = summary(lm(vettpheno  ~ pheno_parents))$coef[2]
        #print(10)
        
        
        
      } else if(length(empty)==1){   #this is special case because I cannot use colSums with only one column
  
        area.pop[1:nrow(area.pop),empty]<-matsons[,1:length(empty)]
        area.pop[1,empty] = sum(area.pop[(length_prop+1):nrow(area.pop),empty])
        
        vettpheno[1:length(empty)] = area.pop[1,empty] + rnorm(length(empty),0,sqrt(var.env))
        
        vettpheno = vettpheno[vettpheno!=50]
        
        pheno[empty]<-vettpheno
        
      } else if(length(empty)<= ncol(matsons)){ # this case if there are more recruits than empty spots in the matrix
        
        
        
        area.pop[1:nrow(area.pop),empty]<-matsons[,1:length(empty)]
        area.pop[1,empty] = colSums(area.pop[(length_prop+1):nrow(area.pop),empty])
        
        vettpheno[1:length(empty)] = area.pop[1,empty] + rnorm(length(empty),0,sqrt(var.env))
        
        vettpheno = vettpheno[vettpheno!=50]
        
        pheno[empty]<-vettpheno
        
        pheno_parents_unique = unique(pheno_parents[1:length(empty)])
        
        off.mean = rep(0,length(pheno_parents_unique))
        sons.for.couple = rep(0,length(pheno_parents_unique))
        
        
        
        ##############
        
        num.sons.check[2,i] = length(vettpheno) ##this tells me how many offspring were introduced
        #herit.coef[i] = summary(lm(vettpheno  ~ pheno_parents[1:length(empty)]))$coef[2]
        #print(13)
        
        
        
      }
      
    }
    
    
    ### assign vB's parameters to newborns for growth ####
    
   
    if(length(empty) > 0) {
     area.pop[linf.pos,empty] = rnorm(n = length(empty), mean = L_inf, sd = L_inf/3)
    area.pop[k.pos,empty] = rnorm(n = length(empty), mean = k_vb, sd = k_vb/3)
    area.pop[t0.pos,empty] = rnorm(n = length(empty), mean = t0_vb, sd = -t0_vb/3)
    }
    
    ####    
    
    
    
    
    
    
    
    
    ####here I compute the mean and the variance of the new generation  phenotypes####
    if (i>1 & sons==1 & length(empty)>0) {
      w_gen_mean[i] = mean(vettpheno)
      w_gen_var[i]  = var(vettpheno)
    }
    
      
  year = i  # counter for plot, I cannot use iter because year < iter if the population went extinct   
    
    ####################   CLOSE REPRODUCTION ##########################
    
  }  ##close time
  
  ### heteroz  vector for each locus at the end of simulation time
  if (i == iter & iter >=50 & extinct ==0) {
    
    dim(heteroz.mat) =  c(num.loci,num.alleles)
    heteroz = matrix(0,num.loci,1)
    heteroz = 1 - rowSums(heteroz.mat^2)
  } else {heteroz = NULL}
  
  ### mean and sd of heteroz vector for each year of simulation time
  
  for (hh in 1:i) {
    
    all_freq_year = heter_mat_year[,hh]
    dim(all_freq_year) =  c(num.loci,num.alleles)
    heter_vect_mean_year[hh] = mean((1- rowSums(all_freq_year^2)), na.rm =T)
    heter_vect_sd_year[hh] = sd((1- rowSums(all_freq_year^2)), na.rm =T)
  } 
  
  ##################### CLOSE FOR TIME LOOP ########################
  
  
  
  ### PLOT
  
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
                        axis.title.y = element_text(size=size.label.x, vjust = 2),
                        axis.text.y  = element_text(size=size.text.x),
                        legend.title = element_blank(),
                        legend.text = element_text(size = size.legend.text),
                        legend.position = c(0.15, 0.95),
                        legend.key = element_rect(fill = "white")) 
  
## create the data.frame
  
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
    
  # pheno_gg = ggplot(dplyr::filter(pop_df, type == "pheno_var"), aes(x = year, y = abund)) +
  #   geom_point(alpha = 0.4) +
  #   geom_line() +
  #   theme.pop +
  #   guides(size = guide_legend(override.aes = list(alpha = 0.2))) +
  #   scale_y_continuous() +
  #   scale_x_continuous(limits = c(0,year+2)) +
  #   labs(y = "#") +
  #   labs(x = "Year") 
  
  } else {
    pop_gg = "Not enough simulation years for plot (must be > 10)"}
    #pheno_gg = "Not enough simulation years for plot (must be > 10)"} 
  
  
  ### change all.freq100 to data.frame ####
    
  all.freq100 = as.data.frame(all.freq100)
  steps = c(1,seq(0,iter,50)[-1])
  #steps[length(steps)] = min(i,steps[length(steps)])  ### if the population goes extinct
  colnames(all.freq100)[1] = "Allelic_value"
  colnames(all.freq100)[2:(length(steps)+1)] = paste("Freq_time",steps,sep ="_")
  colnames(all.freq100)[ncol(all.freq100)] = "Freq_diff"
  all.freq100$Gene = rep(0,nrow(all.freq100))
  
  for (j in 1:num.loci) {
    all.freq100$Gene[seq(j,(num.loci*num.alleles),num.loci)] = j
    
    
  }
  
  ### Gene 0 is the mutant alleles
  
  all.freq100 = arrange(all.freq100, Gene)
  
  
  ### For the exticnt populations, I want only the vectors up to year of exinction ####
  
  dens = dens[1:year]
  phenomean = phenomean[1:year]
  vettad = vettad[1:year]
  test.pop = test.pop[1:year]
  if (year > 10) {
  if (min(dens[10:year]) < 100 ) {quasi_ext = 1} else {quasi_ext = 0}
  } else {quasi_ext = 1}
  if (year > 10) {
  dens_cv = sd(dens[10:year])/mean(dens[10:year])} else {dens_cv = NA}
  if (year > 10) {
  dens_mean = mean(dens[10:year]) } else {dens_mean = NA}
  
  ris.list = list("extinct"=extinct, #1
                  "yearextinct" = i-1, #2
                  #"phenomean" = phenomean,#3
                  #"heteroz" = heteroz,#4
                  "numloci"=num.loci,#5
                  "num.alleles"=num.alleles,#6
                  "sd.alleles" = sd.alleles, #7
                  "recomb"= recomb, #8
                  "popsize"= vettad, #9
                   "dens" = dens,#10
                  #"plot_pop" = pop_gg,#11
                  #"pop" = pop_df,#12
                  "Stream" = Stream,#13
                  "year_max" = year_max,#14
                  "quasi_ext" = quasi_ext,#15
                  "dens_cv" = dens_cv,#16
                  "dens_mean" = dens_mean#17
                  )
  
  return(ris.list)
  
#  dev.off()
  
  #########other potential items to be returned######
  
  
}   # close all

#### The function returns a list of results that can be accessed with $
# extinct = 1 if a population wen extinct, 0 otherwise
# addvar = vector of additive genetic variance, one value for each year
# phenvar = vector of phenotypic variance, one value for each year 
# yearextinct = year of exinction if the population went extinct, iter-1 otherwise
# phenomean = vector of mean phenotype, one value for each year
# heteroz = vector of mean heterozigosity, one value for each year
# sdopt = input sdopt
# meanopt = input meanopt
# numloci = input num.loci
# num.alleles = input num.alleles,
# sd.alleles = input sd.alleles
# recomb = input recomb 
# major = input major
# mumut = input mu.mut
# mutalfa = input mut.alfa, 
# fitness.mean = vector of mean fitness, one value for each year 
# fitness.variance = vector of fitness variance, one value for each year 
# optimum = vector of realized climate variable values, one value for each year
# catastr.vett = vector of point extremes, 1 if the point extreme occurred, 0 otherwise
# popsize.post = population size (after mortality, both normal and due to point extreme), one value for eacy year
# cat.freq = input p.post 
# age.sex.mat = input age.sex.mat
# avg.surv = input avg.surv
# sons.mean = input sons.mean
# cor.all.est = correlation (Pearson's r) between allele frequency of alleles at time = 1 and allele frequency 
##    at the end of simulation time
# cor.all.p" = p-value of the correlation
# n.mut = number of mutant alleles in the population at the end of simulation time


##### DATA

# S.vett = 500
# N.vett = 500
# iter.vett = 250
# selection.vett = c(0.08,0.11,0.13,0.15)
# #selection.vett = c(0.15)
# sdopt.vett = c(0,0.01,0.015)
# num.loci.vett = 10
# plotorno.vett = 0
# meanopt.vett = c(0,0.015)
# major.vett = 0
# num.alleles.vett = 10
# sd.alleles.vett = 0.05
# recomb.vett = 0
# mutation.vett = 0
# mut.alfa.vett = 0.3
# catastr.vett = 1
# p.pre.vett = 0.05
# p.post.vett = c(0.05,0.1,0.15)
# catastr.mort.vett = c(0.3,0.5,0.7)
# age.sex.mat.vett = c(1,2,3,4)
# avg.surv.vett = c(0.7)
# sons.mean.vett = c(1,1.5,2,2.5,3,3.5)    ##### with 1 every population goes extinct
# first.phase.opt.vett = 100
# var.amb.init.vett = 1
# 
# 
# ###### remember that the columns must be named!!!
# 
# dataforpar = expand.grid(S = S.vett, N = N.vett ,iter = iter.vett , selection = selection.vett ,sdopt = sdopt.vett ,num.loci = num.loci.vett,plotorno = plotorno.vett,meanopt = meanopt.vett,major = major.vett,num.alleles = num.alleles.vett,sd.alleles = sd.alleles.vett ,recomb = recomb.vett ,mutation = mutation.vett,mut.alfa = mut.alfa.vett,catastr = catastr.vett,p.pre = p.pre.vett,p.post = p.post.vett,catastr.mort = catastr.mort.vett,
#                          age.sex.mat = age.sex.mat.vett,avg.surv = avg.surv.vett,sons.mean = sons.mean.vett,first.phase = first.phase.opt.vett,var.amb = var.amb.init.vett) 
# 
# 
# 
# replicate.sim = 10
# 
# dataforpar = do.call(rbind, replicate(replicate.sim, dataforpar, simplify=FALSE))