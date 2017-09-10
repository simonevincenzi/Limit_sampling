# Sets up the data in R

library(R2admb)
source("./scripts/ADMButils.s")
library(dplyr)

if(file.exists("./scripts/m_grow3.std") == TRUE) {file.remove("./scripts/m_grow3.std")}
if(file.exists("./scripts/m_grow3.dat") == TRUE) {file.remove("./scripts/m_grow3.dat")}
#rm(results.par)

## create matrix model depending on the input in the global env


if (linf_var == "Heter") {
linf.formula =  ~ heter
linf_ind_sd = 2}


if (linf_var == "Heter + Cohort") {
linf.formula =  ~ heter + Cohort
linf_ind_sd = 3}

if (linf_var == "Temp") {
linf.formula =  ~ dd.days_new
linf_ind_sd = 2}


if (linf_var == "Cohort") {
	linf.formula =  ~ Cohort
	linf_ind_sd = 1}
	
	if (linf_var == "P.Coh") {
	linf.formula =  ~ P.Coh
	linf_ind_sd = 1}

if (linf_var == "Feeding") {
  linf.formula =  ~ feeding_chick
}

if (linf_var == "Feeding and Cohort") {
  linf.formula =  ~ feeding_chick + Cohort
}

if (linf_var == "Feeding per Cohort") {
  linf.formula =  ~ feeding_chick * Cohort
}

if (linf_var == "Sex") {
  linf.formula =  ~ Sex_gen
  linf_ind_sd = 1
}

if (linf_var == "Const") {
		linf.formula =  ~ 1
		linf_ind_sd = 1
		}
		
if (linf_var == "Flood.Coh") {
		linf.formula =  ~ Flood.Coh
		linf_ind_sd = 1
		}

if (linf_var == "Sex + Cohort") {
  linf.formula =  ~ Sex_gen + Cohort
  linf_ind_sd = 4
}

if (linf_var == "Sector") {
  linf.formula =  ~ Sector
  linf_ind_sd = 1
}

if (linf_var == "Pop") {
  linf.formula =  ~ Pop
  linf_ind_sd = 1
}

if (linf_var == "Species") {
  linf.formula =  ~ Species
  linf_ind_sd = 1
}

if (linf_var == "Pop + Cohort") {
  linf.formula =  ~ Pop + Cohort
  linf_ind_sd = 4
}

########## k

		
if (k_var == "Heter") {
k.formula =  ~ heter
k_ind_sd = 2
}

if (k_var == "Heter + Cohort") {
k.formula =  ~ heter + Cohort
k_ind_sd = 3}

if (k_var == "Temp") {
k.formula =  ~ dd.days_new
k_ind_sd = 2}


if (k_var == "Cohort") {
	k.formula =  ~ Cohort
	k_ind_sd = 1
	}
	
if (k_var == "P.Coh") {
	k.formula =  ~ P.Coh
	k_ind_sd = 1}


if (k_var == "Feeding") {
  k.formula =  ~ feeding_chick
}

if (k_var == "Feeding and Cohort") {
  k.formula =  ~ feeding_chick + Cohort
}

if (k_var == "Feeding per Cohort") {
  k.formula =  ~ feeding_chick * Cohort
}

if (k_var == "Sex") {
  k.formula =  ~ Sex_gen
  k_ind_sd = 1
}

if (k_var == "Const") {
		k.formula =  ~ 1
		k_ind_sd = 1
		}
		
if (k_var == "Flood.Coh") {
		k.formula =  ~ Flood.Coh
		k_ind_sd = 1
		}
		
if (k_var == "Sex + Cohort") {
  k.formula =  ~ Sex_gen + Cohort
  k_ind_sd = 4
}

if (k_var == "Sector") {
  k.formula =  ~ Sector
  k_ind_sd = 1
}


if (k_var == "Pop") {
  k.formula =  ~ Pop
  k_ind_sd = 1
}

if (k_var == "Species") {
  k.formula =  ~ Species
  k_ind_sd = 1
}

if (k_var == "Pop + Cohort") {
  k.formula =  ~ Pop + Cohort
  k_ind_sd = 4
}

##############
		
		
t0.formula = ~ 1

linf.mm = model.matrix(linf.formula,data=data_growth)
k.mm = model.matrix(k.formula,data=data_growth)
t0.mm = model.matrix(t0.formula,data = data_growth)



## Density is not used in this work

X = linf.mm

if (linf_var == "Density") {
X[,2]=X[,2]/1000
}

if (linf_var == "Temp") {
X[,2]=X[,2]/1000
}

if (linf_var == "Density + Temp") {
X[,2]=X[,2]/1000
X[,3]=X[,3]/1000
}



X_k0 = k.mm

if (k_var =="Density") {
X_k0[,2]=X_k0[,2]/1000
}

if (k_var == "Temp") {
X_k0[,2]=X_k0[,2]/1000
}

if (k_var == "Density + Temp") {
X_k0[,2]=X_k0[,2]/1000
X_k0[,3]=X_k0[,3]/1000
}


X_t0 = t0.mm

#X=X[,c(1,20)]
#X_k0=as.matrix(X_k0[,1])


###################

#"Cohort","Sex","Flood.Coh","Density","Temp","Density + Temp","Sex + Cohort"
if (linf_var %in% c("Heter","Cohort","Sex","Flood.Coh","Const",
"Heter","Temp","Heter + Cohort","P.Coh","Sector", "Pop", "Species")) {

n_linf_ind_sd = ncol(X)
}

if (linf_var %in% "Sex + Cohort") {

n_linf_ind_sd = (ncol(X)-1) * 2

}

if (linf_var %in% "Pop + Cohort") {
  
  n_linf_ind_sd = (ncol(X)-1) * 2
  
}


if (k_var %in% c("Cohort","Sex","Flood.Coh","Const",
"Heter","Temp","Heter + Cohort","P.Coh","Sector", "Pop", "Species")) {

n_k_ind_sd = ncol(X_k0)
}

if (k_var %in% "Sex + Cohort") {

n_k_ind_sd = (ncol(X_k0)-1) * 2

}

if (k_var %in% "Pop + Cohort") {
  
  n_k_ind_sd = (ncol(X_k0)-1) * 2
  
}






#####
## ordering Marks (tag of individuals)

# diffmark  = diff(as.numeric(data_growth$Mark))
# diffmark  = ifelse(diffmark==0,0,1)



#####
## ordering Marks (tag of individuals)


 Mark.succ = data_growth$Mark

 which.nonnum <- function(x) {
   which(is.na(suppressWarnings(as.numeric(as.character(x)))))
 }
 
 char.row = which.nonnum(Mark.succ)
 
 #char.row = which(is.na(as.numeric(Mark.succ)))

 for (jj in 1:length(unique(Mark.succ[char.row]))) {

   Mark.succ[which(Mark.succ == unique(Mark.succ[char.row][jj]))] = 5555555 + jj


 }

 diffmark  = diff(as.numeric(Mark.succ))



 diffmark  = ifelse(diffmark==0,0,1)


synth.list.3 = list() #list for storing results

### writes the dat file - remember that write_dat is in ADMButil.s

###It changed to dat_write

dat_write("./scripts/m_grow3",list(linf_ind_sd = linf_ind_sd,
n_linf_ind_sd = n_linf_ind_sd,
k_ind_sd = k_ind_sd,
n_k_ind_sd = n_k_ind_sd,
n=nrow(data_growth),
		length=data_growth$Length,
		age=data_growth$Age,
                total_marks=length(unique(data_growth$Mark)),
		start=c(1,which(diffmark>0)+1),
		stop=c(which(diffmark>0),nrow(data_growth)),
		q=ncol(X),
		X=X,
		q_k0=ncol(X_k0),
		X_k0=X_k0,
		q_t0=ncol(X_t0),
X_t0=X_t0,
check=12345))



system("./scripts/m_grow3") ### calls m_grow18 with no standard deviations computed. To compute standard deviations, delete -est

results.par = par_read("./scripts/m_grow3.par") # reads the parameter estimates and other quantities (number of parameters, gradient given by ADMB etc.)

pred = read.table("./scripts/m_grow3.rep")# .rep contains the predictions (using the model) of data used for fitting the model

prov.AIC = -2*results.par$n_par - 2*results.par$loglik #compute AIC (the change of sign is due to a bug in ADMButils)
prov.gradient = results.par$gradient

#assign(paste("results.AIC.zak0.13psi.",psi_fixed.vett[i],sep=""), prov.AIC)

#assign(paste("results.grad.zak0.13psi.",psi_fixed.vett[i],sep=""), prov.gradient)

# here below I assign to the list synth.list.18 the output of the model fitting

synth.list.3$formula = c(paste("linf ~",linf_var),paste("k ~",k_var))
synth.list.3$AIC = prov.AIC
synth.list.3$par = results.par 
synth.list.3$rep = pred
synth.list.3$grad = prov.gradient
if(file.exists("./scripts/m_grow3.std") == TRUE) {
synth.list.3$std = read.table("./scripts/m_grow3.std", header = T) 
} else {synth.list.3$std = NULL}



#######

VBpred.f = function (Linf,k,t0,plpo,maxage=10) {
  age = seq(0,maxage,1)
  Length_age = rep(0,length(age))

  for (ii in 1:length(age)) {
    
    Length_age[ii] = Linf*(1-exp(-k*(age[ii]-t0)))
    
  }
  if (plpo==0) {
    plot(Length_age ~ age, xlim = c(0,max(age)),ylim = c(0,max(Length_age)),pch=16)
  }
  
  else if (plpo==1) {
    points(Length_age ~ age,pch=15)
  }
  
  return(data.frame(Age=age,Length=Length_age))
}



##### produce data frame of Linf, k, t0, expected length at age 1,2,3


if(exists("results.par")) {
  
  
  
  
  if(exists("klinf.df")) {rm(klinf.df)}
  
  
  
  
  
  if(!linf_var %in% c("Cohort","Sex","Flood.Coh","Heter","Temp","Heter + Cohort","Sex + Cohort","P.Coh","Sector","Pop", "Species","Pop + Cohort") & 
  !k_var %in% c("Cohort","Sex","Flood.Coh","Heter","Temp","Heter + Cohort","Sex + Cohort","P.Coh","Sector","Pop", "Species","Pop + Cohort")) {
  
  k = exp(results.par$beta_k0) * exp(exp(results.par$log_sigma_u)*results.par$u )
  Linf = exp(results.par$beta*10 + exp(results.par$log_sigma_v)*results.par$v)
                         
 klinf.df = data.frame(Mark = unique(data_growth$Mark), 
                        k = k, linf = Linf, u = results.par$u, v = results.par$v) 
 
 mark.un = unique(data_growth$Mark)
 klinf.df$Cohort = rep(0,nrow(klinf.df))
 klinf.df$Cohort_C = rep(0,nrow(klinf.df))
 
 for (jj in 1:nrow(klinf.df)) {
   
   klinf.df$Cohort[jj] = as.numeric(data_growth[data_growth$Mark == mark.un[jj],"Cohort"][1,1])
   klinf.df$Cohort_C[jj] = as.character(data_growth[data_growth$Mark == mark.un[jj],"Cohort"][1,1]) 
   
   
   
   
 }
  
  }


#data = m_grow3_gat_Linfcohort_kcohort.par 

if(linf_var %in% c("Cohort","Sex","Flood.Coh","Heter","Temp","Heter + Cohort","Sex + Cohort","P.Coh","Sector","Pop", "Species","Pop + Cohort") | 
k_var %in% c("Cohort","Sex","Flood.Coh","Heter","Temp","Heter + Cohort","Sex + Cohort","P.Coh","Sector","Pop", "Species","Pop + Cohort")) {
  
  
  mark.un = unique(data_growth$Mark)
  
  col.len = rep(0,length(mark.un))
  
  klinf.df = data.frame(Mark = col.len, Cohort = col.len,Cohort_C = col.len, Sex = col.len,
                     k = col.len, linf = col.len,u = col.len, v= col.len, beta = col.len)
 
  for (i in 1:length(mark.un)) {
    
    # print(i)
    ###### Assign mark, cohort, u, v, and Sex if present
    
    klinf.df$Cohort[i] = as.numeric(data_growth[data_growth$Mark == mark.un[i],"Cohort"][1,1])
    if ("Flood.Coh" %in% colnames(data_growth)){
    klinf.df$Flood.Coh[i] = as.numeric(data_growth[data_growth$Mark == mark.un[i],"Flood.Coh"][1,1])}
    if ("P.coh" %in% colnames(data_growth)){
    klinf.df$P.Coh[i] = as.numeric(data_growth[data_growth$Mark == mark.un[i],"P.Coh"][1,1])}
    
    klinf.df$Cohort_C[i] = as.character(data_growth[data_growth$Mark == mark.un[i],"Cohort"][1,1])
    
    if ("heter" %in% colnames(data_growth)){
    klinf.df$heter[i] = as.numeric(data_growth[data_growth$Mark == mark.un[i],"heter"][1,1])}
    if ("dd.days_new" %in% colnames(data_growth)){
    klinf.df$dd.days_new[i] = data_growth[data_growth$Mark == mark.un[i],"dd.days_new"][1,1]}
    
    klinf.df$u[i] = results.par$u[i]
    klinf.df$v[i] = results.par$v[i]
    
    if (linf_var == "Sex" | k_var == "Sex" | linf_var == "Sex + Cohort" | 
    k_var == "Sex + Cohort") {
      
      klinf.df$Sex[i] = as.character(data_growth[data_growth$Mark == mark.un[i],"Sex_gen"][1,1])

    }
    
    if (linf_var == "Sector" | k_var == "Sector") {
      
      klinf.df$Sector[i] = as.character(data_growth[data_growth$Mark == mark.un[i],"Sector"][1,1])
      
    }
    
    if (linf_var == "Pop" | k_var == "Pop" | linf_var == "Pop + Cohort" | 
        k_var == "Pop + Cohort") {
      
      klinf.df$Pop[i] = as.character(data_growth[data_growth$Mark == mark.un[i],"Pop"][1,1])
      
    }
    
    if (linf_var == "Species" | k_var == "Species") {
      
      klinf.df$Species[i] = as.character(data_growth[data_growth$Mark == mark.un[i],"Species"][1,1])
      
    }
    
    
    
    klinf.df$Mark[i] = mark.un[i]
    
    ########
    
    ############# linf Sex
    
    if (linf_var == "Sex") {
      
          if(which(levels(data_growth$Sex_gen) == 
                     klinf.df$Sex[i]) == 1) {
      
            klinf.df$linf[i] = exp(results.par$beta[1]*10 + exp(results.par$log_sigma_v) *
                               klinf.df$v[i])
            
            
            klinf.df$beta[i] = results.par$beta[1]
      
          } else {
        
            klinf.df$linf[i] = exp((results.par$beta[1] + results.par$beta[2])*10 + 
                                 exp(results.par$log_sigma_v) * klinf.df$v[i])
                              klinf.df$beta[i] = results.par$beta[2]}
                      
          
          
          
          
    }
    
    
    ############# linf Sector
    
    if (linf_var == "Sector") {
      
      if(which(levels(data_growth$Sector) == 
               klinf.df$Sector[i]) == 1) {
        
        klinf.df$linf[i] = exp(results.par$beta[1]*10 + exp(results.par$log_sigma_v) *
                                 klinf.df$v[i])
        
        
        klinf.df$beta[i] = results.par$beta[1]
        
      } else {
        
        klinf.df$linf[i] = exp((results.par$beta[1] + results.par$beta[2])*10 + 
                                 exp(results.par$log_sigma_v) * klinf.df$v[i])
        klinf.df$beta[i] = results.par$beta[2]}
      
      
      
      
      
    }
    
    
    ############# linf Pop
    
    if (linf_var == "Pop") {
      
      if(which(levels(data_growth$Pop) == 
               klinf.df$Pop[i]) == 1) {
        
        klinf.df$linf[i] = exp(results.par$beta[1]*10 + exp(results.par$log_sigma_v) *
                                 klinf.df$v[i])
        
        
        klinf.df$beta[i] = results.par$beta[1]
        
      } else {
        
        klinf.df$linf[i] = exp((results.par$beta[1] + results.par$beta[2])*10 + 
                                 exp(results.par$log_sigma_v) * klinf.df$v[i])
        klinf.df$beta[i] = results.par$beta[2]}
      
      
      
      
      
    }
    
    ############# linf Species
    
    if (linf_var == "Species") {
      
      if(which(levels(data_growth$Species) == 
               klinf.df$Species[i]) == 1) {
        
        klinf.df$linf[i] = exp(results.par$beta[1]*10 + exp(results.par$log_sigma_v) *
                                 klinf.df$v[i])
        
        
        klinf.df$beta[i] = results.par$beta[1]
        
      } else {
        
        klinf.df$linf[i] = exp((results.par$beta[1] + results.par$beta[2])*10 + 
                                 exp(results.par$log_sigma_v) * klinf.df$v[i])
        klinf.df$beta[i] = results.par$beta[2]}
      
      
      
      
      
    }
    
    
    
    ############## linf Cohort
    
    
    if (linf_var == "Cohort")  {
    
          if (klinf.df$Cohort[i] ==1) {
      
            klinf.df$linf[i] = exp(results.par$beta[1]*10 + 
                                     exp(results.par$log_sigma_v) * klinf.df$v[i])
            
              }
    
                    else {
      
                          klinf.df$linf[i] = exp((results.par$beta[1] + 
                                    results.par$beta[klinf.df$Cohort[i]])*10 + 
                              exp(results.par$log_sigma_v)* klinf.df$v[i])}
                }
      
      
       ############## linf Flood.Coh
    
    
    if (linf_var == "Flood.Coh")  {
    
          if (klinf.df$Flood.Coh[i] ==1) {
      
            klinf.df$linf[i] = exp(results.par$beta[1]*10 + 
                                     exp(results.par$log_sigma_v) * klinf.df$v[i])
            
              }
    
                    else {
      
                          klinf.df$linf[i] = exp((results.par$beta[1] + 
                                    results.par$beta[klinf.df$Cohort[i]])*10 + 
                              exp(results.par$log_sigma_v)* klinf.df$v[i])}
                }
                
     
     
     ######## linf Parent and Other cohorts
     
     if (linf_var == "P.Coh")  {
    
          if (klinf.df$P.Coh[i] ==1) {
      
            klinf.df$linf[i] = exp(results.par$beta[1]*10 + 
                                     exp(results.par$log_sigma_v) * klinf.df$v[i])
            
              }
    
                    else {
      
                          klinf.df$linf[i] = exp((results.par$beta[1] + 
                                    results.par$beta[klinf.df$Cohort[i]])*10 + 
                              exp(results.par$log_sigma_v)* klinf.df$v[i])}
                }           
                
     ###### linf Density 
     
     if (linf_var == "Heter")  {
    
      
            klinf.df$linf[i] = exp((results.par$beta[1] + 
                                    results.par$beta[2]*(klinf.df$heter[i]))*10 + 
                                     exp(results.par$log_sigma_v) * klinf.df$v[i])
            
              }          
              
              
              
              
       ###### linf Temperature 
     
     if (linf_var == "Temp")  {
    
      
            klinf.df$linf[i] = exp((results.par$beta[1] + 
                                    results.par$beta[2]*(klinf.df$dd.days_new[i]/1000))*10 + 
                                     exp(results.par$log_sigma_v) * klinf.df$v[i])
            
              }                  
              
              
              
        ###### linf Heter + Cohort 
     
     if (linf_var == "Heter + Cohort")  {
    
      
            klinf.df$linf[i] = exp((results.par$beta[1] + 
                                    results.par$beta[2]*(klinf.df$heter[i]) +
                                    results.par$beta[3]*(klinf.df$Cohort[i]))*10 + 
                                     exp(results.par$log_sigma_v) * klinf.df$v[i])
            
              }   
              
              
              
           ############## linf Sex and Cohort
    
    
    if (linf_var == "Sex + Cohort")  {
    
          if (klinf.df$Sex[i] == "M" & klinf.df$Cohort[i] ==1) {
      
            klinf.df$linf[i] = exp(results.par$beta[1]*10 + 
                                     exp(results.par$log_sigma_v) * klinf.df$v[i])
            
              }
    
                    else if (klinf.df$Sex[i] == "M" & klinf.df$Cohort[i]!=1){
      
                          klinf.df$linf[i] = exp((results.par$beta[1] + 
                                    results.par$beta[klinf.df$Cohort[i]+1])*10 + 
                              exp(results.par$log_sigma_v)* klinf.df$v[i])}
                
                else if (klinf.df$Sex[i] == "F" & klinf.df$Cohort[i] ==1){
                
                klinf.df$linf[i] = exp((results.par$beta[1] + 
                                    results.par$beta[2])*10 + ## 2 col is Sex 2
                              exp(results.par$log_sigma_v)* klinf.df$v[i])}
                
                else if (klinf.df$Sex[i] == "F" & klinf.df$Cohort[i]!=1){
      
                          klinf.df$linf[i] = exp((results.par$beta[1] + ## Cohort 1 and Sex 1
                                    results.par$beta[2]+  ## Sex 2
                                    results.par$beta[klinf.df$Cohort[i]+1])*10 + # Cohort !=1 
                              exp(results.par$log_sigma_v)* klinf.df$v[i])}
                
                }
                 
    
    ############## linf Pop and Cohort
    
    
    if (linf_var == "Pop + Cohort")  {
      
      if (klinf.df$Pop[i] == "LIdri_MT" & klinf.df$Cohort[i] ==1) {
        
        klinf.df$linf[i] = exp(results.par$beta[1]*10 + 
                                 exp(results.par$log_sigma_v) * klinf.df$v[i])
        
      }
      
      else if (klinf.df$Pop[i] == "LIdri_MT" & klinf.df$Cohort[i]!=1){
        
        klinf.df$linf[i] = exp((results.par$beta[1] + 
                                  results.par$beta[klinf.df$Cohort[i]+1])*10 + 
                                 exp(results.par$log_sigma_v)* klinf.df$v[i])}
      
      else if (klinf.df$Pop[i] == "UIdri" & klinf.df$Cohort[i] ==1){
        
        klinf.df$linf[i] = exp((results.par$beta[1] + 
                                  results.par$beta[2])*10 + ## 2 col is Sex 2
                                 exp(results.par$log_sigma_v)* klinf.df$v[i])}
      
      else if (klinf.df$Pop[i] == "UIdri" & klinf.df$Cohort[i]!=1){
        
        klinf.df$linf[i] = exp((results.par$beta[1] + ## Cohort 1 and Sex 1
                                  results.par$beta[2]+  ## Sex 2
                                  results.par$beta[klinf.df$Cohort[i]+1])*10 + # Cohort !=1 
                                 exp(results.par$log_sigma_v)* klinf.df$v[i])}
      
    }             
                
      
       ############## linf Const
    
    
    if (linf_var == "Const")  {
    
            klinf.df$linf[i] = exp(results.par$beta[1]*10 + 
                                     exp(results.par$log_sigma_v) * klinf.df$v[i])
        
                }
                            
    
################ k Sex    

          if (k_var == "Sex") {
  
            if(which(levels(data_growth$Sex_gen) == 
                       klinf.df$Sex[i]) == 1) {
    
                      klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                             exp(results.par$log_sigma_u) * klinf.df$u[i])
    
                    } else {
    
                          klinf.df$k[i] = exp(results.par$beta_k0[1] + results.par$beta_k0[2] + 
                             exp(results.par$log_sigma_u) * klinf.df$u[i])}
  
                        }

    ################ k Sector    
    
    if (k_var == "Sector") {
      
      if(which(levels(data_growth$Sector) == 
               klinf.df$Sector[i]) == 1) {
        
        klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                              exp(results.par$log_sigma_u) * klinf.df$u[i])
        
      } else {
        
        klinf.df$k[i] = exp(results.par$beta_k0[1] + results.par$beta_k0[2] + 
                              exp(results.par$log_sigma_u) * klinf.df$u[i])}
      
    }
    
    ################ k Pop    
    
    if (k_var == "Pop") {
      
      if(which(levels(data_growth$Pop) == 
               klinf.df$Pop[i]) == 1) {
        
        klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                              exp(results.par$log_sigma_u) * klinf.df$u[i])
        
      } else {
        
        klinf.df$k[i] = exp(results.par$beta_k0[1] + results.par$beta_k0[2] + 
                              exp(results.par$log_sigma_u) * klinf.df$u[i])}
      
    }
    
    ################ k Species    
    
    if (k_var == "Species") {
      
      if(which(levels(data_growth$Species) == 
               klinf.df$Species[i]) == 1) {
        
        klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                              exp(results.par$log_sigma_u) * klinf.df$u[i])
        
      } else {
        
        klinf.df$k[i] = exp(results.par$beta_k0[1] + results.par$beta_k0[2] + 
                              exp(results.par$log_sigma_u) * klinf.df$u[i])}
      
    }
    
    
    
############## k Cohort


          if (k_var == "Cohort")  {
  
              if (klinf.df$Cohort[i] ==1) {
    
                klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                             exp(results.par$log_sigma_u) * klinf.df$u[i])
    
                        }
  
                      else {
    
                            klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                              results.par$beta_k0[klinf.df$Cohort[i]] + 
                             exp(results.par$log_sigma_u)* klinf.df$u[i])}
                                  }

############## k Flood.Coh


          if (k_var == "Flood.Coh")  {
  
              if (klinf.df$Flood.Coh[i] ==1) {
    
                klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                             exp(results.par$log_sigma_u) * klinf.df$u[i])
    
                        }
  
                      else {
    
                            klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                              results.par$beta_k0[klinf.df$Cohort[i]] + 
                             exp(results.par$log_sigma_u)* klinf.df$u[i])}
                                  }
  
  
  ############## k Parental and Other cohorts


          if (k_var == "P.Coh")  {
  
              if (klinf.df$P.Coh[i] ==1) {
    
                klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                             exp(results.par$log_sigma_u) * klinf.df$u[i])
    
                        }
  
                      else {
    
                            klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                              results.par$beta_k0[klinf.df$Cohort[i]] + 
                             exp(results.par$log_sigma_u)* klinf.df$u[i])}
                                  }
  
  
  
   ###### k Heter
     
     if (k_var == "Heter")  {
    
      
            klinf.df$k[i] = exp((results.par$beta_k0[1] + 
                                    results.par$beta_k0[2]*(klinf.df$heter[i]))+ 
                                     exp(results.par$log_sigma_u) * klinf.df$u[i])
            
              }          
              
              
              
              
       ###### k Temperature 
     
     if (k_var == "Temp")  {
    
      
            klinf.df$k[i] = exp((results.par$beta_k0[1] + 
                                    results.par$beta_k0[2]*(klinf.df$dd.days_new[i]/1000)) + 
                                     exp(results.par$log_sigma_u) * klinf.df$u[i])
            
              }                  
              
              
              
        ###### k Heter + Cohort 
     
     if (k_var == "Heter + Cohort")  {
    
      
            klinf.df$k[i] = exp((results.par$beta_k0[1] + 
                                    results.par$beta_k0[2]*(klinf.df$heter[i]) +
                                    results.par$beta_k0[3]*(klinf.df$Cohort[i])) + 
                                     exp(results.par$log_sigma_u) * klinf.df$u[i])
            
              }                        
                
  
      ############## k Sex + Cohort
    
    
    if (k_var == "Sex + Cohort")  {
    
          if (klinf.df$Sex[i] == "M" & klinf.df$Cohort[i] ==1) {
      
            klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                                     exp(results.par$log_sigma_u) * klinf.df$u[i])
            
              }
    
                    else if (klinf.df$Sex[i] == "M" & klinf.df$Cohort[i]!=1){
      
                          klinf.df$k[i] = exp((results.par$beta_k0[1] + 
                                    results.par$beta_k0[klinf.df$Cohort[i]+1]) + 
                              exp(results.par$log_sigma_u)* klinf.df$u[i])}
                
                else if (klinf.df$Sex[i] == "F" & klinf.df$Cohort[i] ==1){
                
                klinf.df$k[i] = exp((results.par$beta_k0[1] + 
                                    results.par$beta_k0[2]) + ## 2 col is Sex 2
                              exp(results.par$log_sigma_u)* klinf.df$u[i])}
                
                else if (klinf.df$Sex[i] == "F" & klinf.df$Cohort[i]!=1){
      
                          klinf.df$k[i] = exp((results.par$beta_k0[1] + ## Cohort 1 and Sex 1
                                    results.par$beta_k0[2]+  ## Sex 2
                                    results.par$beta_k0[klinf.df$Cohort[i]+1]) + # Cohort !=1 
                              exp(results.par$log_sigma_u)* klinf.df$u[i])}
                
                }
                                  
    
    ############## k Pop + Cohort
    
    
    if (k_var == "Pop + Cohort")  {
      
      if (klinf.df$Pop[i] == "LIdri_MT" & klinf.df$Cohort[i] ==1) {
        
        klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                              exp(results.par$log_sigma_u) * klinf.df$u[i])
        
      }
      
      else if (klinf.df$Pop[i] == "LIdri_MT" & klinf.df$Cohort[i]!=1){
        
        klinf.df$k[i] = exp((results.par$beta_k0[1] + 
                               results.par$beta_k0[klinf.df$Cohort[i]+1]) + 
                              exp(results.par$log_sigma_u)* klinf.df$u[i])}
      
      else if (klinf.df$Pop[i] == "UIdri" & klinf.df$Cohort[i] ==1){
        
        klinf.df$k[i] = exp((results.par$beta_k0[1] + 
                               results.par$beta_k0[2]) + ## 2 col is Sex 2
                              exp(results.par$log_sigma_u)* klinf.df$u[i])}
      
      else if (klinf.df$Pop[i] == "UIdri" & klinf.df$Cohort[i]!=1){
        
        klinf.df$k[i] = exp((results.par$beta_k0[1] + ## Cohort 1 and Sex 1
                               results.par$beta_k0[2]+  ## Sex 2
                               results.par$beta_k0[klinf.df$Cohort[i]+1]) + # Cohort !=1 
                              exp(results.par$log_sigma_u)* klinf.df$u[i])}
      
    }
                                  
############## k Const


          if (k_var == "Const")  {
  
    
                klinf.df$k[i] = exp(results.par$beta_k0[1] + 
                             exp(results.par$log_sigma_u) * klinf.df$u[i])
                                  }

########################################
                               
 }}
  
  klinf.df$t0 = results.par$beta_t0 

  klinf.df$expL1 = rep(0,nrow(klinf.df))
  klinf.df$expL2 = rep(0,nrow(klinf.df))
  klinf.df$expL3 = rep(0,nrow(klinf.df))
  
  for (j in 1:nrow(klinf.df)) {
    
    
    plot.df = VBpred.f(klinf.df$linf[j],klinf.df$k[j],klinf.df$t0[j],2,maxage=5)
    
    klinf.df$expL1[j] = filter(plot.df, Age == 1)$Length
    klinf.df$expL2[j] = filter(plot.df, Age == 2)$Length
    klinf.df$expL3[j] = filter(plot.df, Age == 3)$Length
    
  }
  
  }

 Times.df = as.data.frame(with(data_growth,table(Mark)))


Times.df = Times.df[match(unique(data_growth$Mark), Times.df$Mark),] 
klinf.df$Times = Times.df$Freq 
klinf.df$check = Times.df$Mark
synth.list.3$pred_vb = klinf.df

####### Here I add a dataframe with link, k, and to (lcl and ucl) per cohort

if((linf_var == "Const" & k_var  == "Const") & file.exists("./scripts/m_grow3.std") == TRUE ) {

nrow.df = 1

Coh.par = data.frame(
 linf = rep(0,nrow.df),
 linf_lcl = rep(0,nrow.df),
 linf_ucl = rep(0,nrow.df),
 k = rep(0,nrow.df),
  k_lcl = rep(0,nrow.df),
   k_ucl = rep(0,nrow.df),
 t0 = rep(0,nrow.df),
 t0_lcl = rep(0,nrow.df),
 t0_ucl = rep(0,nrow.df),
 Samples = rep(0,nrow.df),
 n_data = rep(0,nrow.df),
 expL1 = rep(0,nrow.df),
 expL2 = rep(0,nrow.df),
 expL3 = rep(0,nrow.df),
 obsL1 = rep(0,nrow.df),
 obsL1_lcl = rep(0,nrow.df),
 obsL1_ucl = rep(0,nrow.df),
 obsL2 = rep(0,nrow.df),
 obsL2_lcl = rep(0,nrow.df),
 obsL2_ucl = rep(0,nrow.df),
 obsL3 = rep(0,nrow.df),
 obsL3_lcl = rep(0,nrow.df),
 obsL3_ucl = rep(0,nrow.df))
 
Coh.par$n_data = nrow(data_growth)
Coh.par$Samples = length(unique(data_growth$Mark))
 
 
 klinf.pos = which(synth.list.3$std$name == "Linf_sd")
 k.pos = which(synth.list.3$std$name == "k_sd")
 t0.pos = which(synth.list.3$std$name == "t0_sd")
 
 
 
 ####### Linf
 Coh.par$linf = synth.list.3$std$value[klinf.pos]
 Coh.par$linf_lcl = synth.list.3$std$value[klinf.pos] - 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 Coh.par$linf_ucl = synth.list.3$std$value[klinf.pos] + 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 
 ######## k
 
 Coh.par$k = synth.list.3$std$value[k.pos]
 Coh.par$k_lcl = synth.list.3$std$value[k.pos] - 
 1.96*synth.list.3$std$std.dev[k.pos]
 Coh.par$k_ucl = synth.list.3$std$value[k.pos] + 
 1.96*synth.list.3$std$std.dev[k.pos]
 
 ######## t0
 
 Coh.par$t0 = synth.list.3$std$value[t0.pos]
 Coh.par$t0_lcl = synth.list.3$std$value[t0.pos] - 
 1.96*synth.list.3$std$std.dev[t0.pos]
 Coh.par$t0_ucl = synth.list.3$std$value[t0.pos] + 
 1.96*synth.list.3$std$std.dev[t0.pos]
 
 
 ##### Expected and observed length
 
 j = 1
 
 plot.df = VBpred.f(Coh.par$linf[j],Coh.par$k[j],Coh.par$t0[j],2,maxage=10)
    
    Coh.par$expL1[j] = filter(plot.df, Age == 1)$Length
    Coh.par$expL2[j] = filter(plot.df, Age == 2)$Length
    Coh.par$expL3[j] = filter(plot.df, Age == 3)$Length
    
    
    Coh.par$obsL1[j] = mean(filter(data_growth, Age == 1)$Length,na.rm = T)
      Coh.par$obsL1_lcl[j] = as.numeric(quantile(filter(data_growth, 
      Age == 1)$Length,c(0.025,0.975))[1])
      Coh.par$obsL1_ucl[j] = as.numeric(quantile(filter(data_growth, 
      Age == 1)$Length,c(0.025,0.975))[2])
     
     Coh.par$obsL2[j] = mean(filter(data_growth, Age == 2)$Length,na.rm = T) 
     Coh.par$obsL2_lcl[j] = as.numeric(quantile(filter(data_growth,
     Age == 2)$Length,c(0.025,0.975))[1])
      Coh.par$obsL2_ucl[j] = as.numeric(quantile(filter(data_growth,
      Age == 2)$Length,c(0.025,0.975))[2])
     
     
     Coh.par$obsL3[j] = mean(filter(data_growth, Age == 3)$Length,na.rm = T)
     Coh.par$obsL3_lcl[j] = as.numeric(quantile(filter(data_growth, 
     Age == 3)$Length,c(0.025,0.975))[1])
      Coh.par$obsL3_ucl[j] = as.numeric(quantile(filter(data_growth, 
      Age == 3)$Length,c(0.025,0.975))[2]) 
 
 
 
 
 
synth.list.3$coh.par = Coh.par

}

########################## Cohort

if((linf_var == "Cohort" | k_var  == "Cohort") & file.exists("m_grow3.std") == TRUE ) {

nrow.df = length(unique(data_growth$Cohort))

Coh.par = data.frame(Cohort = rep(0,nrow.df),
 linf = rep(0,nrow.df),
 linf_lcl = rep(0,nrow.df),
 linf_ucl = rep(0,nrow.df),
 k = rep(0,nrow.df),
  k_lcl = rep(0,nrow.df),
   k_ucl = rep(0,nrow.df),
 t0 = rep(0,nrow.df),
 t0_lcl = rep(0,nrow.df),
 t0_ucl = rep(0,nrow.df),
 Samples = rep(0,nrow.df),
 expL1 = rep(0,nrow.df),
 expL2 = rep(0,nrow.df),
 expL3 = rep(0,nrow.df),
 obsL1 = rep(0,nrow.df),
 obsL1_lcl = rep(0,nrow.df),
 obsL1_ucl = rep(0,nrow.df),
 obsL2 = rep(0,nrow.df),
 obsL2_lcl = rep(0,nrow.df),
 obsL2_ucl = rep(0,nrow.df),
 obsL3 = rep(0,nrow.df),
 obsL3_lcl = rep(0,nrow.df),
 obsL3_ucl = rep(0,nrow.df))
 
  Coh.par$Samples = as.numeric(table(data_growth$Cohort))
 
 klinf.pos = which(synth.list.3$std$name == "Linf_sd")
 k.pos = which(synth.list.3$std$name == "k_sd")
 t0.pos = which(synth.list.3$std$name == "t0_sd")
 
 
 #### Cohort
 
 Coh.par$Cohort = levels(data_growth$Cohort)
 
 
 ####### Linf
 Coh.par$linf = synth.list.3$std$value[klinf.pos]
 Coh.par$linf_lcl = synth.list.3$std$value[klinf.pos] - 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 Coh.par$linf_ucl = synth.list.3$std$value[klinf.pos] + 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 
 ######## k
 
 Coh.par$k = synth.list.3$std$value[k.pos]
 Coh.par$k_lcl = synth.list.3$std$value[k.pos] - 
 1.96*synth.list.3$std$std.dev[k.pos]
 Coh.par$k_ucl = synth.list.3$std$value[k.pos] + 
 1.96*synth.list.3$std$std.dev[k.pos]
 
 ######## t0
 
 Coh.par$t0 = synth.list.3$std$value[t0.pos]
 Coh.par$t0_lcl = synth.list.3$std$value[t0.pos] - 
 1.96*synth.list.3$std$std.dev[t0.pos]
 Coh.par$t0_ucl = synth.list.3$std$value[t0.pos] + 
 1.96*synth.list.3$std$std.dev[t0.pos]
 



#####Expected length at age 1,2,3,

  for (j in 1:nrow(Coh.par)) {
    
    
    plot.df = VBpred.f(Coh.par$linf[j],Coh.par$k[j],Coh.par$t0[j],2,maxage=10)
    
    Coh.par$expL1[j] = filter(plot.df, Age == 1)$Length
    Coh.par$expL2[j] = filter(plot.df, Age == 2)$Length
    Coh.par$expL3[j] = filter(plot.df, Age == 3)$Length
    
    
    Coh.par$obsL1[j] = mean(filter(data_growth, 
     Cohort == Coh.par$Cohort[j] & Age == 1)$Length,na.rm = T)
      Coh.par$obsL1_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Coh.par$Cohort[j] & Age == 1)$Length,c(0.025,0.975))[1])
      Coh.par$obsL1_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Coh.par$Cohort[j] & Age == 1)$Length,c(0.025,0.975))[2])
     
     Coh.par$obsL2[j] = mean(filter(data_growth, 
     Cohort == Coh.par$Cohort[j] & Age == 2)$Length,na.rm = T) 
     Coh.par$obsL2_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Coh.par$Cohort[j] & Age == 2)$Length,c(0.025,0.975))[1])
      Coh.par$obsL2_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Coh.par$Cohort[j] & Age == 2)$Length,c(0.025,0.975))[2])
     
     
     Coh.par$obsL3[j] = mean(filter(data_growth, 
     Cohort == Coh.par$Cohort[j] & Age == 3)$Length,na.rm = T)
     Coh.par$obsL3_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Coh.par$Cohort[j] & Age == 3)$Length,c(0.025,0.975))[1])
      Coh.par$obsL3_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Coh.par$Cohort[j] & Age == 3)$Length,c(0.025,0.975))[2]) 
    
    

					}
Coh.par$obsL1[which(is.nan(Coh.par$obsL1))] = NA
Coh.par$obsL2[which(is.nan(Coh.par$obsL2))] = NA
Coh.par$obsL3[which(is.nan(Coh.par$obsL3))] = NA


synth.list.3$coh.par = Coh.par

}




######### Here I add a dataframe with link, k, and to (lcl and ucl) per Flood.Coh

if((linf_var == "Flood.Coh" | k_var  == "Flood.Coh") & file.exists("m_grow3.std") == TRUE ) {


nrow.df = length(unique(data_growth$Flood.Coh))

Flood.Coh.par = data.frame(Cohort = rep(0,nrow.df),
 linf = rep(0,nrow.df),
 linf_lcl = rep(0,nrow.df),
 linf_ucl = rep(0,nrow.df),
 k = rep(0,nrow.df),
  k_lcl = rep(0,nrow.df),
   k_ucl = rep(0,nrow.df),
 t0 = rep(0,nrow.df),
 t0_lcl = rep(0,nrow.df),
 t0_ucl = rep(0,nrow.df),
 Samples = rep(0,nrow.df),
 expL1 = rep(0,nrow.df),
 expL2 = rep(0,nrow.df),
 expL3 = rep(0,nrow.df),
 obsL1 = rep(0,nrow.df),
 obsL1_lcl = rep(0,nrow.df),
 obsL1_ucl = rep(0,nrow.df),
 obsL2 = rep(0,nrow.df),
 obsL2_lcl = rep(0,nrow.df),
 obsL2_ucl = rep(0,nrow.df),
 obsL3 = rep(0,nrow.df),
 obsL3_lcl = rep(0,nrow.df),
 obsL3_ucl = rep(0,nrow.df))
 
 Flood.Coh.par$Samples = as.numeric(table(data_growth$Flood.Coh))
 
 klinf.pos = which(synth.list.3$std$name == "Linf_sd")
 k.pos = which(synth.list.3$std$name == "k_sd")
 t0.pos = which(synth.list.3$std$name == "t0_sd")
 
 
 #### Flood Cohort
 
 Flood.Coh.par$Cohort = levels(data_growth$Flood.Coh)
 
 
 ####### Linf
 Flood.Coh.par$linf = synth.list.3$std$value[klinf.pos]
 Flood.Coh.par$linf_lcl = synth.list.3$std$value[klinf.pos] - 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 Flood.Coh.par$linf_ucl = synth.list.3$std$value[klinf.pos] + 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 
 ######## k
 
 Flood.Coh.par$k = synth.list.3$std$value[k.pos]
 Flood.Coh.par$k_lcl = synth.list.3$std$value[k.pos] - 
 1.96*synth.list.3$std$std.dev[k.pos]
 Flood.Coh.par$k_ucl = synth.list.3$std$value[k.pos] + 
 1.96*synth.list.3$std$std.dev[k.pos]
 
 ######## t0
 
 Flood.Coh.par$t0 = synth.list.3$std$value[t0.pos]
 Flood.Coh.par$t0_lcl = synth.list.3$std$value[t0.pos] - 
 1.96*synth.list.3$std$std.dev[t0.pos]
 Flood.Coh.par$t0_ucl = synth.list.3$std$value[t0.pos] + 
 1.96*synth.list.3$std$std.dev[t0.pos]


######Expected length at age 1,2,3,

  for (j in 1:nrow(Flood.Coh.par)) {
    
    
    plot.df = VBpred.f(Flood.Coh.par$linf[j],Flood.Coh.par$k[j],Flood.Coh.par$t0[j],2,maxage=10)
    
    Flood.Coh.par$expL1[j] = filter(plot.df, Age == 1)$Length
    Flood.Coh.par$expL2[j] = filter(plot.df, Age == 2)$Length
    Flood.Coh.par$expL3[j] = filter(plot.df, Age == 3)$Length
    
     Flood.Coh.par$obsL1[j] = mean(filter(data_growth, 
     Flood.Coh == Flood.Coh.par$Cohort[j] & Age == 1)$Length,na.rm = T) 
     Flood.Coh.par$obsL1_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Flood.Coh == Flood.Coh.par$Cohort[j] & Age == 1)$Length,c(0.025,0.975))[1])
      Flood.Coh.par$obsL1_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Flood.Coh == Flood.Coh.par$Cohort[j] & Age == 1)$Length,c(0.025,0.975))[2])
     
     
     Flood.Coh.par$obsL2[j] = mean(filter(data_growth, 
     Flood.Coh == Flood.Coh.par$Cohort[j] & Age == 2)$Length,na.rm = T) 
     Flood.Coh.par$obsL2_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Flood.Coh == Flood.Coh.par$Cohort[j] & Age == 2)$Length,c(0.025,0.975))[1])
      Flood.Coh.par$obsL2_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Flood.Coh == Flood.Coh.par$Cohort[j] & Age == 2)$Length,c(0.025,0.975))[2])
     
     
     Flood.Coh.par$obsL3[j] = mean(filter(data_growth, 
     Flood.Coh == Flood.Coh.par$Cohort[j] & Age == 3)$Length,na.rm = T) 
     Flood.Coh.par$obsL3_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Flood.Coh == Flood.Coh.par$Cohort[j] & Age == 3)$Length,c(0.025,0.975))[1])
      Flood.Coh.par$obsL3_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Flood.Coh == Flood.Coh.par$Cohort[j] & Age == 3)$Length,c(0.025,0.975))[2])

					}

Flood.Coh.par$obsL1[which(is.nan(Flood.Coh.par$obsL1))] = NA
Flood.Coh.par$obsL2[which(is.nan(Flood.Coh.par$obsL2))] = NA
Flood.Coh.par$obsL3[which(is.nan(Flood.Coh.par$obsL3))] = NA					

synth.list.3$coh.par = Flood.Coh.par



}

########### Sex

####### Here I add a dataframe with link, k, and to (lcl and ucl) per cohort

if((linf_var == "Sex" | k_var  == "Sex") & file.exists("m_grow3.std") == TRUE ) {

nrow.df = length(unique(data_growth$Sex_gen))

Sex.par = data.frame(Cohort = rep(0,nrow.df),
Sex = rep(0,nrow.df),
 linf = rep(0,nrow.df),
 linf_lcl = rep(0,nrow.df),
 linf_ucl = rep(0,nrow.df),
 k = rep(0,nrow.df),
  k_lcl = rep(0,nrow.df),
   k_ucl = rep(0,nrow.df),
 t0 = rep(0,nrow.df),
 t0_lcl = rep(0,nrow.df),
 t0_ucl = rep(0,nrow.df),
 Samples = rep(0,nrow.df),
 expL1 = rep(0,nrow.df),
 expL2 = rep(0,nrow.df),
 expL3 = rep(0,nrow.df),
 obsL1 = rep(0,nrow.df),
 obsL1_lcl = rep(0,nrow.df),
 obsL1_ucl = rep(0,nrow.df),
 obsL2 = rep(0,nrow.df),
 obsL2_lcl = rep(0,nrow.df),
 obsL2_ucl = rep(0,nrow.df),
 obsL3 = rep(0,nrow.df),
 obsL3_lcl = rep(0,nrow.df),
 obsL3_ucl = rep(0,nrow.df))
 
 Sex.par$Samples = as.numeric(table(data_growth$Sex_gen))
 
 klinf.pos = which(synth.list.3$std$name == "Linf_sd")
 k.pos = which(synth.list.3$std$name == "k_sd")
 t0.pos = which(synth.list.3$std$name == "t0_sd")
 
 
 #### Sex levels
 
 Sex.par$Sex = levels(data_growth$Sex_gen)
 
 
 ####### Linf
 Sex.par$linf = synth.list.3$std$value[klinf.pos]
 Sex.par$linf_lcl = synth.list.3$std$value[klinf.pos] - 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 Sex.par$linf_ucl = synth.list.3$std$value[klinf.pos] + 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 
 ######## k
 
 Sex.par$k = synth.list.3$std$value[k.pos]
 Sex.par$k_lcl = synth.list.3$std$value[k.pos] - 
 1.96*synth.list.3$std$std.dev[k.pos]
 Sex.par$k_ucl = synth.list.3$std$value[k.pos] + 
 1.96*synth.list.3$std$std.dev[k.pos]
 
 ######## t0
 
 Sex.par$t0 = synth.list.3$std$value[t0.pos]
 Sex.par$t0_lcl = synth.list.3$std$value[t0.pos] - 
 1.96*synth.list.3$std$std.dev[t0.pos]
 Sex.par$t0_ucl = synth.list.3$std$value[t0.pos] + 
 1.96*synth.list.3$std$std.dev[t0.pos]


######Expected length at age 1,2,3,

  for (j in 1:nrow(Sex.par)) {
    
    
    plot.df = VBpred.f(Sex.par$linf[j],Sex.par$k[j],Sex.par$t0[j],2,maxage=10)
    
    Sex.par$expL1[j] = filter(plot.df, Age == 1)$Length
    Sex.par$expL2[j] = filter(plot.df, Age == 2)$Length
    Sex.par$expL3[j] = filter(plot.df, Age == 3)$Length
    
     Sex.par$obsL1[j] = mean(filter(data_growth, 
     Sex_gen == Sex.par$Sex[j] & Age == 1)$Length,na.rm = T) 
     Sex.par$obsL1_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Sex_gen == Sex.par$Sex[j] & Age == 1)$Length,c(0.025,0.975))[1])
      Sex.par$obsL1_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Sex_gen == Sex.par$Sex[j] & Age == 1)$Length,c(0.025,0.975))[2])
     
     
     Sex.par$obsL2[j] = mean(filter(data_growth, 
     Sex_gen == Sex.par$Sex[j] & Age == 2)$Length,na.rm = T) 
     Sex.par$obsL2_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Sex_gen == Sex.par$Sex[j] & Age == 2)$Length,c(0.025,0.975))[1])
      Sex.par$obsL2_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Sex_gen == Sex.par$Sex[j] & Age == 2)$Length,c(0.025,0.975))[2])
     
     
     Sex.par$obsL3[j] = mean(filter(data_growth, 
     Sex_gen == Sex.par$Sex[j] & Age == 3)$Length,na.rm = T) 
     Sex.par$obsL3_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Sex_gen == Sex.par$Sex[j] & Age == 3)$Length,c(0.025,0.975))[1])
      Sex.par$obsL3_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Sex_gen == Sex.par$Sex[j] & Age == 3)$Length,c(0.025,0.975))[2])

					}

Sex.par$obsL1[which(is.nan(Sex.par$obsL1))] = NA
Sex.par$obsL2[which(is.nan(Sex.par$obsL2))] = NA
Sex.par$obsL3[which(is.nan(Sex.par$obsL3))] = NA					

synth.list.3$coh.par = Sex.par



}


########################## Sex + Cohort

if((linf_var == "Sex + Cohort" | k_var  == "Sex + Cohort") & file.exists("m_grow3.std") == TRUE ) {

nrow.df = length(unique(data_growth$Cohort))*2

Sex.Coh.par = data.frame(Cohort = rep(0,nrow.df),
Sex = c(rep(levels(data_growth$Sex_gen)[1],nrow.df/2),
rep(levels(data_growth$Sex_gen)[2],nrow.df/2)),
 linf = rep(0,nrow.df),
 linf_lcl = rep(0,nrow.df),
 linf_ucl = rep(0,nrow.df),
 k = rep(0,nrow.df),
  k_lcl = rep(0,nrow.df),
   k_ucl = rep(0,nrow.df),
 t0 = rep(0,nrow.df),
 t0_lcl = rep(0,nrow.df),
 t0_ucl = rep(0,nrow.df),
 Samples = rep(0,nrow.df),
 expL1 = rep(0,nrow.df),
 expL2 = rep(0,nrow.df),
 expL3 = rep(0,nrow.df),
 obsL1 = rep(0,nrow.df),
 obsL1_lcl = rep(0,nrow.df),
 obsL1_ucl = rep(0,nrow.df),
 obsL2 = rep(0,nrow.df),
 obsL2_lcl = rep(0,nrow.df),
 obsL2_ucl = rep(0,nrow.df),
 obsL3 = rep(0,nrow.df),
 obsL3_lcl = rep(0,nrow.df),
 obsL3_ucl = rep(0,nrow.df))
 
  Coh.par$Samples = as.numeric(table(data_growth$Cohort))
 
 klinf.pos = which(synth.list.3$std$name == "Linf_sd")
 k.pos = which(synth.list.3$std$name == "k_sd")
 t0.pos = which(synth.list.3$std$name == "t0_sd")
 
 
 #### Cohort
 
 Sex.Coh.par$Cohort = rep(levels(data_growth$Cohort),2)
 
 
 ####### Linf
 Sex.Coh.par$linf = synth.list.3$std$value[klinf.pos]
 Sex.Coh.par$linf_lcl = synth.list.3$std$value[klinf.pos] - 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 Sex.Coh.par$linf_ucl = synth.list.3$std$value[klinf.pos] + 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 
 ######## k
 
 Sex.Coh.par$k = synth.list.3$std$value[k.pos]
 Sex.Coh.par$k_lcl = synth.list.3$std$value[k.pos] - 
 1.96*synth.list.3$std$std.dev[k.pos]
 Sex.Coh.par$k_ucl = synth.list.3$std$value[k.pos] + 
 1.96*synth.list.3$std$std.dev[k.pos]
 
 ######## t0
 
 Sex.Coh.par$t0 = synth.list.3$std$value[t0.pos]
 Sex.Coh.par$t0_lcl = synth.list.3$std$value[t0.pos] - 
 1.96*synth.list.3$std$std.dev[t0.pos]
 Sex.Coh.par$t0_ucl = synth.list.3$std$value[t0.pos] + 
 1.96*synth.list.3$std$std.dev[t0.pos]
 



#####Expected length at age 1,2,3,

  for (j in 1:nrow(Sex.Coh.par)) {
    
    
    plot.df = VBpred.f(Sex.Coh.par$linf[j],Sex.Coh.par$k[j],Sex.Coh.par$t0[j],2,maxage=10)
    
    Sex.Coh.par$expL1[j] = filter(plot.df, Age == 1)$Length
    Sex.Coh.par$expL2[j] = filter(plot.df, Age == 2)$Length
    Sex.Coh.par$expL3[j] = filter(plot.df, Age == 3)$Length
    
    
    Sex.Coh.par$obsL1[j] = mean(filter(data_growth, 
     Cohort == Sex.Coh.par$Cohort[j] & Sex_gen == Sex.Coh.par$Sex[j] & Age == 1)$Length,na.rm = T)
      Sex.Coh.par$obsL1_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Sex.Coh.par$Cohort[j] & Sex_gen == Sex.Coh.par$Sex[j] & Age == 1)$Length,c(0.025,0.975))[1])
      Sex.Coh.par$obsL1_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Sex.Coh.par$Cohort[j] & Sex_gen == Sex.Coh.par$Sex[j] & Age == 1)$Length,c(0.025,0.975))[2])
     
     Sex.Coh.par$obsL2[j] = mean(filter(data_growth, 
     Cohort == Sex.Coh.par$Cohort[j] & Sex_gen == Sex.Coh.par$Sex[j] & Age == 2)$Length,na.rm = T) 
     Sex.Coh.par$obsL2_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Sex.Coh.par$Cohort[j] & Sex_gen == Sex.Coh.par$Sex[j] & Age == 2)$Length,c(0.025,0.975))[1])
      Sex.Coh.par$obsL2_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Sex.Coh.par$Cohort[j] & Sex_gen == Sex.Coh.par$Sex[j] & Age == 2)$Length,c(0.025,0.975))[2])
     
     
     Sex.Coh.par$obsL3[j] = mean(filter(data_growth, 
     Cohort == Sex.Coh.par$Cohort[j] & Sex_gen == Sex.Coh.par$Sex[j] & Age == 3)$Length,na.rm = T)
     Sex.Coh.par$obsL3_lcl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Sex.Coh.par$Cohort[j] & Sex_gen == Sex.Coh.par$Sex[j] & Age == 3)$Length,c(0.025,0.975))[1])
      Sex.Coh.par$obsL3_ucl[j] = as.numeric(quantile(filter(data_growth, 
	Cohort == Sex.Coh.par$Cohort[j] & Sex_gen == Sex.Coh.par$Sex[j] & Age == 3)$Length,c(0.025,0.975))[2]) 
    
    

					}
Sex.Coh.par$obsL1[which(is.nan(Sex.Coh.par$obsL1))] = NA
Sex.Coh.par$obsL2[which(is.nan(Sex.Coh.par$obsL2))] = NA
Sex.Coh.par$obsL3[which(is.nan(Sex.Coh.par$obsL3))] = NA


Sex.Coh.par = dplyr::arrange(Sex.Coh.par, Cohort, Sex)


synth.list.3$coh.par = Sex.Coh.par
}




########################## Pop + Cohort

if((linf_var == "Pop + Cohort" | k_var  == "Pop + Cohort") & file.exists("m_grow3.std") == TRUE ) {
  
  nrow.df = length(unique(data_growth$Cohort))*2
  
  Pop.Coh.par = data.frame(Cohort = rep(0,nrow.df),
                           Pop = c(rep(levels(data_growth$Pop)[1],nrow.df/2),
                                   rep(levels(data_growth$Pop)[2],nrow.df/2)),
                           linf = rep(0,nrow.df),
                           linf_lcl = rep(0,nrow.df),
                           linf_ucl = rep(0,nrow.df),
                           k = rep(0,nrow.df),
                           k_lcl = rep(0,nrow.df),
                           k_ucl = rep(0,nrow.df),
                           t0 = rep(0,nrow.df),
                           t0_lcl = rep(0,nrow.df),
                           t0_ucl = rep(0,nrow.df),
                           Samples = rep(0,nrow.df),
                           expL1 = rep(0,nrow.df),
                           expL2 = rep(0,nrow.df),
                           expL3 = rep(0,nrow.df),
                           obsL1 = rep(0,nrow.df),
                           obsL1_lcl = rep(0,nrow.df),
                           obsL1_ucl = rep(0,nrow.df),
                           obsL2 = rep(0,nrow.df),
                           obsL2_lcl = rep(0,nrow.df),
                           obsL2_ucl = rep(0,nrow.df),
                           obsL3 = rep(0,nrow.df),
                           obsL3_lcl = rep(0,nrow.df),
                           obsL3_ucl = rep(0,nrow.df))
  
  Pop.Coh.par$Samples = c(as.numeric(table(data_growth$Pop,data_growth$Cohort)[1,]),
                                 as.numeric(table(data_growth$Pop,data_growth$Cohort)[2,]))
  
  klinf.pos = which(synth.list.3$std$name == "Linf_sd")
  k.pos = which(synth.list.3$std$name == "k_sd")
  t0.pos = which(synth.list.3$std$name == "t0_sd")
  
  
  #### Cohort
  
  Pop.Coh.par$Cohort = rep(levels(data_growth$Cohort),2)
  
  
  ####### Linf
  Pop.Coh.par$linf = synth.list.3$std$value[klinf.pos]
  Pop.Coh.par$linf_lcl = synth.list.3$std$value[klinf.pos] - 
    1.96*synth.list.3$std$std.dev[klinf.pos]
  Pop.Coh.par$linf_ucl = synth.list.3$std$value[klinf.pos] + 
    1.96*synth.list.3$std$std.dev[klinf.pos]
  
  ######## k
  
  Pop.Coh.par$k = synth.list.3$std$value[k.pos]
  Pop.Coh.par$k_lcl = synth.list.3$std$value[k.pos] - 
    1.96*synth.list.3$std$std.dev[k.pos]
  Pop.Coh.par$k_ucl = synth.list.3$std$value[k.pos] + 
    1.96*synth.list.3$std$std.dev[k.pos]
  
  ######## t0
  
  Pop.Coh.par$t0 = synth.list.3$std$value[t0.pos]
  Pop.Coh.par$t0_lcl = synth.list.3$std$value[t0.pos] - 
    1.96*synth.list.3$std$std.dev[t0.pos]
  Pop.Coh.par$t0_ucl = synth.list.3$std$value[t0.pos] + 
    1.96*synth.list.3$std$std.dev[t0.pos]
  
  
  
  
  #####Expected length at age 1,2,3,
  
  for (j in 1:nrow(Pop.Coh.par)) {
    
    
    plot.df = VBpred.f(Pop.Coh.par$linf[j],Pop.Coh.par$k[j],Pop.Coh.par$t0[j],2,maxage=10)
    
    Pop.Coh.par$expL1[j] = filter(plot.df, Age == 1)$Length
    Pop.Coh.par$expL2[j] = filter(plot.df, Age == 2)$Length
    Pop.Coh.par$expL3[j] = filter(plot.df, Age == 3)$Length
    
    
    Pop.Coh.par$obsL1[j] = mean(filter(data_growth, 
                                       Cohort == Pop.Coh.par$Cohort[j] & Pop == Pop.Coh.par$Pop[j] & Age == 1)$Length,na.rm = T)
    Pop.Coh.par$obsL1_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                          Cohort == Pop.Coh.par$Cohort[j] & Pop == Pop.Coh.par$Pop[j] & Age == 1)$Length,c(0.025,0.975))[1])
    Pop.Coh.par$obsL1_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                          Cohort == Pop.Coh.par$Cohort[j] & Pop == Pop.Coh.par$Pop[j] & Age == 1)$Length,c(0.025,0.975))[2])
    
    Pop.Coh.par$obsL2[j] = mean(filter(data_growth, 
                                       Cohort == Pop.Coh.par$Cohort[j] & Pop == Pop.Coh.par$Pop[j] & Age == 2)$Length,na.rm = T) 
    Pop.Coh.par$obsL2_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                          Cohort == Pop.Coh.par$Cohort[j] & Pop == Pop.Coh.par$Pop[j] & Age == 2)$Length,c(0.025,0.975))[1])
    Pop.Coh.par$obsL2_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                          Cohort == Pop.Coh.par$Cohort[j] & Pop == Pop.Coh.par$Pop[j] & Age == 2)$Length,c(0.025,0.975))[2])
    
    
    Pop.Coh.par$obsL3[j] = mean(filter(data_growth, 
                                       Cohort == Pop.Coh.par$Cohort[j] & Pop == Pop.Coh.par$Pop[j] & Age == 3)$Length,na.rm = T)
    Pop.Coh.par$obsL3_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                          Cohort == Pop.Coh.par$Cohort[j] & Pop == Pop.Coh.par$Pop[j] & Age == 3)$Length,c(0.025,0.975))[1])
    Pop.Coh.par$obsL3_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                          Cohort == Pop.Coh.par$Cohort[j] & Pop == Pop.Coh.par$Pop[j] & Age == 3)$Length,c(0.025,0.975))[2]) 
    
    
    
  }
  Pop.Coh.par$obsL1[which(is.nan(Pop.Coh.par$obsL1))] = NA
  Pop.Coh.par$obsL2[which(is.nan(Pop.Coh.par$obsL2))] = NA
  Pop.Coh.par$obsL3[which(is.nan(Pop.Coh.par$obsL3))] = NA
  
  
  Pop.Coh.par = dplyr::arrange(Pop.Coh.par, Cohort, Pop)
  
  
  synth.list.3$coh.par = Pop.Coh.par
}


######### Here I add a dataframe with link, k, and to (lcl and ucl) per Flood.Coh

if((linf_var == "P.Coh" | k_var  == "P.Coh") & file.exists("m_grow3.std") == TRUE ) {


nrow.df = length(unique(data_growth$P.Coh))

P.Coh.par = data.frame(Cohort = rep(0,nrow.df),
 linf = rep(0,nrow.df),
 linf_lcl = rep(0,nrow.df),
 linf_ucl = rep(0,nrow.df),
 k = rep(0,nrow.df),
  k_lcl = rep(0,nrow.df),
   k_ucl = rep(0,nrow.df),
 t0 = rep(0,nrow.df),
 t0_lcl = rep(0,nrow.df),
 t0_ucl = rep(0,nrow.df),
 Samples = rep(0,nrow.df),
 expL1 = rep(0,nrow.df),
 expL2 = rep(0,nrow.df),
 expL3 = rep(0,nrow.df),
 obsL1 = rep(0,nrow.df),
 obsL1_lcl = rep(0,nrow.df),
 obsL1_ucl = rep(0,nrow.df),
 obsL2 = rep(0,nrow.df),
 obsL2_lcl = rep(0,nrow.df),
 obsL2_ucl = rep(0,nrow.df),
 obsL3 = rep(0,nrow.df),
 obsL3_lcl = rep(0,nrow.df),
 obsL3_ucl = rep(0,nrow.df))
 
 P.Coh.par$Samples = as.numeric(table(data_growth$P.Coh))
 
 klinf.pos = which(synth.list.3$std$name == "Linf_sd")
 k.pos = which(synth.list.3$std$name == "k_sd")
 t0.pos = which(synth.list.3$std$name == "t0_sd")
 
 
 #### Parental and Other Cohorts
 
 P.Coh.par$Cohort = levels(data_growth$P.Coh)
 
 
 ####### Linf
 P.Coh.par$linf = synth.list.3$std$value[klinf.pos]
 P.Coh.par$linf_lcl = synth.list.3$std$value[klinf.pos] - 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 P.Coh.par$linf_ucl = synth.list.3$std$value[klinf.pos] + 
 1.96*synth.list.3$std$std.dev[klinf.pos]
 
 ######## k
 
 P.Coh.par$k = synth.list.3$std$value[k.pos]
 P.Coh.par$k_lcl = synth.list.3$std$value[k.pos] - 
 1.96*synth.list.3$std$std.dev[k.pos]
 P.Coh.par$k_ucl = synth.list.3$std$value[k.pos] + 
 1.96*synth.list.3$std$std.dev[k.pos]
 
 ######## t0
 
 P.Coh.par$t0 = synth.list.3$std$value[t0.pos]
 P.Coh.par$t0_lcl = synth.list.3$std$value[t0.pos] - 
 1.96*synth.list.3$std$std.dev[t0.pos]
 P.Coh.par$t0_ucl = synth.list.3$std$value[t0.pos] + 
 1.96*synth.list.3$std$std.dev[t0.pos]


######Expected length at age 1,2,3,

  for (j in 1:nrow(P.Coh.par)) {
    
    
    plot.df = VBpred.f(P.Coh.par$linf[j],P.Coh.par$k[j],P.Coh.par$t0[j],2,maxage=10)
    
    P.Coh.par$expL1[j] = filter(plot.df, Age == 1)$Length
    P.Coh.par$expL2[j] = filter(plot.df, Age == 2)$Length
    P.Coh.par$expL3[j] = filter(plot.df, Age == 3)$Length
    
     P.Coh.par$obsL1[j] = mean(filter(data_growth, 
     P.Coh == P.Coh.par$Cohort[j] & Age == 1)$Length,na.rm = T) 
     P.Coh.par$obsL1_lcl[j] = as.numeric(quantile(filter(data_growth, 
	P.Coh == P.Coh.par$Cohort[j] & Age == 1)$Length,c(0.025,0.975))[1])
      P.Coh.par$obsL1_ucl[j] = as.numeric(quantile(filter(data_growth, 
	P.Coh == P.Coh.par$Cohort[j] & Age == 1)$Length,c(0.025,0.975))[2])
     
     
     P.Coh.par$obsL2[j] = mean(filter(data_growth, 
     P.Coh == P.Coh.par$Cohort[j] & Age == 2)$Length,na.rm = T) 
     P.Coh.par$obsL2_lcl[j] = as.numeric(quantile(filter(data_growth, 
	P.Coh == P.Coh.par$Cohort[j] & Age == 2)$Length,c(0.025,0.975))[1])
      P.Coh.par$obsL2_ucl[j] = as.numeric(quantile(filter(data_growth, 
	P.Coh == P.Coh.par$Cohort[j] & Age == 2)$Length,c(0.025,0.975))[2])
     
     
     P.Coh.par$obsL3[j] = mean(filter(data_growth, 
     P.Coh == P.Coh.par$Cohort[j] & Age == 3)$Length,na.rm = T) 
     P.Coh.par$obsL3_lcl[j] = as.numeric(quantile(filter(data_growth, 
	P.Coh == P.Coh.par$Cohort[j] & Age == 3)$Length,c(0.025,0.975))[1])
      P.Coh.par$obsL3_ucl[j] = as.numeric(quantile(filter(data_growth, 
	P.Coh == P.Coh.par$Cohort[j] & Age == 3)$Length,c(0.025,0.975))[2])

					}

P.Coh.par$obsL1[which(is.nan(P.Coh.par$obsL1))] = NA
P.Coh.par$obsL2[which(is.nan(P.Coh.par$obsL2))] = NA
P.Coh.par$obsL3[which(is.nan(P.Coh.par$obsL3))] = NA					

synth.list.3$coh.par = P.Coh.par



}


######### Here I add a dataframe with link, k, and to (lcl and ucl) per Sector

if((linf_var == "Sector" | k_var  == "Sector") & file.exists("m_grow3.std") == TRUE ) {
  
  
  nrow.df = length(unique(data_growth$Sector))
  
  Sector.par = data.frame(Cohort = rep(0,nrow.df),
                          Sector = rep(0,nrow.df),
                         linf = rep(0,nrow.df),
                         linf_lcl = rep(0,nrow.df),
                         linf_ucl = rep(0,nrow.df),
                         k = rep(0,nrow.df),
                         k_lcl = rep(0,nrow.df),
                         k_ucl = rep(0,nrow.df),
                         t0 = rep(0,nrow.df),
                         t0_lcl = rep(0,nrow.df),
                         t0_ucl = rep(0,nrow.df),
                         Samples = rep(0,nrow.df),
                         expL1 = rep(0,nrow.df),
                         expL2 = rep(0,nrow.df),
                         expL3 = rep(0,nrow.df),
                         obsL1 = rep(0,nrow.df),
                         obsL1_lcl = rep(0,nrow.df),
                         obsL1_ucl = rep(0,nrow.df),
                         obsL2 = rep(0,nrow.df),
                         obsL2_lcl = rep(0,nrow.df),
                         obsL2_ucl = rep(0,nrow.df),
                         obsL3 = rep(0,nrow.df),
                         obsL3_lcl = rep(0,nrow.df),
                         obsL3_ucl = rep(0,nrow.df))
  
  Sector.par$Samples = as.numeric(table(data_growth$Sector))
  
  klinf.pos = which(synth.list.3$std$name == "Linf_sd")
  k.pos = which(synth.list.3$std$name == "k_sd")
  t0.pos = which(synth.list.3$std$name == "t0_sd")
  
  
  #### Parental and Other Cohorts
  
  Sector.par$Sector= levels(data_growth$Sector)
  
  
  ####### Linf
  Sector.par$linf = synth.list.3$std$value[klinf.pos]
  Sector.par$linf_lcl = synth.list.3$std$value[klinf.pos] - 
    1.96*synth.list.3$std$std.dev[klinf.pos]
  Sector.par$linf_ucl = synth.list.3$std$value[klinf.pos] + 
    1.96*synth.list.3$std$std.dev[klinf.pos]
  
  ######## k
  
  Sector.par$k = synth.list.3$std$value[k.pos]
  Sector.par$k_lcl = synth.list.3$std$value[k.pos] - 
    1.96*synth.list.3$std$std.dev[k.pos]
  Sector.par$k_ucl = synth.list.3$std$value[k.pos] + 
    1.96*synth.list.3$std$std.dev[k.pos]
  
  ######## t0
  
  Sector.par$t0 = synth.list.3$std$value[t0.pos]
  Sector.par$t0_lcl = synth.list.3$std$value[t0.pos] - 
    1.96*synth.list.3$std$std.dev[t0.pos]
  Sector.par$t0_ucl = synth.list.3$std$value[t0.pos] + 
    1.96*synth.list.3$std$std.dev[t0.pos]
  
  
  ######Expected length at age 1,2,3,
  
  for (j in 1:nrow(Sector.par)) {
    
    
    plot.df = VBpred.f(Sector.par$linf[j],Sector.par$k[j],Sector.par$t0[j],2,maxage=10)
    
    Sector.par$expL1[j] = filter(plot.df, Age == 1)$Length
    Sector.par$expL2[j] = filter(plot.df, Age == 2)$Length
    Sector.par$expL3[j] = filter(plot.df, Age == 3)$Length
    
    Sector.par$obsL1[j] = mean(filter(data_growth, 
                                     Sector == Sector.par$Sector[j] & Age == 1)$Length,na.rm = T) 
    Sector.par$obsL1_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Sector == Sector.par$Sector[j] & Age == 1)$Length,c(0.025,0.975))[1])
    Sector.par$obsL1_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Sector == Sector.par$Sector[j] & Age == 1)$Length,c(0.025,0.975))[2])
    
    
    Sector.par$obsL2[j] = mean(filter(data_growth, 
                                      Sector == Sector.par$Sector[j] & Age == 2)$Length,na.rm = T) 
    Sector.par$obsL2_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Sector == Sector.par$Sector[j] & Age == 2)$Length,c(0.025,0.975))[1])
    Sector.par$obsL2_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Sector == Sector.par$Sector[j] & Age == 2)$Length,c(0.025,0.975))[2])
    
    
    Sector.par$obsL3[j] = mean(filter(data_growth, 
                                      Sector == Sector.par$Sector[j] & Age == 3)$Length,na.rm = T) 
    Sector.par$obsL3_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Sector == Sector.par$Sector[j] & Age == 3)$Length,c(0.025,0.975))[1])
    Sector.par$obsL3_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Sector == Sector.par$Sector[j] & Age == 3)$Length,c(0.025,0.975))[2])
    
  }
  
  Sector.par$obsL1[which(is.nan(Sector.par$obsL1))] = NA
  Sector.par$obsL2[which(is.nan(Sector.par$obsL2))] = NA
  Sector.par$obsL3[which(is.nan(Sector.par$obsL3))] = NA					
  
  synth.list.3$coh.par = Sector.par
  
  
  
}



######### Here I add a dataframe with link, k, and to (lcl and ucl) per Pop

if((linf_var == "Pop" | k_var  == "Pop") & file.exists("m_grow3.std") == TRUE ) {
  
  
  nrow.df = length(unique(data_growth$Pop))
  
  Pop.par = data.frame(Cohort = rep(0,nrow.df),
                          Pop = rep(0,nrow.df),
                          linf = rep(0,nrow.df),
                          linf_lcl = rep(0,nrow.df),
                          linf_ucl = rep(0,nrow.df),
                          k = rep(0,nrow.df),
                          k_lcl = rep(0,nrow.df),
                          k_ucl = rep(0,nrow.df),
                          t0 = rep(0,nrow.df),
                          t0_lcl = rep(0,nrow.df),
                          t0_ucl = rep(0,nrow.df),
                          Samples = rep(0,nrow.df),
                          expL1 = rep(0,nrow.df),
                          expL2 = rep(0,nrow.df),
                          expL3 = rep(0,nrow.df),
                          obsL1 = rep(0,nrow.df),
                          obsL1_lcl = rep(0,nrow.df),
                          obsL1_ucl = rep(0,nrow.df),
                          obsL2 = rep(0,nrow.df),
                          obsL2_lcl = rep(0,nrow.df),
                          obsL2_ucl = rep(0,nrow.df),
                          obsL3 = rep(0,nrow.df),
                          obsL3_lcl = rep(0,nrow.df),
                          obsL3_ucl = rep(0,nrow.df))
  
  Pop.par$Samples = as.numeric(table(data_growth$Pop))
  
  klinf.pos = which(synth.list.3$std$name == "Linf_sd")
  k.pos = which(synth.list.3$std$name == "k_sd")
  t0.pos = which(synth.list.3$std$name == "t0_sd")
  
  
  #### Parental and Other Cohorts
  
  Pop.par$Pop= levels(data_growth$Pop)
  
  
  ####### Linf
  Pop.par$linf = synth.list.3$std$value[klinf.pos]
  Pop.par$linf_lcl = synth.list.3$std$value[klinf.pos] - 
    1.96*synth.list.3$std$std.dev[klinf.pos]
  Pop.par$linf_ucl = synth.list.3$std$value[klinf.pos] + 
    1.96*synth.list.3$std$std.dev[klinf.pos]
  
  ######## k
  
  Pop.par$k = synth.list.3$std$value[k.pos]
  Pop.par$k_lcl = synth.list.3$std$value[k.pos] - 
    1.96*synth.list.3$std$std.dev[k.pos]
  Pop.par$k_ucl = synth.list.3$std$value[k.pos] + 
    1.96*synth.list.3$std$std.dev[k.pos]
  
  ######## t0
  
  Pop.par$t0 = synth.list.3$std$value[t0.pos]
  Pop.par$t0_lcl = synth.list.3$std$value[t0.pos] - 
    1.96*synth.list.3$std$std.dev[t0.pos]
  Pop.par$t0_ucl = synth.list.3$std$value[t0.pos] + 
    1.96*synth.list.3$std$std.dev[t0.pos]
  
  
  ######Expected length at age 1,2,3,
  
  for (j in 1:nrow(Pop.par)) {
    
    
    plot.df = VBpred.f(Pop.par$linf[j],Pop.par$k[j],Pop.par$t0[j],2,maxage=10)
    
    Pop.par$expL1[j] = filter(plot.df, Age == 1)$Length
    Pop.par$expL2[j] = filter(plot.df, Age == 2)$Length
    Pop.par$expL3[j] = filter(plot.df, Age == 3)$Length
    
    Pop.par$obsL1[j] = mean(filter(data_growth, 
                                      Pop == Pop.par$Pop[j] & Age == 1)$Length,na.rm = T) 
    Pop.par$obsL1_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Pop == Pop.par$Pop[j] & Age == 1)$Length,c(0.025,0.975))[1])
    Pop.par$obsL1_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Pop == Pop.par$Pop[j] & Age == 1)$Length,c(0.025,0.975))[2])
    
    
    Pop.par$obsL2[j] = mean(filter(data_growth, 
                                      Pop == Pop.par$Pop[j] & Age == 2)$Length,na.rm = T) 
    Pop.par$obsL2_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Pop == Pop.par$Pop[j] & Age == 2)$Length,c(0.025,0.975))[1])
    Pop.par$obsL2_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Pop == Pop.par$Pop[j] & Age == 2)$Length,c(0.025,0.975))[2])
    
    
    Pop.par$obsL3[j] = mean(filter(data_growth, 
                                      Pop == Pop.par$Pop[j] & Age == 3)$Length,na.rm = T) 
    Pop.par$obsL3_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Pop == Pop.par$Pop[j] & Age == 3)$Length,c(0.025,0.975))[1])
    Pop.par$obsL3_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                         Pop == Pop.par$Pop[j] & Age == 3)$Length,c(0.025,0.975))[2])
    
  }
  
  Pop.par$obsL1[which(is.nan(Pop.par$obsL1))] = NA
  Pop.par$obsL2[which(is.nan(Pop.par$obsL2))] = NA
  Pop.par$obsL3[which(is.nan(Pop.par$obsL3))] = NA					
  
  synth.list.3$coh.par = Pop.par
  
  
  
}


######### Here I add a dataframe with link, k, and to (lcl and ucl) per Species

if((linf_var == "Species" | k_var  == "Species") & file.exists("m_grow3.std") == TRUE ) {
  
  
  nrow.df = length(unique(data_growth$Species))
  
  Species.par = data.frame(Cohort = rep(0,nrow.df),
                       Species = rep(0,nrow.df),
                       linf = rep(0,nrow.df),
                       linf_lcl = rep(0,nrow.df),
                       linf_ucl = rep(0,nrow.df),
                       k = rep(0,nrow.df),
                       k_lcl = rep(0,nrow.df),
                       k_ucl = rep(0,nrow.df),
                       t0 = rep(0,nrow.df),
                       t0_lcl = rep(0,nrow.df),
                       t0_ucl = rep(0,nrow.df),
                       Samples = rep(0,nrow.df),
                       expL1 = rep(0,nrow.df),
                       expL2 = rep(0,nrow.df),
                       expL3 = rep(0,nrow.df),
                       obsL1 = rep(0,nrow.df),
                       obsL1_lcl = rep(0,nrow.df),
                       obsL1_ucl = rep(0,nrow.df),
                       obsL2 = rep(0,nrow.df),
                       obsL2_lcl = rep(0,nrow.df),
                       obsL2_ucl = rep(0,nrow.df),
                       obsL3 = rep(0,nrow.df),
                       obsL3_lcl = rep(0,nrow.df),
                       obsL3_ucl = rep(0,nrow.df))
  
  Species.par$Samples = as.numeric(table(data_growth$Species))
  
  klinf.pos = which(synth.list.3$std$name == "Linf_sd")
  k.pos = which(synth.list.3$std$name == "k_sd")
  t0.pos = which(synth.list.3$std$name == "t0_sd")
  
  
  #### Parental and Other Cohorts
  
  Species.par$Species= levels(data_growth$Species)
  
  
  ####### Linf
  Species.par$linf = synth.list.3$std$value[klinf.pos]
  Species.par$linf_lcl = synth.list.3$std$value[klinf.pos] - 
    1.96*synth.list.3$std$std.dev[klinf.pos]
  Species.par$linf_ucl = synth.list.3$std$value[klinf.pos] + 
    1.96*synth.list.3$std$std.dev[klinf.pos]
  
  ######## k
  
  Species.par$k = synth.list.3$std$value[k.pos]
  Species.par$k_lcl = synth.list.3$std$value[k.pos] - 
    1.96*synth.list.3$std$std.dev[k.pos]
  Species.par$k_ucl = synth.list.3$std$value[k.pos] + 
    1.96*synth.list.3$std$std.dev[k.pos]
  
  ######## t0
  
  Species.par$t0 = synth.list.3$std$value[t0.pos]
  Species.par$t0_lcl = synth.list.3$std$value[t0.pos] - 
    1.96*synth.list.3$std$std.dev[t0.pos]
  Species.par$t0_ucl = synth.list.3$std$value[t0.pos] + 
    1.96*synth.list.3$std$std.dev[t0.pos]
  
  
  ######Expected length at age 1,2,3,
  
  for (j in 1:nrow(Species.par)) {
    
    
    plot.df = VBpred.f(Species.par$linf[j],Species.par$k[j],Species.par$t0[j],2,maxage=10)
    
    Species.par$expL1[j] = filter(plot.df, Age == 1)$Length
    Species.par$expL2[j] = filter(plot.df, Age == 2)$Length
    Species.par$expL3[j] = filter(plot.df, Age == 3)$Length
    
    Species.par$obsL1[j] = mean(filter(data_growth, 
                                   Species == Species.par$Species[j] & Age == 1)$Length,na.rm = T) 
    Species.par$obsL1_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                      Species == Species.par$Species[j] & Age == 1)$Length,c(0.025,0.975))[1])
    Species.par$obsL1_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                      Species == Species.par$Species[j] & Age == 1)$Length,c(0.025,0.975))[2])
    
    
    Species.par$obsL2[j] = mean(filter(data_growth, 
                                   Species == Species.par$Species[j] & Age == 2)$Length,na.rm = T) 
    Species.par$obsL2_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                      Species == Species.par$Species[j] & Age == 2)$Length,c(0.025,0.975))[1])
    Species.par$obsL2_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                      Species == Species.par$Species[j] & Age == 2)$Length,c(0.025,0.975))[2])
    
    
    Species.par$obsL3[j] = mean(filter(data_growth, 
                                   Species == Species.par$Species[j] & Age == 3)$Length,na.rm = T) 
    Species.par$obsL3_lcl[j] = as.numeric(quantile(filter(data_growth, 
                                                      Species == Species.par$Species[j] & Age == 3)$Length,c(0.025,0.975))[1])
    Species.par$obsL3_ucl[j] = as.numeric(quantile(filter(data_growth, 
                                                      Species == Species.par$Species[j] & Age == 3)$Length,c(0.025,0.975))[2])
    
  }
  
  Species.par$obsL1[which(is.nan(Species.par$obsL1))] = NA
  Species.par$obsL2[which(is.nan(Species.par$obsL2))] = NA
  Species.par$obsL3[which(is.nan(Species.par$obsL3))] = NA					
  
  synth.list.3$coh.par = Species.par
  
  
  
}






