library(marked)

library(R2admb)

library(splines)

library(tidyverse)

# delete some objects that have been potentially used by other scripts

rm(data_df)
rm(annual)
rm(data.marked)
rm(data.prov)


uppvol.df = read.csv("uppvol_2015_complete.csv",header=T, # read dataset of the population from csv
                     stringsAsFactors = FALSE,na.strings ="")

data_df_complete = filter (uppvol.df,Cohort!="C15" & !is.na(Mark)) %>%  arrange(., Mark, Year, Month)


### C14 would be of 0+ 

max_year_v = c(2006,2008,2010,2012,2014)
mod_list = list()
annual = 1

for (j_n in 1:length(max_year_v)) {
  
  
  max_year = max_year_v[j_n]
  
  ## first and last year
  
  minyear= 2004
  maxyear = max_year
  
  rangeofyears = minyear:maxyear
  
  
  data_df = filter(data_df_complete, Year <= max_year)

ncolonne = (length(rangeofyears)*2) - 1  
#ncolonne corresponds to the number of sampling occasions. 21 in this case

## Cohort levels in order
Cohort.levels = c("C00","C01","C02","C03","C04",
                  "C05","C06","C07","C08","C09","C10","C11","C12","C13","C14")

data_df$Cohort <- factor(data_df$Cohort, levels = Cohort.levels) #Assign cohort levels

colonne.names = as.character(c(92004,62005,92005,62006,92006,62007,92007,
                               62008,92008,62009,92009,62010,92010,62011,92011,62012,92012,62013,92013,62014,92014,62015,92015))

colonne.names = colonne.names[1:ncolonne]

#colonne.names are the sampling occasions (format myyyy)

data.marked = as.data.frame(matrix(0,10000,(ncolonne+1))) #prepare data.frame for recapture data

colnames(data.marked) = c("id",colonne.names) ## add an id column

data.marked$initial.age = rep(0,nrow(data.marked))  # initial age 

data.marked$Coh = rep(0,nrow(data.marked)) # Cohort (number)

data.marked$Coh_n = rep(0,nrow(data.marked)) # Cohort (character/factor)


unique.mark.data = with(data_df,unique(Mark)) # unique tagged fish

for (i in 1:length(unique.mark.data)) {  #loop over unique tagged fish
  
  data.prov = subset(data_df,Mark == unique.mark.data[i]) ## data relative to the tagged fish
  
  year.prov = data.prov$Year  ## years in which the fish was sampled
  
  monthyear = paste(data.prov$Month,data.prov$Year,sep="") ## Month and Year in which the fish was sampled
  
  incl.year = colonne.names %in% monthyear  ### which colonne.names are in monthyear
  
  data.marked[i,2:(ncolonne+1)] = ifelse(incl.year==F,0,1) # for the tagged fish in the columns
  # of the dataframe I assign 0 if the fish was not sampled, 1 otherwise
  
  if (annual == 0) {
    
    if (min(subset(data.prov, Year == min(data.prov$Year))$Month) <= 7) { 
      ### check whether the first capture was in June or September (use <= 7 to be sure)
      
      data.marked[i,"initial.age"] = min(data.prov$Age) *12 ## we are considering months
      
    } else {data.marked[i,"initial.age"] = (min(data.prov$Age) *12) + 3 }} else {
      
      if (min(subset(data.prov, Year == min(data.prov$Year))$Month) <= 7) { 
        ### check whether the first capture was in June or September (use <= 7 to be sure)
        
        data.marked[i,"initial.age"] = min(data.prov$Age) *1 ## we are considering months
        
      } else {data.marked[i,"initial.age"] = (min(data.prov$Age) *1) + 0.25 }
      
    }  ### initial.age is a reserved column name
  
  data.marked[i,"Coh"] = data.prov$Cohort[1] ## Cohort of the fish as number
  
  data.marked[i,"Coh_n"] = as.character(data.prov$Cohort[1]) ## Cohort of the fish as character/factor 
  
  data.marked[i,"id"] = data.prov$Mark[1] # Tag of fish
  
}

data.marked = filter(data.marked, id!=0) #data.marked[1:sum(data.marked$id>0),]

dataformerge = data.marked[,2:(ncolonne+1)]

dataformerge$ch <- do.call(paste, c(dataformerge, sep=""))

data.marked$ch = dataformerge$ch 


data.marked = data.marked[,-c(2:(ncolonne+1))]


data.marked$Coh_n <- factor(data.marked$Coh_n, levels = Cohort.levels)


#Coh = data.marked$Coh

colnames(data.marked)[1] = "Mark"  #id is reserved	

#data.marked = assign.vb.par.f(marked.df = data.marked, vb.pred.df = vol0.synth$pred_vb)


if (annual == 0) {  ## if I compute monthly, time between sampling is 9 months, 3 months
  spacebwtime = c(rep(c(9,3),(ncolonne/2))) } else {spacebwtime = c(rep(c(0.75,0.25),(ncolonne/2)))}
## start with 9 or 0.7 because the first sampling was september


# Add Season covariate (0 is Sept to June, 1 is June to Sept)
season.rep = (c(0,(rep(c(1,0),(ncolonne/2)))))

Season=matrix(rep(season.rep,nrow(data.marked)),ncol=length(season.rep),byrow=T)

col.spacebwtime = c(1,cumsum(spacebwtime)+1)

colnames(Season)=paste("Season",col.spacebwtime,sep="")
# at the end of each column of Season I need a number starting from 1 indicating the 
# cumulative time


data.marked=cbind(data.marked,Season) # bind Mark, capture history and Season


#design.Phi=list(static=c("Coh","k","linf","expL3","expL2","expL1"),time.varying=c("Season"))
design.Phi=list(static=c("Coh_n"),time.varying=c("Season"))
design.p=list(static=c("Coh_n"),time.varying=c("Season"))
design.parameters=list(Phi=design.Phi,p=design.p)
data.proc=process.data(data.marked, model = "CJS", time.interval = spacebwtime)
ddl = make.design.data(data.proc,parameters=design.parameters)

fit.models=function()
{
  
  Phi.dot=list(formula = ~1)
  Phi.Season = list(formula = ~ Season)
  Phi.Coh. = list(formula = ~ Coh_n)
  Phi.time = list(formula = ~ time)
  
  
  p.dot=list(formula = ~1)
  p.Season = list(formula = ~ Season)
  p.Coh = list(formula = ~ Coh_n)
  p.time = list(formula = ~ time)
  p.Age = list(formula=~bs(Age))
  
  cml=create.model.list(c("Phi","p"))
  results=crm.wrapper(cml,data=data.proc, ddl=ddl,
                      external=FALSE,accumulate=FALSE, hessian = T)
  return(results)
}
test=fit.models() # marked object with fitted models, AIC etc
test.ddl = ddl # design data

## I find the position of the best models with Phi(~1) and Phi(~time)
model_v = gsub("\\(|\\)", "", as.character(test$model.table$model))
row_model_const = min(which(grepl("Phi~1", model_v)))
row_model_time = min(which(grepl("Phi~time", model_v)))
pos_best_const = as.numeric(rownames(test$model.table)[row_model_const])
pos_best_time = as.numeric(rownames(test$model.table)[row_model_time])
##

## extract the parameters of model with highest and lowest survival for sampling occasion
test_time = arrange(predict(test[[pos_best_time]], ddl = test.ddl, se = T)$Phi, estimate)


max_test_time = test_time[nrow(test_time),] %>%  add_column(samples = nrow(data.marked), final_year = maxyear, tot_year = 1 + (final_year - minyear))
cont = 1
while((max_test_time$ucl-max_test_time$lcl)>0.8 | (max_test_time$ucl-max_test_time$lcl)<0.02) {
  max_test_time = test_time[nrow(test_time)-cont,] %>%  add_column(samples = nrow(data.marked), final_year = maxyear, tot_year = 1 + (final_year - minyear))
  cont = cont + 1
}
min_test_time = test_time[1,] %>%  add_column(samples = nrow(data.marked), final_year = maxyear, tot_year = 1 + (final_year - minyear))
cont = 1
while((min_test_time$ucl-min_test_time$lcl)>0.8 | (min_test_time$ucl-min_test_time$lcl)<0.02) {
  min_test_time = test_time[(1+cont),] %>%  add_column(samples = nrow(data.marked), final_year = maxyear, tot_year = 1 + (final_year - minyear))
  cont = cont + 1
}

mod_list[[j_n]] = list("mod_cost" = predict(test[[pos_best_const]], ddl = test.ddl, se = T)$Phi %>%  add_column(samples = nrow(data.marked), final_year = maxyear, tot_year = 1 + (final_year - minyear)), "min_sampl" = min_test_time, "max_sampl" = max_test_time,  "mod" = test, "ddl" = test.ddl, "p_b_time" = pos_best_time)
}

uppvol_surv_06_14.list = mod_list
saveRDS(uppvol_surv_06_14.list,"uppvol_surv_06_14.list.RDS")
