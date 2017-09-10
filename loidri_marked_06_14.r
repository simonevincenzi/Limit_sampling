library(marked)
library(R2admb)
library(splines)
library(tidyverse)

# delete some objects that have been potentially used by other scripts

rm(data_df)
rm(annual)
rm(data.marked)
rm(data.prov)

# filter data 

data_df_complete = arrange(loidri_df, Mark_cor, Year, Month) %>%
  filter(., !is.na(Mark_cor) & Mark_cor!="NA" & Cohort_cor!="C15" & Age_cor >=1 & !Mark_cor %in% c("BOTTLE","bottle","Dead") & Length >=115 & !grepl("9472A",Mark_cor) & !grepl("dead",Mark_cor))

# data_df_complete = arrange(uppidri_df, Mark_cor, Year, Month) %>%
#   filter(., !is.na(Mark_cor) & Mark_cor!="NA" & Cohort_cor!="C15" & Age_cor >=1 & !Mark_cor %in% c("BOTTLE","bottle","Dead") & Length >=115 & !grepl("9472A",Mark_cor) & !grepl("dead",Mark_cor))
# 
# data_df_complete = arrange(rtidri_df, Mark_cor, Year, Month) %>%
#   filter(., !is.na(Mark_cor), Age_cor >=1 , !Mark_cor %in% c("BOTTLE","bottle","Dead") , Length >=115, !grepl("9472A",Mark_cor), !grepl("dead",Mark_cor))

max_year_v = c(2006,2008,2010,2012,2014)
mod_list = list()
annual = 1

for (j_n in 1:length(max_year_v)) {
  
  
  max_year = max_year_v[j_n]
  
  ## first and last year
  
  minyear= 2004
  maxyear = max_year
  
  data_df = filter(data_df_complete, Year <= max_year)
  
  rangeofyears = minyear:maxyear
  
  ncolonne = (length(rangeofyears)*2)  # ncolonne corresponds to the number of sampling occasions
  
  # re-factor Cohorts
  
  Cohort.levels = c("C96","C97","C98","C99","C00","C01","C02","C03","C04","C05","C06","C07","C08","C09","C10","C11","C12","C13","C14")
  
  data_df$Cohort_cor <- factor(data_df$Cohort_cor, levels = Cohort.levels)
  
  # name of columns with format myyyy
  
  colonne.names = as.character(c(62004,92004,62005,92005,62006,92006,62007,92007,
                                 62008,92008,62009,92009,62010,92010,62011,92011,62012,92012,62013,92013,62014,92014,62015,92015))
  
  colonne.names = colonne.names[1:ncolonne]
  
  # prepare data for marked
  
  data.marked = as.data.frame(matrix(0,10000,(ncolonne+1)))
  
  colnames(data.marked) = c("id",colonne.names) #prepare data.frame for recapture data
  
  data.marked$initial.age = rep(0,nrow(data.marked)) # initial age
  
  data.marked$Coh = rep(0,nrow(data.marked)) # Cohort (number)
  
  data.marked$Coh_n = rep(0,nrow(data.marked)) # Cohort (character/factor)
  
  
  
  
  unique.mark.data = with(data_df,unique(Mark_cor))
  
  for (i in 1:length(unique.mark.data)) {
    
    data.prov = subset(data_df,Mark_cor == unique.mark.data[i])
    
    year.prov = data.prov$Year
    
    monthyear = paste(data.prov$Month,data.prov$Year,sep="")
    
    incl.year = colonne.names %in% monthyear
    
    data.marked[i,2:(ncolonne+1)] = ifelse(incl.year==F,0,1)
    
    
    if (annual == 0) {
      
      if (min(subset(data.prov, Year == min(data.prov$Year))$Month) <= 7) { 
        ### check whether the first capture was in June or September (use <= 7 to be sure)
        
        data.marked[i,"initial.age"] = min(data.prov$Age_cor) *12 ## we are considering months
        
      } else {data.marked[i,"initial.age"] = (min(data.prov$Age_cor) *12) + 3 }} else {
        
        if (min(subset(data.prov, Year == min(data.prov$Year))$Month) <= 7) { 
          ### check whether the first capture was in June or September (use <= 7 to be sure)
          
          data.marked[i,"initial.age"] = min(data.prov$Age_cor) *1 ## we are considering months
          
        } else {data.marked[i,"initial.age"] = (min(data.prov$Age_cor) *1) + 0.25 }
        
      }  ### initial.age is a reserved column name
    
    data.marked[i,"Coh"] = data.prov$Cohort_cor[1] ## Cohort of the fish as number
    
    data.marked[i,"Species"] = data.prov$Species[1]
    
    data.marked[i,"Pop"] = data.prov$Pop[1]
    
    data.marked[i,"Stream"] = data.prov$Stream[1]
    
    data.marked[i,"Coh_n"] = as.character(data.prov$Cohort_cor[1]) ## Cohort of the fish as character/factor 
    
    
    data.marked[i,"id"] = data.prov$Mark_cor[1] # Tag of fish
    
  }
  
  ### I join the columns with the 1/0 for sampled/not sampled
  
  data.marked = filter(data.marked, id!=0) #data.marked[1:sum(data.marked$id>0),]
  
  dataformerge = data.marked[,2:(ncolonne+1)]
  
  dataformerge$ch <- do.call(paste, c(dataformerge, sep=""))
  
  data.marked$ch = dataformerge$ch 
  
  data.marked = data.marked[,-c(2:(ncolonne+1))]
  
  colnames(data.marked)[1] = "Mark"
  
  data.marked$Coh_n <- factor(data.marked$Coh_n, levels = Cohort.levels)
  
  # space between sampling occasions with two options, annual or monthly
  
  if (annual == 0) {
    spacebwtime = c(3,rep(c(9,3),11))} else if (annual == 1) 
    {spacebwtime = c(0.25,rep(c(0.75,0.25),((ncolonne/2)-1)))} else if (annual == 2) {
      spacebwtime = rep(1,(ncolonne - 1))}
  
  
  # Add Season covariate
  
  season.rep = c(1,(rep(c(0,1),((ncolonne/2)-1))),0)  # 1 is from June to September, 0 from September to June
  
  # [1] 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
  
  season.rep = c("J-S",(rep(c("S-J","J-S"),((ncolonne/2)-1))),"S-J") # J is June, S is September
  
  Season=matrix(rep(season.rep,nrow(data.marked)),ncol=length(season.rep),byrow=T)
  
  col.spacebwtime = c(1,cumsum(spacebwtime)+1) # labels for the time-varying factor
  
  colnames(Season)=paste("Season",col.spacebwtime,sep="")
  
  data.marked=cbind(data.marked,Season)
  
  ### create the objects for marked
  
  design.Phi=list(static=c("Coh_n"),time.varying=c("Season")) 
  design.p=list(static=c("Coh_n"),time.varying=c("Season"))
  design.parameters=list(Phi=design.Phi,p=design.p)
  data.proc=process.data(data.marked, model = "CJS", time.intervals = spacebwtime)
  ddl=make.design.data(data.proc,parameters=design.parameters)
  
  
  # fit.models=function()
  # {
  #   p.dot=list(formula = ~1)
  #   p.Season = list(formula = ~ Season)
  #   p.Coh = list(formula = ~ Coh_n)
  #   p.time = list(formula = ~ time)
  #   p.Age = list(formula=~bs(Age))
  # 
  # 
  #   Phi.full = list(formula=~ Coh_n * time)
  # 
  #   ##
  #   cml=create.model.list(c("Phi","p"))
  #   results=crm.wrapper(cml,data=data.proc, ddl=ddl,
  #                       external=FALSE,accumulate=FALSE, hessian = T, use.admb = F)
  #   return(results)
  # }
  # test=fit.models()
  
  
  
  
  
  ### Fit survival models (bs is splines), best recapture model comes from previous work
  
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
    
    ##
    cml=create.model.list(c("Phi","p"))
    results=crm.wrapper(cml,data=data.proc, ddl=ddl,
                        external=FALSE,accumulate=FALSE, hessian = T, use.admb = F)
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
  
  mod_list[[j_n]] = list("mod_cost" = predict(test[[pos_best_const]], ddl = test.ddl, se = T)$Phi %>%  add_column(samples = nrow(data.marked), final_year = maxyear, tot_year = 1 + (final_year - minyear)), "min_sampl" = min_test_time, "max_sampl" = max_test_time, "mod" = test, "ddl" = test.ddl, "p_b_time" = pos_best_time)
}

loidri_surv_06_14.list = mod_list
saveRDS(loidri_surv_06_14.list,"loidri_surv_06_14.list.RDS")
