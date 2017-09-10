require(MASS)

VB.f = function (Linf,k,t0,Age) {
  
  Length_age = Linf*(1-exp(-k*(Age-t0)))
  
  return(Length_age)
}


predictNLS <- function(
  object, 
  newdata, 
  nsim = 1000,
  ...
)
{
  require(MASS, quietly = TRUE)
  
  ## get right-hand side of formula
  RHS <- as.list(object$call$formula)[[3]]
  EXPR <- as.expression(RHS)
  
  ## all variables in model
  VARS <- all.vars(EXPR)
  
  ## coefficients
  COEF <- coef(object)
  
  ## extract predictor variable    
  predNAME <- setdiff(VARS, names(COEF))  
  
  ## take fitted values, if 'newdata' is missing
  if (missing(newdata)) {
    newdata <- eval(object$data)[predNAME]
    colnames(newdata) <- predNAME
  }
  
  ## check that 'newdata' has same name as predVAR
  if (names(newdata)[1] != predNAME) stop("newdata should have name '", predNAME, "'!")
  
  ## get parameter coefficients
  COEF <- coef(object)
  
  ## get variance-covariance matrix
  VCOV <- vcov(object)
  
  ## augment variance-covariance matrix for 'mvrnorm' 
  ## by adding a column/row for 'error in x'
  NCOL <- ncol(VCOV)
  ADD1 <- c(rep(0, NCOL))
  ADD1 <- matrix(ADD1, ncol = 1)
  colnames(ADD1) <- predNAME
  VCOV <- cbind(VCOV, ADD1)
  ADD2 <- c(rep(0, NCOL + 1))
  ADD2 <- matrix(ADD2, nrow = 1)
  rownames(ADD2) <- predNAME
  VCOV <- rbind(VCOV, ADD2) 
  
  ## iterate over all entries in 'newdata' as in usual 'predict.' functions
  NR <- nrow(newdata)
  respVEC <- numeric(NR)
  seVEC <- numeric(NR)
  varPLACE <- ncol(VCOV)   
  
  ## define counter function
  counter <- function (i) 
  {
    if (i%%10 == 0) 
      cat(i)
    else cat(".")
    if (i%%50 == 0) 
      cat("\n")
    flush.console()
  }
  
  outMATnls <- NULL 
  
  for (i in 1:NR) {
    counter(i)
    
    ## get predictor values and optional errors
    predVAL <- newdata[i, 1]
    if (ncol(newdata) == 2) predERROR <- newdata[i, 2] else predERROR <- 0
    names(predVAL) <- predNAME  
    names(predERROR) <- predNAME  
    
    ## create mean vector for 'mvrnorm'
    MU <- c(COEF, predVAL)
    
    ## create variance-covariance matrix for 'mvrnorm'
    ## by putting error^2 in lower-right position of VCOV
    newVCOV <- VCOV
    newVCOV[varPLACE, varPLACE] <- predERROR^2
    
    ## create MC simulation matrix
    simMAT <- mvrnorm(n = nsim, mu = MU, Sigma = newVCOV, empirical = TRUE)
    
    ## evaluate expression on rows of simMAT
    EVAL <- try(eval(EXPR, envir = as.data.frame(simMAT)), silent = TRUE)
    if (inherits(EVAL, "try-error")) stop("There was an error evaluating the simulations!")
    
    ## collect statistics
    PRED <- data.frame(predVAL)
    colnames(PRED) <- predNAME   
    FITTED <- predict(object, newdata = data.frame(PRED))
    MEAN.sim <- mean(EVAL, na.rm = TRUE)
    SD.sim <- sd(EVAL, na.rm = TRUE)
    MEDIAN.sim <- median(EVAL, na.rm = TRUE)
    MAD.sim <- mad(EVAL, na.rm = TRUE)
    QUANT <- quantile(EVAL, probs = c(0.025,0.975))
    RES <- c(as.numeric(predVAL),FITTED, MEAN.sim, SD.sim, MEDIAN.sim, MAD.sim, QUANT[1], QUANT[2])
    outMATnls <- rbind(outMATnls, RES)
  }
  
  colnames(outMATnls) <- c("Age","Fitted","Mean","Sd","Median","Mad","Quant_low","Quant_high")
  rownames(outMATnls) <- NULL
  
  cat("\n")
  
  return(as.data.frame(outMATnls))  
  
}



## Function to read AD Model Builder fit from its par and cor files.
## Use by:
## simple.fit <- read.admbFit('c:/admb/examples/simple')
## Then the object 'simple.fit' is a list containing sub-objects
# 'names', 'est', 'std', 'cor', and 'cov' for all model
# parameters and sdreport quantities.
#
read.admbFit<-function(file){
  ret<-list()
  # read par file
  parfile<-as.numeric(scan(paste(file,'.par', sep=''),
                           what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar<-as.integer(parfile[1])
  ret$nloglike<-parfile[2] #objective function value
  ret$maxgrad<-parfile[3]
  
  # read cor file
  file<-paste(file,'.cor', sep='')
  lin<-readLines(file)
  # total parameter including sdreport variables
  ret$totPar<-length(lin)-2
  #log of the determinant of the hessian
  ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin<-lapply(strsplit(lin[1:ret$totPar+2], ' '),function(x)x[x!=''])
  ret$names<-unlist(lapply(sublin,function(x)x[2]))
  ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))
  ret$cor<-matrix(NA, ret$totPar, ret$totPar)
  corvec<-unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)]))
  ret$cor[upper.tri(ret$cor, diag=TRUE)]<-as.numeric(corvec)
  ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)]
  # covariance matrix
  ret$cov<-ret$cor*(ret$std %o% ret$std)
  return(ret)
}


loidri.fit.nls = nls(Length ~ Linf.nls*(1-exp(-k.nls*(Age-t0.nls))),data = filter(allidri_df, Month == 9, Age >0, Pop == "LIdri_MT"),
                             start = list(Linf.nls = 300, k.nls = 0.5, t0.nls = 0.3))


rtidri.fit.nls = nls(Length ~ Linf.nls*(1-exp(-k.nls*(Age-t0.nls))),data = filter(allidri_df, Month == 9, Age >0, Pop == "LIdri_RT"),
              start = list(Linf.nls = 300, k.nls = 0.5, t0.nls = 0.3))

uppidri.fit.nls = nls(Length ~ Linf.nls*(1-exp(-k.nls*(Age-t0.nls))),data = filter(allidri_df, Month == 9, Age >0, Pop == "UIdri_MT"),
              start = list(Linf.nls = 300, k.nls = 0.5, t0.nls = 0.3))

uppidri.pred = data.frame(Fitted = predict(uppidri.fit.nls, newdata = data.frame(Age = 1:15))) %>%
  cbind(.,data.frame(Age = 1:15,Pop = rep("UIdri_MT",15)))

loidri.pred = data.frame(Fitted = predict(loidri.fit.nls, newdata = data.frame(Age = 1:15))) %>%
  cbind(.,data.frame(Age = 1:15,Pop = rep("LIdri_MT",15)))

rtidri.pred = data.frame(Fitted = predict(rtidri.fit.nls, newdata = data.frame(Age = 1:15))) %>%
  cbind(.,data.frame(Age = 1:15,Pop = rep("LIdri_RT",15)))

trout.nls.predict = rbind(uppidri.pred, loidri.pred, rtidri.pred)
