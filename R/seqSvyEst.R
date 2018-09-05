#####Load necessary packages####
library(lme4)
library(ineq)
library(dplyr)
library(Hmisc)
library(stats)
library(reshape2)
library(rstanarm)
library(lubridate)

#####Functions####

#' mdl.updater performs Bayesian sequential update on prior model with new data (usually from mvAM)
#' used by seq.estimator.AR and seq.estimator.XS
#'
#' @param prior.mdl - prior model of class stan_lmer
#' @param mvam.DF - new data to update model
#' @param VIF - variance inflation factor, minimum value to inflate variance of prior estimates by
#' @param trainWithPrior - if FALSE all priors are set to be uninformative
#' @param prior.reg - controls regulurization prior for random effect prior, see 'decov' in rstanarm help
#' @param prior.scale - controls scale prior for random effect prior, see 'decov' in rstanarm help
#' 
#' @return stan_lmer model object with re-estimated parameters from prior.mdl with data from mvam.DF
#' @export
mdl.updater <- function(prior.mdl,mvam.DF,VIF=4,trainWithPrior=TRUE,prior.reg=1,prior.scale=2){
  
  #pull out formula
  fmla <- prior.mdl$formula
  
  if(trainWithPrior){
    
    ##pull out fixed effect priors
    coef.fix <- names(fixef(prior.mdl))
    prior.mu <- prior.mdl$coefficients[coef.fix]
    prior.std <- sqrt(diag(prior.mdl$covmat))[coef.fix]
    
    ##pull out random effect priors
    coef.rdm <- names(ranef(prior.mdl))
    #extract variance estimates of random effects
    vcov.rdm <- as.data.frame(VarCorr(prior.mdl),comp="Variance")
    vcov.rdm <- vcov.rdm[vcov.rdm$grp %in% coef.rdm,'vcov']
    #compute parameters, regularization and scale are provided by user
    prior.conc <- Lasym(vcov.rdm)#lorenz asymmetry coefficient sets the concentration parameter of the dirichlet prior
    prior.shape <- sqrt(sum(vcov.rdm))/prior.scale+1 #mode of gamma prior is square-root of trace
    
    #add prior if model does not include covariate for logFCS from baseline
    if(!('logFCS.prior' %in% coef.fix)){
      
      #add logFCS.prior to formula
      fmla <- update.formula(fmla,~.+logFCS.prior)
      #train lme4 model
      lmer.mdl <- lmer(formula=fmla,
                       data=mvam.DF,weights=SelectWt)
      
      #pull out and set prior
      indx <- which(names(fixef(lmer.mdl))=='logFCS.prior')
      prior.mu <- c(lmer.mdl@beta[indx],prior.mu)
      prior.std <- c(sqrt(diag(vcov(lmer.mdl))[indx]*4),prior.std)
      
    }
    
    #train posterior model on mvam data with above priors
    mdl.mvam.poster <- stan_lmer(formula=fmla,
                                 data=mvam.DF,weights=SelectWt,
                                 iter=3000,chains=4,cores=4,thin=2,control=list(adapt_delta=.999),QR=TRUE,
                                 prior = normal(prior.mu[2:length(prior.mu)],prior.std[2:length(prior.std)]*VIF,FALSE),
                                 prior_intercept = normal(prior.mu[1],prior.std*VIF),
                                 decov(regularization = prior.reg, concentration = prior.conc, 
                                       shape = prior.shape, scale = prior.scale))
    
  } else {
    
    #possible to ignore altogether
    mdl.mvam.poster <- stan_lmer(formula=fmla,
                                 data=mvam.DF,weights=SelectWt,
                                 iter=3000,chains=4,cores=4,thin=2,control=list(adapt_delta=.999),QR=TRUE)
    
  }
  
  #return posterior model
  return(mdl.mvam.poster)
  
  #another model (called the offset model) is used to adjust for mode effect (survey methodology) differences
  #typically this is estimated only once, but can aslo be estimated sequentially
  # if(estimateOffset){
  #   #calculate residual of new data with prior model
  #   mvam.DF$logFCS.diff <- with(mvam.DF,log(FCS)-logFCS.prior)
  #   #modify formula to to set LHS to residual
  #   fmla <- update.formula(fmla,logFCS.diff ~ .-logFCS.prior)
  #   #estimate model
  #   mdl.mvam.offset <- stan_lmer(formula=fmla,
  #                                data=mvam.DF,weights=SelectWt,
  #                                iter=3000,chains=4,cores=4,thin=2,control=list(adapt_delta=.999),QR=TRUE,
  #                                prior_intercept = normal(prior.mdl$meanShift,sqrt(prior.mdl$shift.var/2)))
  #   
  #   #primary covariate we care about is the mean-shift (due to mode effect)
  #   mdl.mvam.poster$meanShift <- -mdl.mvam.offset$coefficients[1]
  #   mdl.mvam.poster$shift.var <- mdl.mvam.offset$covmat[1]
  #   
  #   return(c(list(mdl.mvam.poster),list(mdl.mvam.offset)))
  #   
  # } else {
  #   return(mdl.mvam.poster)
  # }
}


#' seq.estimator.AR windows over dataset performing bayesian sequential update each time
#' model is autoregressive thus using logFCS results from previous window in RHS 
#' 
#' @param obs.DF - dataset to window over, usually mVAM data
#' @param prior.mdl - starting model to use
#' @param offset.mdl - results from offset.mdl are added to results from posterior model to account for mode effect
#' @param date.col - date column in obs.DF
#' @param week.step - window step size in weeks
#' @param window.size - window size in days
#' @param week.offset - which week to start bayesian sequential updates on
#'
#' @return list object of stan_lmer models, one model per window
#' @export
seq.estimator.AR <- function(obs.DF,prior.mdl,offset.mdl,date.col,week.step=1,window.size=30,week.offset=0){

  # Function creates sliding window over dataset and sequentially updates starting model; inputs:
  # obs.DF - dataset to window over, usually mVAM data
  # prior.mdl - starting model to use
  # offset.mdl - results from offset.mdl are added to results from posterior model to account for mode effect
  # date.col - date column in obs.DF
  # week.step - window step size in weeks
  # window.size - window size in days
  # week.offset - which week to start bayesian sequential updates on
  # NOTE: this function is for an autoregressive model for logFCS at time t, that is at each window
  # the model that estimates logFCS uses the results of the previous window's model on the current window
  # those results are then used in the RHS for the current model
  # RETURNS: list object of rStanARM models, one model per window

  #sim.DF$logFCS.prior <- log(sim.DF$FCS)
  #rslt.mtx <- matrix(NA,nrow = nrow(sim.DF),ncol = length(wks))
  
  #weeks in dataset
  wks <- week(date.col)
  wks <- seq(min(wks)+week.offset,max(wks)-round(window.size/7),by=week.step)
  
  #function to calculate logFCS from prior model
  #calculate logFCS from prior model
  calc.logFCS.prior <- function(df,mdl.stan){
    
    #because of the face efsna data didnt cover Sa'ada and Al Maharah
    #we assume Sa'ada=Hajjah and Al Maharah="Hadramaut for the purpose of the prior.mdl
    if(!('Al Maharah' %in% mdl.stan$data$ADM1_NAME)){
      df$ADM1_NAME <- as.factor(df$ADM1_NAME)
      df$ADM1_NAME_ACTUAL <- df$ADM1_NAME
      df$ADM1_NAME <- droplevels(revalue(df$ADM1_NAME,c("Sa'ada"="Hajjah","Al Maharah"="Hadramaut")))
    }
    
    df$logFCS.prior <- rowMeans(t(posterior_predict(mdl.stan,newdata=df,draws=500)))
    
    if('FCS' %in% colnames(df)) {
      df$logFCS.diff <- with(df,log(FCS)-logFCS.prior)
    }
    
    if(!('Al Maharah' %in% mdl.stan$data$ADM1_NAME)){
      df$ADM1_NAME <- df$ADM1_NAME_ACTUAL
      df$ADM1_NAME_ACTUAL <- NULL
    }
    
    return(df)
  }
  
  #estimate offset for observations
  obs.DF$logOffset <- rowMeans(t(posterior_predict(offset.mdl,newdata=obs.DF,draws=500)))
  
  #container to store results
  rslt.mdl <- vector("list",length(wks)+1)
  rslt.mdl[[1]] <- prior.mdl
  
  for(i in c(1:length(wks))){
    #filter dataset for weeks spanning window
    strDate <- ymd("2018-01-01") + weeks(wks[i]-1)
    endDate <- strDate+window.size
    indx.wndw <- which(date.col>=strDate & date.col<=endDate)
    print(paste('modeling',strDate,'to',endDate))
    
    #caclulate log FCS from prior model
    obs.DF <- calc.logFCS.prior(obs.DF,prior.mdl)
    obs.DF$logFCS.prior <- obs.DF$logFCS.prior-obs.DF$logOffset
    
    #calculate variance inflation factor
    VIF <- max(4,nrow(prior.mdl$data)/nrow(obs.DF[indx.wndw,]))
    
    #produce model
    rslt.mdl[[i+1]] <- mdl.updater(prior.mdl,obs.DF[indx.wndw,],VIF=VIF)
    
    #clean up and save
    rslt.mdl[[i+1]]$start.date <- strDate
    rslt.mdl[[i+1]]$end.date <- endDate
    save(rslt.mdl,file='/media/pato/DATA/Dev-VAM/BayesianExps/YmnSeqEst.RData')
    
    #sim.DF$logFCS.prior <- rowMeans(t(posterior_predict(new.mdl,newdata=sim.DF,draws=500))+new.mdl$meanShift)
    #rslt.mtx[,i] <- sim.DF$logFCS.prior
    
    prior.mdl <- rslt.mdl[[i+1]]
  }
  browser()
  return(rslt.mdl)
}

#' seq.estimator.AR windows over dataset performing bayesian sequential update each time
#' model is cross-sectional using logFCS results only from the original prior.mdl in the RHS
#'
#' @param obs.DF - dataset to window over, usually mVAM data
#' @param prior.mdl - to calculate logFCS at baseline
#' @param offset.mdl - results from offset.mdl form the starting priors when updating prior.mdl
#' @param date.col - date column in obs.DF
#' @param week.step - window step size in weeks
#' @param window.size - window size in days
#' @param week.offset - which week to start bayesian sequential updates on
#'
#' @return list object of stan_lmer models, one model per window
#' @export
seq.estimator.XS <- function(obs.DF,prior.mdl,offset.mdl,date.col,week.step=1,window.size=30,week.offset=0){

  # Function creates sliding window over dataset and sequentially updates starting model; inputs:
  # obs.DF - dataset to window over, usually mVAM data
  # prior.mdl - starting model to use
  # offset.mdl - results from offset.mdl are added to results from posterior model to account for mode effect
  # date.col - date column in obs.DF
  # week.step - window step size in weeks
  # window.size - window size in days
  # week.offset - which week to start bayesian sequential updates on
  # NOTE: this function is for a cross-sectional model for logFCS at time t, that is at each window
  # the model that estimates logFCS uses the logFCS results of the prior.mdl on the current window
  # those results are then used in the RHS for the current model. Consequently for the initial window at t=0
  # the priors are set to be uninformative
  # RETURNS: list object of rStanARM models, one model per window
  
  #sim.DF$logFCS.prior <- log(sim.DF$FCS)
  #rslt.mtx <- matrix(NA,nrow = nrow(sim.DF),ncol = length(wks))
  
  #weeks in dataset
  wks <- week(date.col)
  wks <- seq(min(wks)+week.offset,max(wks)-round(window.size/7),by=week.step)
  
  #calculate logFCS from prior model
  calc.logFCS.prior <- function(df,mdl.stan){
    
    #because of the face efsna data didnt cover Sa'ada and Al Maharah
    #we assume Sa'ada=Hajjah and Al Maharah="Hadramaut for the purpose of the prior.mdl
    if(!('Al Maharah' %in% mdl.stan$data$ADM1_NAME)){
      df$ADM1_NAME <- as.factor(df$ADM1_NAME)
      df$ADM1_NAME_ACTUAL <- df$ADM1_NAME
      df$ADM1_NAME <- droplevels(revalue(df$ADM1_NAME,c("Sa'ada"="Hajjah","Al Maharah"="Hadramaut")))
    }
    
    df$logFCS.prior <- rowMeans(t(posterior_predict(mdl.stan,newdata=df,draws=500)))
    
    if('FCS' %in% colnames(df)) {
      df$logFCS.diff <- with(df,log(FCS)-logFCS.prior)
    }
    
    if(!('Al Maharah' %in% mdl.stan$data$ADM1_NAME)){
      df$ADM1_NAME <- df$ADM1_NAME_ACTUAL
      df$ADM1_NAME_ACTUAL <- NULL
    }
    
    return(df)
  }
  obs.DF <- calc.logFCS.prior(obs.DF,prior.mdl)
  
  #container to store results
  rslt.mdl <- vector("list",length(wks)+1)
  rslt.mdl[[1]] <- prior.mdl
  
  for(i in c(1:length(wks))){
    #filter dataset for weeks spanning window
    strDate <- ymd("2018-01-01") + weeks(wks[i]-1)
    endDate <- strDate+window.size
    indx.wndw <- which(date.col>=strDate & date.col<=endDate)
    print(paste('modeling',strDate,'to',endDate))
    
    #calculate variance inflation factor
    VIF <- max(4,nrow(prior.mdl$data)/nrow(obs.DF[indx.wndw,]))
    
    #produce model
    if(!('logFCS.prior' %in% names(fixef(prior.mdl)))){ #this executes only at first iteration
      offset.mdl$formula <- prior.mdl$formula
      rslt.mdl[[i+1]] <- mdl.updater(offset.mdl,obs.DF[indx.wndw,],VIF=VIF)#,trainWithPrior=FALSE)
    } else {
      rslt.mdl[[i+1]] <- mdl.updater(prior.mdl,obs.DF[indx.wndw,],VIF=VIF)
    }
    
    #clean up and save
    rslt.mdl[[i+1]]$start.date <- strDate
    rslt.mdl[[i+1]]$end.date <- endDate
    save(rslt.mdl,file='C:/Users/gaurav.singhal/Desktop/ymnSeqMdl/ymnSeqMdl.RData')
    
    #sim.DF$logFCS.prior <- rowMeans(t(posterior_predict(new.mdl,newdata=sim.DF,draws=500))+new.mdl$meanShift)
    #rslt.mtx[,i] <- sim.DF$logFCS.prior
    
    prior.mdl <- rslt.mdl[[i+1]]
  }
  return(rslt.mdl)
}

#' mdl.simulator takes model objects from seq.estimator, simulates posteriors on the given data and returns results
#'
#' @param seq.mdls - rStanARM model collection returned from either seq.estimator.AR or .XS
#' @param offset.mdl - results from offset.mdl are added to results from posterior model to account for mode effect
#' @param sim.DF - dataset to predict posteriors on for each model in sim.DF
#' @param strata.col - column in sim.DF that signifies strata
#' @param weight.col - column in sim.DF that signifies observation weights
#' @param n.sim - number of times to simulate
#' @param AR - is model autoregressive (TRUE) or cross-sectional (FALSE)
#'
#' @return list object of 3 dataframes
# str.est - mean FCS,  %Poor, %Borderline with 95% Confidence intervals for each strata and window
# qnt.est - FCS for 0.5% tp 99.5% quantiles for each strata and window
# pdf.est - probability distribution function point estimates for each value of FCS from 10-112 for
#           each strata and window
#' @export

mdl.simulator <- function(seq.mdls,offset.mdl,sim.DF,strata.col,weight.col,n.sim=500,AR=FALSE){

  # seq.mdls - rStanARM model collection returned from either seq.estimator.AR or .XS
  # offset.mdl - results from offset.mdl are added to results from posterior model to account for mode effect
  # sim.DF - dataset to predict posteriors on for each model in sim.DF
  # strata.col - column in sim.DF that signifies strata
  # weight.col - column in sim.DF that signifies observation weights
  # n.sim - number of times to simulate
  # AR - is model autoregressive (TRUE) or cross-sectional (FALSE)
  # RETURNS: list object of 3 dataframes
  # str.est - mean FCS,  %Poor, %Borderline with 95% Confidence intervals for each strata and window
  # qnt.est - FCS for 0.5% tp 99.5% quantiles for each strata and window
  # pdf.est - probability distribution function point estimates for each value of FCS from 10-112 for
  #           each strata and window

  #make sure strata col is character not factor
  sim.DF[,strata.col] <- as.character(sim.DF[,strata.col])
  
  #first model is seq.mdls is base.mdl -> separata
  base.mdl <- seq.mdls[[1]]
  seq.mdls <- seq.mdls[c(2:length(seq.mdls))]
  
  #calculate logFCS from prior model
  calc.logFCS.prior <- function(df,mdl.stan){
    
    #because of the face efsna data didnt cover Sa'ada and Al Maharah
    #we assume Sa'ada=Hajjah and Al Maharah="Hadramaut for the purpose of the prior.mdl
    if(!('Al Maharah' %in% mdl.stan$data$ADM1_NAME)){
      df$ADM1_NAME <- as.factor(df$ADM1_NAME)
      df$ADM1_NAME_ACTUAL <- df$ADM1_NAME
      df$ADM1_NAME <- droplevels(revalue(df$ADM1_NAME,c("Sa'ada"="Hajjah","Al Maharah"="Hadramaut")))
    }
    
    df$logFCS.prior <- rowMeans(t(posterior_predict(mdl.stan,newdata=df,draws=500)))
    
    if('FCS' %in% colnames(df)) {
      df$logFCS.diff <- with(df,log(FCS)-logFCS.prior)
    }
    
    if(!('Al Maharah' %in% mdl.stan$data$ADM1_NAME)){
      df$ADM1_NAME <- df$ADM1_NAME_ACTUAL
      df$ADM1_NAME_ACTUAL <- NULL
    }
    
    return(df)
  }
  
  #either take log of FCS is already in sim.DF
  if('FCS' %in% colnames(sim.DF)){
    sim.DF$logFCS.prior <- log(sim.DF$FCS)
  } else { 
    # else use prior.mdl to calculate initial FCS for sim.DF
    sim.DF <- calc.logFCS.prior(sim.DF,base.mdl)
  }
  
  #calculate offsets from offset model
  sim.offsets.mtx <- t(posterior_predict(offset.mdl,newdata=sim.DF,draws=n.sim))
  
  #Create dataframes to store strata estimates
  n.strata <- length(unique(sim.DF[,strata.col]))+1
  n.tSteps <- length(seq.mdls)
  str.est.DF <- as.data.frame(matrix(nrow=n.strata*n.tSteps,ncol=12))
  colnames(str.est.DF) <- c(strata.col,'Start.Date','End.Date','FCSMean','CI.Hi.FCS','CI.Lo.FCS',
                            'PctPoor','CI.Hi.Poor','CI.Lo.Poor','PctBrdr','CI.Hi.Brdr','CI.Lo.Brdr')
  #str.est.DF[,strata.col] <- rep(unique(sim.DF[,strata.col]),n.tSteps)
  sim.est.DF <- cbind(sim.DF[,c(strata.col,weight.col)],
                      as.data.frame(matrix(nrow=nrow(sim.DF),ncol=n.tSteps)))
  
  #function to compute strata summary statistics from simulated posterior predictions
  est.strata <- function(df,wgt.col,n.sims){
    #cast as dataframe
    df <- as.data.frame(df)
    #compute weights
    wgt <- df[,wgt.col]/sum(df[,wgt.col])
    #column indices containing simulations from posterior
    indx <- c((dim(df)[2]-n.sims+1):dim(df)[2])
    
    #compute mean, $poor, %borderline
    mean.sim <- apply(df[,indx],2,function(x) weighted.mean(x,wgt))
    poor.sim <- apply(df[,indx],2,function(x) weighted.mean(x<=28,wgt))
    brdr.sim <- apply(df[,indx],2,function(x) weighted.mean(x<=42,wgt))
    
    #compute confidence intervals for mean and prevalences
    rslt <- as.data.frame(matrix(nrow=1,ncol=9))
    colnames(rslt) <- c('Mean.FCS','CI.Hi.FCS','CI.Lo.FCS',
                        'Pct.Poor','CI.Hi.Poor','CI.Lo.Poor',
                        'Pct.Brdr','CI.Hi.Brdr','CI.Lo.Brdr')
    rslt[1,] <- c(mean(mean.sim),quantile(mean.sim,0.975),quantile(mean.sim,0.025),
                  mean(poor.sim),quantile(poor.sim,0.975),quantile(poor.sim,0.025),
                  mean(brdr.sim),quantile(brdr.sim,0.975),quantile(brdr.sim,0.025))
    
    return(rslt)
  }
  
  for (i in c(1:n.tSteps)){
    #filling in start and end dates or strata summary table
    indx <- c(1:n.strata)+n.strata*(i-1)
    str.est.DF$Start.Date[indx] <- seq.mdls[[i]]$start.date
    str.est.DF$End.Date[indx] <- seq.mdls[[i]]$end.date
    
    #simulate predictions from posterior
    sim.mtx <- t(posterior_predict(seq.mdls[[i]],newdata=sim.DF,draws=n.sim))-sim.offsets.mtx
    #update prior logFCS <- this is what makes it autoregressive
    if(AR){sim.DF$logFCS.prior <- rowMeans(sim.mtx)}
    
    #place predictions into dataframe for post processing
    sim.est.DF[,i+2] <- exp(rowMeans(sim.mtx))
    sim.prd.DF <- cbind(sim.DF[,c(strata.col,weight.col)],exp(sim.mtx))
    rm(sim.mtx)
    
    #calculate strata level summary statistics
    str.est.DF[indx[c(1:(length(indx)-1))],c(1,c(4:12))] <- sim.prd.DF %>% group_by_(.dots=strata.col) %>% do(est.strata(.,weight.col,n.sim))
    sim.prd.DF$ADM1_NAME <- 'Yemen'
    str.est.DF[indx[length(indx)],c(1,c(4:12))] <- sim.prd.DF %>% group_by_(.dots=strata.col) %>% do(est.strata(.,weight.col,n.sim))
  }
  #cleanup dates
  str.est.DF$Start.Date <- as.Date(str.est.DF$Start.Date,'1970-01-01')
  str.est.DF$End.Date <- as.Date(str.est.DF$End.Date,'1970-01-01')
  
  #for simulated values turn to quantiles
  colnames(sim.est.DF)[c(3:(2+n.tSteps))] <- unique(str.est.DF$End.Date)
  sim.est.DF$HHID <- c(1:dim(sim.est.DF)[1])
  sim.est.DF <- melt(sim.est.DF,id.vars = c('ADM1_NAME','HHID','wgt'),variable.name='End.Date',value.name='FCS')
  sim.est.DF$End.Date <- as.Date(as.numeric(as.character(sim.est.DF$End.Date)),'1970-01-01')
  
  #function to compute quantiles from posterior simulations
  wgt.Qntl <- function(df,wgt.col){
    #normalize weights
    df[[wgt.col]] <- df[[wgt.col]]/sum(df[[wgt.col]])
    #compute FCS values at quantiles
    x <- wtd.quantile(df$FCS,df[[wgt.col]],probs=seq(0.005,0.995,by=0.005))
    #return
    rslt <- as.data.frame(matrix(nrow=1,ncol=length(x)))
    colnames(rslt) <- names(x)
    rslt[1,] <- x
    return(rslt)
  }
  #function to compute PDF from posterior simulations
  wgt.Dens <- function(df,wgt.col){
    #normalize weights
    df[[wgt.col]] <- df[[wgt.col]]/sum(df[[wgt.col]])
    #compute pdf
    dx <- density(df$FCS,weights=df[[wgt.col]],from=10,to=100)
    #interpolate probability at value for FCS
    pr <- approx(dx$x,dx$y,xout=seq(11,80,by=1))
    #return results
    rslt <- as.data.frame(matrix(nrow=1,ncol=length(pr$y)))
    colnames(rslt) <- pr$x
    rslt[1,] <- pr$y
    return(rslt)
  }
  #use dplyr to do compute PDF and quantiles
  qnt.DF <- sim.est.DF %>% group_by_(.dots=c(strata.col,'End.Date')) %>% do(wgt.Qntl(.,weight.col))
  pdf.DF <- sim.est.DF %>% group_by_(.dots=c(strata.col,'End.Date')) %>% do(wgt.Dens(.,weight.col))
  
  #assemble dataframes togethere and return
  sim.est.DF$ADM1_NAME <- 'Yemen'
  qnt.DF <- rbind(qnt.DF,
                  sim.est.DF %>% group_by_(.dots=c(strata.col,'End.Date')) %>% do(wgt.Qntl(.,weight.col)))
  pdf.DF <- rbind(pdf.DF,
                  sim.est.DF %>% group_by_(.dots=c(strata.col,'End.Date')) %>% do(wgt.Dens(.,weight.col)))
  
  
  return(list('str.est'=str.est.DF,'qnt.est'=qnt.DF,'pdf.est'=pdf.DF))
}