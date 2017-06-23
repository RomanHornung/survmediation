metestone <-
function(times, cens, expos, m, z=NULL, simul=TRUE, npvalsim=1e+5, riskincr, exposinflc=TRUE, minflc=TRUE) {

  # setwd("Z:/")
  # load("./Projects/Sander/MeEff/Data/amlcgcohort2withtcga_all.Rdata")
  # times <- pheno$timeos
  # cens <- pheno$status
  # expos <- pheno$runx1
  # m <- X[,30]
  # z <- list(sex=pheno$sex, age=pheno$age, cyto=pheno$cyto)
  # npvalsim=100
  # riskincr=TRUE
  # exposinflc=FALSE
  # minflc=FALSE
  
  #require("timereg")
  
  if(is.null(z)) {
    z <- ""
	zconst <- ""
	zwoconst <- ""
  }
  else {
    for(i in seq(along=z))
      eval(parse(text=paste(names(z)[i], " <- z[[i]]", sep="")))
    z <- names(z)
    zconst <- paste(" + ", paste(paste("const(", z, ")", sep=""), collapse=" + "), sep="")
    zwoconst <- paste(" + ", paste(z, collapse=" + "), sep="")
  }
    
  if(exposinflc & minflc) {
    nocovariates <- "Surv(times, cens) ~ const(expos) + const(m)"
    aalenfit <- timereg::aalen(as.formula(paste(nocovariates, zconst, sep="")), robust=TRUE)    
  }
  if((!exposinflc) & minflc){
    nocovariates <- "Surv(times, cens) ~ expos + const(m)"
    aalenfit <- timereg::aalen(as.formula(paste(nocovariates, zconst, sep="")), robust=TRUE)    
  }
  if(exposinflc & (!minflc)){
    nocovariates <- "Surv(times, cens) ~ const(expos) + m"
    aalenfit <- timereg::aalen(as.formula(paste(nocovariates, zconst, sep="")), robust=TRUE)    
  }
  if((!exposinflc) & (!minflc)){
    nocovariates <- "Surv(times, cens) ~ expos + m"
    aalenfit <- timereg::aalen(as.formula(paste(nocovariates, zconst, sep="")), robust=TRUE)    
  }
  
  ols_m <- lm(as.formula(paste("m ~ expos", zwoconst, sep="")))
  mean_alpha <- ols_m$coefficients["expos"]
  var_alpha <- summary(ols_m)$coefficients["expos",]["Std. Error"]^2
  
  if(!simul) {	
    if(exposinflc & minflc)
      pval <- pcalcdelta(mean_lambda1=aalenfit$gamma[1], mean_lambda3=aalenfit$gamma[2], 
                             covar_lambda=aalenfit$var.gamma, mean_alpha=mean_alpha,
                             var_alpha=var_alpha, expos=expos, riskincr=riskincr)
    if((!exposinflc) & minflc)
      pval <- pcalcdelta(mean_lambda1=NULL, mean_lambda3=aalenfit$gamma[1], 
                         covar_lambda=rbind(c(NA, NA), c(NA, aalenfit$var.gamma[1,1])), mean_alpha=mean_alpha,
                         var_alpha=var_alpha, expos=expos, riskincr=riskincr)	
    if(exposinflc & (!minflc)) {
	    indrel <- max(which(aalenfit$cum[,"time"] <= quantile(times, 0.9, na.rm=TRUE)))
      pval <- pcalcdelta(mean_lambda1=NULL, mean_lambda3=aalenfit$cum[,"m"][indrel], 
                         covar_lambda=rbind(c(NA, NA), c(NA, aalenfit$var.cum[,"m"][indrel])), mean_alpha=mean_alpha,
                         var_alpha=var_alpha, expos=expos, riskincr=riskincr)
	  }
    if((!exposinflc) & (!minflc)) {
      indrel <- max(which(aalenfit$cum[,"time"] <= quantile(times, 0.9, na.rm=TRUE)))
      pval <- pcalcdelta(mean_lambda1=NULL, mean_lambda3=aalenfit$cum[,"m"][indrel], 
                         covar_lambda=rbind(c(NA, NA), c(NA, aalenfit$var.cum[,"m"][indrel])), mean_alpha=mean_alpha,
                         var_alpha=var_alpha, expos=expos, riskincr=riskincr)
	}
  }
  else {	
    if(exposinflc & minflc)
      pval <- pcalcsim(mean_lambda1=aalenfit$gamma[1], mean_lambda3=aalenfit$gamma[2], 
                         covar_lambda=aalenfit$var.gamma[c("const(expos)", "const(m)"), c("const(expos)", "const(m)")], mean_alpha=mean_alpha,
                         var_alpha=var_alpha, expos=expos, riskincr=riskincr, npvalsim=npvalsim)
    if((!exposinflc) & minflc)
    pval <- pcalcsim(mean_lambda1=NULL, mean_lambda3=aalenfit$gamma[1], 
                       covar_lambda=rbind(c(NA, NA), c(NA, aalenfit$var.gamma[1,1])), mean_alpha=mean_alpha,
                       var_alpha=var_alpha, expos=expos, riskincr=riskincr, npvalsim=npvalsim)
    if(exposinflc & (!minflc)) {
      indrel <- max(which(aalenfit$cum[,"time"] <= quantile(times, 0.9, na.rm=TRUE)))
      pval <- pcalcsim(mean_lambda1=NULL, mean_lambda3=aalenfit$cum[,"m"][indrel], 
                       covar_lambda=rbind(c(NA, NA), c(NA, aalenfit$var.cum[,"m"][indrel])), mean_alpha=mean_alpha,
                       var_alpha=var_alpha, expos=expos, riskincr=riskincr, npvalsim=npvalsim)
	}
    if((!exposinflc) & (!minflc)) {
      indrel <- max(which(aalenfit$cum[,"time"] <= quantile(times, 0.9, na.rm=TRUE)))
      pval <- pcalcsim(mean_lambda1=NULL, mean_lambda3=aalenfit$cum[,"m"][indrel], 
                       covar_lambda=rbind(c(NA, NA), c(NA, aalenfit$var.cum[,"m"][indrel])), mean_alpha=mean_alpha,
                       var_alpha=var_alpha, expos=expos, riskincr=riskincr, npvalsim=npvalsim)
	}
  }
	
  return(list(pvalue=pval))	
	
}
