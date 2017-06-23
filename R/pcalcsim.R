pcalcsim <-
function(mean_lambda1, mean_lambda3, covar_lambda, mean_alpha, var_alpha, expos, riskincr, npvalsim=1e+5) {
    
  if(!is.na(covar_lambda[1,1])) {
    
    #require("mvtnorm")  
    lambda <- mvtnorm::rmvnorm(npvalsim, mean = c(mean_lambda1,mean_lambda3),
                      sigma = covar_lambda)
    alpha <- rnorm(npvalsim,mean=mean_alpha, sd=sqrt(var_alpha))
      
    IE <- lambda[,2] * alpha
      
    if(riskincr)
      return(mean(IE <= 0))
    else
      return(mean(IE >= 0))
    
  }
  else {
      
    lambda3 <- rnorm(npvalsim, mean = mean_lambda3,
                     sd = sqrt(covar_lambda[2,2]))
    alpha <- rnorm(npvalsim,mean=mean_alpha, sd=sqrt(var_alpha))
      
    IE <- lambda3 * alpha
      
    if(riskincr)
      return(mean(IE <= 0))
    else
      return(mean(IE >= 0))
      
  }
    
}
