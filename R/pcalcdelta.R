pcalcdelta <-
function(mean_lambda1, mean_lambda3, covar_lambda, mean_alpha, var_alpha, expos, riskincr) {

  IE <- mean_lambda3*mean_alpha
  varIE <- ((mean_alpha)^2)*covar_lambda[2,2] + ((mean_lambda3)^2)*var_alpha
  
  if(riskincr)
    return(1 - pnorm(IE, mean=0, sd=sqrt(varIE)))
  else
    return(pnorm(IE, mean=0, sd=sqrt(varIE)))
    
}
