\name{survmediation-internal} 
\title{Internal survmediation functions}
\alias{metestone}
\alias{pcalcdelta}
\alias{pcalcsim}
\description{
  Internal survmediation functions.
}
\usage{
metestone(times, cens, expos, m, z = NULL, simul = TRUE, npvalsim = 1e+5, 
  riskincr, exposinflc = TRUE, minflc = TRUE)
pcalcdelta(mean_lambda1, mean_lambda3, covar_lambda, mean_alpha, 
  var_alpha, expos, riskincr)
pcalcsim(mean_lambda1, mean_lambda3, covar_lambda, mean_alpha, 
  var_alpha, expos, riskincr, npvalsim = 1e+5)
}
\details{
  These internal \pkg{survmediation} functions are not usually called
  directly by the user.
}
\keyword{internal}
