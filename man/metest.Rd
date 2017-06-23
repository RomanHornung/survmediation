\name{metest}
\alias{metest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mediation analysis
}
\description{
Univariately test for mediation effect with respect to one or several numerical variables in a survival setting.
}
\usage{
metest(times, cens, expos, M, z = NULL, simul = TRUE, npvalsim = 1e+5, riskincr, 
  exposinflc = TRUE, minflc = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{times}{
numeric. Vector of survival/censoring times.
}
  \item{cens}{
numeric. Survival status vector. Each entry must take the value "1" if the corresponding entry in \code{times} is a survival time and "0" if this entry is a censoring time.
}
  \item{expos}{
numeric. Exposure vector. Each value must take the value "1" if the exposure is present in the corresponding case and the value "0" if the exposure is absent for that case.
}
  \item{M}{
matrix or vector. If \code{M} is matrix than there are more than one (potential) mediators. In this case,
each column contains the values of one of the (potential) mediators. If \code{M} is a vector than this vector simply contains
the values of the (potential) mediator to be studied.
}
  \item{z}{
list. Named list, where each entry contains the values of one of the covariates to be adjusted for.
}
  \item{simul}{
logical. TRUE if the simulation approach should be used. FALSE if the delta approximation should be used.
}
  \item{npvalsim}{
numeric. Number of simulation iterations to be used in the simulation approach. Default is 1e+5. To obtain more precise p values use 1e+6 or 1e+7 iterations if computationally feasible.
}
  \item{riskincr}{
logical. TRUE if it is assumed that the presence of the exposure leads to an increase in risk. FALSE if presence of the exposure is
assumed to lead to a decreased risk.
}
  \item{exposinflc}{
logical. TRUE if the influence of the exposure should be modelled as constant over time. FALSE if this influence should be modelled as time-varying.
}
  \item{minflc}{
logical. TRUE if the influence of the mediator should be modelled as constant over time. FALSE if this influence should be modelled as time-varying.
}
}
\details{
For details see Lange & Hansen (2011).
}
\value{
\code{metest} returns an object of class \code{metest}.
An object of class "\code{metest}" is a list containing the following components: 
\item{pvalues}{ vector of length equal to the number of (potental) mediators tested, where
the j-th entry corresponds to the p-value for the j-th (potential) mediator }
}
\references{
Lange, T., Hansen, J.V. (2011) Direct and indirect effects in a survival context. Epidemiology 22:575–581.
}
\author{
Roman Hornung
}

\examples{
# Simulate a dataset:
#####################

# Simulation parameters:

# Number of observations:
n <- 100
# Number of (potential) mediators:
p <- 10

# Indices of true mediators:
mediatorind <- 1:3

# Baseline hazard rate:
lambda <- 0.05

# Effect of exposure on the three mediators:
exposmediatoreff <- 0.7

# Effect of the mediators on survival:
surveff <- 0.8

# Direct effect of the mediator on survival:
exposeff <- 0.6

# Hazard rate of exponentially distributed censoring times:
lambdacens <- 0.02

# Coefficient of linear predictor in the Cox regression
# model relating the (potential) mediators and the
# exposure to the outcome:

beta <- rep(0, p+1)
beta[mediatorind] <- surveff
beta[p+1] <- exposeff


# Simulate data:

# Set seed:
set.seed(123)

# Exposure:
expos <- rep(c(1,0), each=n/2)

# (Potential) mediators:
X <- matrix(nrow=n, ncol=p, data=rnorm(n*p))
X[expos==1, mediatorind] <- X[expos==1, mediatorind] + exposmediatoreff

# Uncensored survival times:
linpred <- cbind(X, expos)\%*\%beta
indhazards <- as.vector(lambda*(exp(linpred)))
timesuncens <- -log(runif(n))/indhazards

# Censoring times:
censtimes <- rexp(n, rate=lambdacens)

# Censoring variable:
cens <- ifelse(timesuncens < censtimes, 1, 0)

# Observed survival times:
times <- timesuncens
times[cens==0] <- censtimes[cens==0]

# data.frame:
simdata <- data.frame(X)
simdata$expos <- expos
simdata$times <- times
simdata$cens <- cens




# Mediation tests:
##################

# Time-constant influence of exposure and time-constant influences of mediators:

# First mediator only:
metest(times=simdata$times, cens=simdata$cens, expos=simdata$expos, M=simdata[,1], 
       simul = TRUE, npvalsim = 10000, riskincr=TRUE, exposinflc = TRUE, minflc = TRUE)
# All (potential) mediators:
metest(times=simdata$times, cens=simdata$cens, expos=simdata$expos, 
       M=as.matrix(simdata[,1:10]), simul = TRUE, npvalsim = 10000, riskincr=TRUE, 
       exposinflc = TRUE, minflc = TRUE)


# Time-varying influence of exposure and time-constant influences of mediators:

metest(times=simdata$times, cens=simdata$cens, expos=simdata$expos, 
       M=as.matrix(simdata[,1:10]), simul = TRUE, npvalsim = 10000, 
       riskincr=TRUE, exposinflc = FALSE, minflc = TRUE)


# Time-constant influence of exposure and time-varying influences of mediators:

metest(times=simdata$times, cens=simdata$cens, expos=simdata$expos, 
       M=as.matrix(simdata[,1:10]), simul = TRUE, npvalsim = 10000, riskincr=TRUE, 
       exposinflc = TRUE, minflc = FALSE)


# Time-varying influence of exposure and time-varying influences of mediators:

metest(times=simdata$times, cens=simdata$cens, expos=simdata$expos, 
       M=as.matrix(simdata[,1:10]), simul = TRUE, npvalsim = 10000, riskincr=TRUE, 
       exposinflc = FALSE, minflc = FALSE)
}
