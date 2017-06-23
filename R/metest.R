metest <-
function(times, cens, expos, M, z=NULL, simul=TRUE, npvalsim=1e+5, riskincr, exposinflc=TRUE, minflc=TRUE) {

  if(class(M)=="numeric") {
    metesttemp <- metestone(times=times, cens=cens, expos=expos, m=M, z=z, simul=simul, npvalsim=npvalsim, 
                            riskincr=riskincr, exposinflc=exposinflc, minflc=minflc)
    pvalues <- metesttemp$pvalue
  }
  
  if(class(M)=="matrix") {
    pvalues <- 0
    for(i in 1:ncol(M)) { 
      metesttemp <- metestone(times=times, cens=cens, expos=expos, m=M[,i], z=z, simul=simul, npvalsim=npvalsim, 
                              riskincr=riskincr, exposinflc=exposinflc, minflc=minflc)
      pvalues[i] <- metesttemp$pvalue
    }
  }
  
  class(metest) <- "metest"

  return(list(pvalues=pvalues))
 
}
