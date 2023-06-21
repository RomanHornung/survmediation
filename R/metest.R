metest <-
  function(times, cens, expos, M, z=NULL, simul=TRUE, npvalsim=1e+5, riskincr, exposinflc=TRUE, minflc=TRUE, returnmodels=FALSE) {
    
    if(!returnmodels) {
      
      if("numeric" %in% class(M)) {
        metesttemp <- metestone(times=times, cens=cens, expos=expos, m=M, z=z, simul=simul, npvalsim=npvalsim, 
                                riskincr=riskincr, exposinflc=exposinflc, minflc=minflc)
        pvalues <- metesttemp$pvalue
      }
      
      if("matrix" %in% class(M)) {
        pvalues <- 0
        for(i in 1:ncol(M)) { 
          metesttemp <- metestone(times=times, cens=cens, expos=expos, m=M[,i], z=z, simul=simul, npvalsim=npvalsim, 
                                  riskincr=riskincr, exposinflc=exposinflc, minflc=minflc)
          pvalues[i] <- metesttemp$pvalue
        }
      }
      
      params <- NULL
      
    } else {
      
      if("numeric" %in% class(M)) {
        metesttemp <- metestone(times=times, cens=cens, expos=expos, m=M, z=z, simul=simul, npvalsim=npvalsim, 
                                riskincr=riskincr, exposinflc=exposinflc, minflc=minflc)
        pvalues <- metesttemp$pvalue
        params <- metesttemp$params
      }
      
      if("matrix" %in% class(M)) {
        pvalues <- 0
        params <- list()
        for(i in 1:ncol(M)) { 
          metesttemp <- metestone(times=times, cens=cens, expos=expos, m=M[,i], z=z, simul=simul, npvalsim=npvalsim, 
                                  riskincr=riskincr, exposinflc=exposinflc, minflc=minflc)
          pvalues[i] <- metesttemp$pvalue
          params[[i]] <- metesttemp$params
        }
      }
      
    }
    
    class(metest) <- "metest"
    
    return(list(pvalues=pvalues, params=params))
    
  }
