annualMixBoot <- function(x,aic.threshold=5,mode.threshold=10,init.mode,ni=1) {
	#x is a matrix with columns representing the years created by reshapeCW
	#init.mu is a list of starting values created from identifyModes
	ny <- ncol(x)
	out <- list()
	outg <- list()
	best.model <- c()

	for(i in 1:ny) {
			out[[i]] <- NULL
			outg[[i]] <- NULL
			best.model[i] <- 0
				out[[i]] <- nMixNormalBoot(x[,i],aic.threshold=aic.threshold,mode.threshold=mode.threshold,init.mode=init.mode[[i]],niter=ni)
				outg[[i]] <- nMixGammaBoot(x[,i],aic.threshold=aic.threshold,mode.threshold=mode.threshold,init.mode=init.mode[[i]],niter=ni)
				best.model[i] <- which.min(c(aicc(lL=out[[i]]$loglik,k=length(out[[i]]$lambda)*3-1,n=length(out[[i]]$x)),aicc(lL=outg[[i]]$loglik,k=length(outg[[i]]$lambda)*3-1,n=length(outg[[i]]$x))))
		
	}
		oo <- list(out,outg,best.model)
		return(oo)	
	}
	
	