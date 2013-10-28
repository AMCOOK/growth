nMixNormalBoot <- function(x,aic.threshold=2,mode.threshold=5,init.mode,niter=1000) {

		require(mixtools)
		loglikes 	<- vector(length=niter)
		aic 		<- vector(length=niter)
		nmodes 		<- vector(length=niter)
		modes 		<- list()
		x 			<- na.omit(x)
		n			<- length(x)
		nsamp 		<- length(init.mode)
		outmodes	<- list()
		
		for(i in 1:niter) {
		ins <- sample(init.mode,size=sample.int(nsamp,1),replace=F)
		k			<- length(ins)
		modes[[i]] 	<- ins
		nmodes[[i]] <- k
		
		
			if(k==1) {	# k=1 needs special handling
					mu			<- mean(x) 				# MLE of mean
					sigma 		<- sd(x)*sqrt((n-1)/n) 	# MLE of standard deviation
					
					loglikes[i] <- sum(dnorm(x,mu,sigma,log=TRUE))
					aic[i] 		<- aicc(lL=loglikes[i],k=k*3-1,n=n)
					outmodes[[i]] <- mu
				} else {
					mixture 	<- try(normalmixEM(x,k=k,maxit=1000,epsilon=1e-2,mu=ins,verb=F),silent=T)
					if(class(mixture)!='try-error') {
					loglikes[i] <- loglikeMix(x,mixture=mixture)
					aic[i] 		<- aicc(lL=loglikes[i],k=k*3-1,n=n)
					outmodes[[i]] <- mixture$mu
				} else {
					loglikes[i] <- -100000
					aic[i] 		<- aicc(lL=loglikes[i],k=k*3-1,n=n)
					outmodes[[i]] <- 0
				}
			}
		}
	#nk <- findMinAIC(x=data.frame(nmodes,aic,1:length(aic)),thresh=aic.threshold)	
	nk <- findBestModel(x=data.frame(nmodes,aic,1:length(aic)),aic.thresh=aic.threshold,modes=modes,mode.thresh=6)	
	nmo <- nmodes[nk]
	if(length(nmo>1)==0) browser()
	if(nmo>1) {
			mixture <- normalmixEM(x,k=nmodes[nk],maxit=1000,epsilon=1e-2,mu=outmodes[[nk]],verb=F)
			o 		<- sort(mixture$mu) 
	} else {
		mu<-mean(x) # MLE of mean
				sigma <- sd(x)*sqrt((n-1)/n) # MLE of standard deviation
				loglikes <- sum(dnorm(x,mu,sigma,log=TRUE))
		mixture=list(mu=mu,sigma = sigma,ft='normalmixEM',lambda=1,ft='normalmixEM',loglik=loglikes)
	}

	return(mixture)
}