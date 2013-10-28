annualMeans <- function(x,lambda.threshold=0.075) {
	#x is from the annualMix function
	
	bf <- x[[3]] 
	out <- list()
	#setup init matrix
		lo <- c()
			for(i in 1:length(bf)) {
			if(bf[i]>0) {
	        	lo[i] <-  length(x[[bf[i]]][[i]]$lambda)  
	  			}
	  		}
	  	mo <- max(lo)
	ll <- length(x[[1]])
	out <- as.data.frame(matrix(NA,nrow=ll,ncol=2+5*mo,dimnames=list(c(1:ll),c('Index','model',paste('par1',1:mo,sep="."),paste('par2',1:mo,sep="."),paste('lambda',1:mo,sep="."),paste('xbar',1:mo,sep="."),paste('index',1:mo,sep=".")))))
		
	for(i in 1:length(bf)) {
		if(bf[i]>0) {
			y <- x[[bf[i]]][[i]]
		out[i,'Index'] <- i
		out[i,'model'] <- y$ft
			
		if(y$ft=='gammamixEM') {
				inde 										<- order(y$median)
				llp 										<- length(inde)
				out[i,grep('par1',colnames(out))][1:llp] 	<- y$gamma.pars[1,][inde]
				out[i,grep('par2',colnames(out))][1:llp] 	<- y$gamma.pars[2,][inde]
				out[i,grep('lambda',colnames(out))][1:llp] 	<- y$lambda[inde]
				out[i,grep('xbar',colnames(out))][1:llp] 	<- y$median[inde]	
				out[i,grep('index',colnames(out))][1:llp] 	<- inde	
				
		}else{
				inde 										<- order(y$mu)
				llp 										<- length(inde)
				out[i,grep('par1',colnames(out))][1:llp] 	<- y$mu[inde]
				out[i,grep('par2',colnames(out))][1:llp] 	<- y$sigma[inde]
				out[i,grep('lambda',colnames(out))][1:llp] 	<- y$lambda[inde]
				out[i,grep('xbar',colnames(out))][1:llp] 	<- y$mu[inde]
				out[i,grep('index',colnames(out))][1:llp] 	<- inde	
			}
		}
	}
	return(as.data.frame(out))
}

#old
#annualMeans <- function(x,lambda.threshold=0.075) {
#	#x is from the annualMix function
#	
#	bf <- x[[3]]
#	ll <- length(x[[1]])
#	out <- data.frame(year=1:ll,best.model=NA,par1.1=NA,par2.1=NA,par3.1=NA,par4.1=NA,par5.1=NA,par6.1=NA,par1.2=NA,par2.2=NA,par3.2=NA,par4.2=NA,par5.2=NA,par6.2=NA,lam1=NA,lam2=NA,lam3=NA,lam4=NA,lam5=NA,lam6=NA,mu1=NA,mu2=NA,mu3=NA,mu4=NA,mu5=NA,mu6=NA)
#	for(i in 1:length(bf)) {
#	if(bf[i]>0) {
#		y <- x[[bf[i]]][[i]]
#		out[i,2] <- y$ft
#		lo <- length(y$lambda)
#			
#		for(j in 1:lo) {
#			if(y$ft=='gammamixEM') {
#					
#				out[i,2+j] 	<- y$gamma.pars[1,j]
#				out[i,8+j] 	<- y$gamma.pars[2,j]
#				out[i,14+j] <- y$lambda[j]	
#				out[i,20+j] <- y$median[j]
#		
#		}else	{
#				out[i,2+j] 	<- y$mu[j]
#				out[i,8+j] 	<- y$sigma[j]
#				out[i,14+j] <- y$lambda[j]
#				out[i,20+j] <- y$mu[j]
#				}
#			}
#		}
#	    for(i in 1:nrow(out)) {
#	    	
#			l <- which(out[i,15:20]<lambda.threshold)
#			out[i,l+20]  <- NA
#			out[i,21:26] <- out[i,20+order(out[i,21:26])]
#	    }
#	    
#		
#}
#return(out)
#}