identifyYearClasses <- function(r,male=T,growth.interval=NA) {
	if(is.na(growth.interval)) {
		if(male) {growth.interval = c(10,25) 
		} else {
			growth.interval = c(5,15)
		}
	}
			p 	<- r[,grep('xbar',names(r))]
			ind <- r[,grep('index',names(r))]
			lam <- r[,grep('lambda',names(r))]
			out <- list()
	   	    m=0
			for(i in 1:(nrow(p)-1)) {
	   	        	pl 	<- p[i,]
	   	        	ps 	<- length(pl[!is.na(pl)])
	   	    		for(j in 1:ps) {
	   	    		
	   	    			  if(any(p[i+1,!is.na(p[i+1,])] >= (pl[1,j]+growth.interval[1]) & p[i+1,!is.na(p[i+1,])] <= (pl[1,j]+growth.interval[2]))) {
	   	    			  m=m+1
	   	    			  ui <- which(p[i+1,] >= (pl[1,j]+growth.interval[1]) & p[i+1,] <= (pl[1,j]+growth.interval[2]))
	   	    			  if(length(ui)>1) {
	   	    			  	if(lam[i+1,ui[1]]>.08){
	   	    			  	ui <- ui[1]
	   	    			  	} else {
	   	    			  	ui <- ui[2]
	   	    			  	}
	   	    			  }
	   	    			  out[[m]] <- cbind(i,ind[i,j],ind[i+1,ui])
	   	     	               }
	   	     	         }
	   	    		}
	   	    		nc <- max(dimList(out)[,2])
	   	    		op <- as.data.frame(matrix(NA,nrow=length(out),ncol=nc,dimnames=list(c(1:length(out)),c('rowindex','modecomponent',paste('comp+1',1:(nc-2),sep=".")))))
	   	    		for(i in 1:length(out)) {
	   	    			lp <- out[[i]]
	   	    			op[i,1:length(lp)] <- lp	 
	   	    		}
	   	    		 return(out)
	   	    	}
	   	    			