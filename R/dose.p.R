#' Title
#'
#' @param obj 
#' @param p 
#'
#' @return
#' @export
#'
#' @examples
dose.p <-
function(obj,p)
	{
	np=length(p)
	se=rep(0,np)
	b=as.vector(obj$coef)
	x.p=(qnorm(p)-b[1])/b[2]
	for(i in 1:np)
	{
	pd= -c(1,x.p[i])/b[2]
	se[i]=sqrt((t(pd)%*%vcov(obj))%*%pd)
	}
	a=matrix(c(x.p,se),ncol=2)
	a=data.frame(a)
	names(a)=c("dose","se")
	return(a)
	}
