#' Title
#'
#' @param dat 
#' @param response 
#'
#' @return
#' @export
#'
#' @examples
ntau <-
function(dat,response=1)
{
	# NOTE: ntau(dat,response=0) == -ntau(dat,response=1)
	# Whatever the response(i.e., 0 or 1) with Pr[response increases as stress increases] 
	# then a positive ntau(dat) <---> to a proper non-decreasing CDF response model
	# a negative ntau <---> to a flat response model <---> Mu=-Inf, Sig=Inf, pnorm((X-Mu)/Sig)=C
	# where C=#responses/#tested = r/n, say. then (X-MU)/Sig=qnorm(r/n) for all X ==> Sig=K, Mu=X-K
	# For K = Inf (i.e., K very large).
	
	if(response==0) dat$Y=1-dat$Y;
	st=dat$X;	
	i1=which(dat$Y==1); 
	i0=which(dat$Y==0); 
	r1=r0=n=dat$COUNT;
	r1[i0]=0; 
	r0[i1]=0;	
	nt=sum(n); n1=sum(r1); 
	tau1=sum((r1/n-n1/nt)*(n*st))
	tau2=sum(r1*st)-n1*sum(n*st)/nt
	tau3=sum(r1*st)-n1*mean(st,weights=n)
	tau4=n1*(mean(st,weights=r1)-mean(st,weights=n))
	return(tau4)
}
