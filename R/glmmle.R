#' Title
#'
#' @param mydata 
#' @param response 
#' @param maxitt 
#' @param eps 
#' @param lgit 
#'
#' @return
#' @export
#'
#' @examples
glmmle <-
function(mydata, response = 1, maxitt = 10., eps = 1e-006, lgit = F)
{
	mydata=na.omit(mydata);
	x = rep(mydata$X, mydata$COUNT);
	y = rep(mydata$Y, mydata$COUNT);
	if(response == 0.) y = abs(y - 1.);
	options(warn = -1); # otherwise, R version complains in following while loop
	nmxx=c("mu","sig","llik","anom","mix","overlap","remo0","remo1","bigk","okk","mhat","shat","xm","xs");
	bigk=okk=numeric(0);
	
	j=m.update(mydata); M0=j$M0; m1=j$m1;
	xm=(j$M0+j$m1)/2; xs=(j$M0-j$m1);
	remo0=remo1=0;	
	
	if(all(y==1) | all(y==0))
	{
		anom=T; mix=F; overlap=F; mu=NA; sig=NA; ll=NA;
		xx=list(mu, sig, ll, anom, mix, overlap, remo0, remo1, bigk, okk, mu, sig, xm, xs); 
		names(xx)=nmxx;
		return(xx);
	}
	
	if(xs < 0) 
		{
		anom=T; mix=T; overlap=F; mu=xm; sig=-xs/6; ll=0;
		xx=list(mu, sig, ll, anom, mix, overlap, remo0, remo1, bigk, okk, mu, sig, xm, xs); 
		names(xx)=nmxx;
		return(xx);
		}
		
	if(xs == 0) 
		{
		anom=T; mix=T; overlap=T; mu=xm; sig=0; iw=which(x==M0); ll=NA; if(M0 > 0) ll=log(M0^iw);  
		xx=list(mu, sig, ll, anom, mix, overlap, remo0, remo1, bigk, okk, mu, sig, xm, xs); 
		names(xx)=nmxx;
		return(xx);
		}
	
	u=tauf(x,y); tau=u$tau; p1=u$p1;
	
	if(tau <= 0) 
		{
		anom=T; mix=T; overlap=T; mu=-Inf; sig=Inf; 
		iov=x >= M0 & x <= m1;
		ovx=x[iov]; nov=length(ovx);
		ll=sum(y[ovx])*log(p1) + (nov-sum(y[ovx]))*log(1-p1); 
		xx=list(mu, sig, ll, anom, mix, overlap, remo0, remo1, bigk, okk, mu, sig, xm, xs); 
		names(xx)=nmxx;
		return(xx);
		}

	# KNOW xs > 0 & tauf(x,y)  > 0
	# R's GLM WORKS WITH NEAR 0/1 FITTED PROBABILITIES (unlike S's GLM)	
	
		if(lgit) xglm = glm(y ~ x, family = binomial(link = logit), maxit = maxitt, epsilon = eps) else
			xglm = glm(y ~ x, family = binomial(link = probit), maxit = maxitt, epsilon = eps)
		ab = as.vector(xglm$coef);
		
	anom=F; mix=T; overlap=T; mu= -ab[1]/ab[2]; sig=1/ab[2]; ll=llik(mydata,mu,sig);
	xx=list(mu, sig, ll, anom, mix, overlap, remo0, remo1, bigk, okk, mu, sig, xm, xs); 
	names(xx)=nmxx;
	return(xx);
}
