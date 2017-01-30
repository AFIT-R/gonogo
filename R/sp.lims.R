#' Title
#'
#' @param dat 
#' @param conf 
#' @param maxitt 
#' @param response 
#' @param rd 
#'
#' @return
#' @export
#'
#' @examples
sp.lims <-
function(dat,conf,maxitt=10,response=1,rd=6)
	{
	# Calculates qlo & qhi (GLM) and plo & phi (dose.p)
	x=rep(dat$X,dat$COUNT);	y=rep(dat$Y,dat$COUNT)
	if(response == 0.)y=abs(y-1.)
	xglm=glm(y ~ x, family = binomial(link = probit), maxit = 	maxitt, epsilon = 1e-006);
	ab=as.vector(xglm$coef);
	muhat=-ab[1]/ab[2];
	sighat=1/ab[2];
	a1=c(1,10,100,1000,10000,100000,250000)/1000000; al=c(a1,.5,sort(1-a1));
	q0=muhat+qnorm(al)*sighat;
	yy=predict(xglm, list(x = q0), se.fit = T);
	# There's no yy$df in R, define it
	df=sum(dat$COUNT)-2;
	conf=(1+conf)/2; k=qt(conf,df);
	yu=yy$fit+k*yy$se.fit;
	yl=yy$fit-k*yy$se.fit;
	plo=round(pnorm(yl),rd);phi=round(pnorm(yu),rd);
 	p0=pnorm(yy$fit);
	ug=dose.p(xglm,al);
	ssee=ug$se;
	qlo=round(q0-qt(conf,xglm$df.residual)*ssee,rd);
	qhi=round(q0+qt(conf,xglm$df.residual)*ssee,rd);
	q0=round(q0,rd); 
	return(matrix(c(qlo,q0,qhi,plo,al,phi),ncol=6));
	}
