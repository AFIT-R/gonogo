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
ml.lims <-
function(dat,conf,maxitt=10,response=1,rd=6)
	{
	# Calculates qlo, qhi, plo & phi as in ml2002
	x=rep(dat$X,dat$COUNT);	y=rep(dat$Y,dat$COUNT)
	if(response == 0.)y=abs(y-1.)
	xglm=glm(y ~ x, family = binomial(link = probit), maxit = 	maxitt, epsilon = 1e-006);
	ab=as.vector(xglm$coef);
	muhat=-ab[1]/ab[2];
	sighat=1/ab[2];
	a1=c(1,10,100,1000,10000,100000,250000)/1000000; al=c(a1,.5,sort(1-a1));
	# CI's for Normal
	qp=qnorm(al);
	vcov1=yinfomat(dat,muhat,sighat)$vcov1*sighat^2;
	varq=vcov1[1,1]+qp*(qp*vcov1[2,2]+2*vcov1[1,2]);
	zscore=qnorm((1+conf)/2);
	zdel=zscore*sqrt(varq);
		q0=muhat+qnorm(al)*sighat;
		qlo=muhat+qp*sighat-zdel;
		qhi=muhat+qp*sighat+zdel;
	dpda=1/sqrt(2*pi)*exp(-0.5*qp^2)/sighat;
	varp=dpda^2*(vcov1[1,1]+2*qp*vcov1[1,2]+qp^2*vcov1[2,2]);
	zdel=zscore*sqrt(varp);
		plo=al-zdel; plo[which(plo<0)]=0;
		phi=al+zdel; phi[which(phi>1)]=1;
	qlo=round(qlo,rd); q0=round(q0,rd); qhi=round(qhi,rd);	plo=round(plo,rd); phi=round(phi,rd);
	return(matrix(c(qlo,q0,qhi,plo,al,phi),ncol=6));
	}
