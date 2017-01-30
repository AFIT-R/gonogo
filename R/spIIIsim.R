#' Title
#'
#' @param d0 
#' @param dat0 
#' @param tmu 
#' @param tsig 
#' @param n3 
#' @param p 
#' @param lam 
#' @param reso 
#' @param ln 
#' @param iseed 
#'
#' @return
#' @export
#'
#' @examples
spIIIsim <-
function(d0,dat0,tmu,tsig,n3,p,lam,reso,ln,iseed)
{
jvec=matrix(rep(0,10*(n3+1)),ncol=10);
nq=glmmle(d0);
mu=nq$mu; sig=nq$sig;

# Calculate initial tau1^2
# this variance/covariance matrix (vcov1) is scale free
ww=yinfomat(d0,mu,sig);
tau2=sum(t(c(1,qnorm(p)^2))*diag(ww$vcov1));
# print("tau2"); print(tau2)

# Truncate tau2[1]
ti=round((c(3,5)/qnorm(.975))^2,4);

#** NEW 
if(ln) ti=round((c(3,5)/qlnorm(.975))^2,4);

tau2=min(max(tau2[1],ti[1]),ti[2]);
	
# Use Mu Tilda and Sigma Tilda instead of Mu Hat and Sigma Hat 
m1=min(d0$X,na.rm=T); 
m2=max(d0$X,na.rm=T);
m2=min(c(mu,m2),na.rm=T);
mut=max(c(m1,m2),na.rm=T);
sigt=min(sig,diff(range(d0$X)),na.rm=T);
	
# Beta = 1/(2* SigmaTilda) per pp 9. You get beta = 0.4302985
# be=1/(2*sigt); make "be" scale free
be=sig/(2*sigt);

#** NEW 
if(ln) be=(plnorm(qlnorm(p))*sig)/(pnorm(qnorm(p))*sigt)

c1=f3point8(lam);
nu=sqrt(tau2)*c1;
	
xx=mut+qnorm(p)*sigt+nu;
u=gd0(xx,d0,dat0,"III1",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;
ny=length(d0$Y); yy=d0$Y[ny];
jvec[1,]=c(0,0,0,0,0,tau2,nu,0,xx,yy);
for(i in 1:n3)
{
	# Compute next X|d0
	vv=skewL(c1,nu,tau2,p,be);
	a=vv[5]; tau2=vv[6]; nu=vv[7]; b=vv[8];
	xx=d0$X[nrow(d0)]-a*(d0$Y[nrow(d0)]-b)*sig;

if(i < n3)
	{
	u=gd0(xx,d0,dat0,"III2",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;
	ny=length(d0$Y);
	yy=d0$Y[ny]
	jvec[i+1,]=c(vv,xx,yy);
	}
if(i == n3)
	{
	d0=rbind(d0,d0[nrow(d0),]);
	d0[nrow(d0),1:6]=c(0,0,0,round(xx,5),0,0);
	d0$ID[nrow(d0)]="III3";
	jvec[i+1,]=c(vv,xx,NA);
	}
}
jvec=data.frame(jvec);
names(jvec)=c("j","k","v","u","a","tau2","nu","b","x","y");
ret=list(d0,dat0,jvec); 
names(ret)=c("d0","dat0","jvec");
return(ret);
}
