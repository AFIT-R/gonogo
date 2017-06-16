#' gonogoSim
#'
#' gonogoSim Description
#'
#' @param mlo Guess for mu_min
#' @param mhi Guess for mu_max
#' @param sg Guess for standard deviation, should satisfy: mhi - mlo >= 6*sg
#' @param n2 Phase II test size, n2 >= 0, n2 = 0 skips Phase II and proceeds directly to Phase III
#' @param n3 Phase III test size, n3 >= 0, n3 = 0 skips Phase III
#' @param p Phase III skewed RMJ procedure p (probability)
#' @param lam Phase III skewed RMJ procedure lambda (skewness coefficient)
#' @param dm dm is the deviation from the true mean (mu_true), which is taken to be (mlo+mhi)/2.
#' @param ds ds is the deviation from the true standard deviation (sigma_true), which is taken to be sg.
#' @param ln test in log(X) units (Experimental ln=T)
#' @param plt plt=0 for no plot
#' @param neyer Neyer test (neyer=T) or 3pod test (neyer=F)
#' @param iseed An iseed unequal to -1 allows one to repeatedly generate the same sequence of X's & Y's.
#' 0 <= iseed < Inf if you want repeatability in the X's and Y's
#' if iseed is NOT the default, then we set.seed(nd0+iseed) in gd0, where nd0 is the unique trial number within gd0.
#' @param IIgo IIgo = T means we're continuing into Phase II (IIgo = F would be useful to compare 3pod and Neyer Phase I).
#' 3pod's Phase I sample size (n1) was further broken down into two parts (n11 and n12).
#' n11, being the appropriate sample size to compare to Neyer's n1, is the stopping point for the IIgo=F option for the 3pod case.
#' @param M the simulation ALWAYS multiplies mlo, mhi, sg, dm and ds by M (default is 1)
#' this is done to show procedures are scale-free
#'
#' @return list(d0,jvec,tmu,tsig,v$mu,v$sig,en,abo,titl,uni,p,reso,ln,lam,neyer,M,dm,ds,iseed)
#' @export
#'
#' @examples
#' M= 1;  y1=gonogoSim(0,22,3,5,5,.9,1,plt=1,dm=1,ds=1,M= 1,iseed=5)
#' M=10; y10=gonogoSim(0,22,3,5,5,.9,1,plt=1,dm=1,ds=1,M=10,iseed=5)
#' Look at graphs: ptest(y1,i) v ptest(10,i), for i=1,2 and 3. Examine y1$d0 and y10$d0
gonogoSim <-
function(mlo,mhi,sg,n2,n3,p,lam,dm=0,ds=0,ln=F,plt=0,neyer=F,iseed=-1,IIgo=T,M=1)
{
	# IIgo = F <--> Stop Simulation at End of (1) 3pod Phase I2; (2) Neyer Phase I
	# 0 <= iseed < Inf if you want repeatability in the X's and Y's
	# if iseed is NOT the default, then we set.seed(nd0+iseed) in gd0, where nd0 is the unique trial number within gd0.
	# M: To verify the Phase III is really scale-free, run gonogoSim twice with different M's (with same fixed iseed option).
	# For example: 	M= 1;  y1=gonogoSim(0,22,3,5,5,.9,1,plt=1,dm=1,ds=1,M= 1,iseed=5)
	# 		M=10; y10=gonogoSim(0,22,3,5,5,.9,1,plt=1,dm=1,ds=1,M=10,iseed=5)
	#		Look at graphs: ptest(y1,i) v ptest(10,i), for i=1,2 and 3. Examine y1$d0 and y10$d0

	# reso option disabled (i.e., reso=0 for now). Left references in code to possibly reconsider this matter.
	reso=0;

	jvec=NULL;
	if(M <= 0) M=1;
	sgrem=sg=M*sg; mlo1=mlo=M*mlo; mhi1=mhi=M*mhi; dm=M*dm; ds=M*ds;

	del5=(mhi-mlo)/6;
	epsi=del5/1000;
	if(sg>(mhi-mlo)/6+epsi){cat(paste("sg is too big (sg <= ",round(del5,4),")\nTry again\n\n",sep="")); return();}
	if(p <= 0 | p >= 1 | lam <= 0) {cat(paste("p must be between 0 & 1 and lambda > 0.\nTry again\n\n",sep="")); return();}
	if(n2 <= 0 | n3 <= 0) {cat(paste("n2 & n3 must be positive integers.\nTry again\n\n",sep="")); return();}

	savinit=c(mlo,mhi,sg);
	if(ln) {v=fgs(mlo,mhi,sg); mlo=v[1]; mhi=v[2]; sg=v[3];}
	init=c(mlo,mhi,sg);
	tmu=(mlo+mhi)/2+dm; tsig=sg+ds;

	dat0=data.frame(numeric(0));
  if (!neyer)
  {
	w=pI1(mlo,mhi,sg,tmu,tsig,reso,ln,iseed);
	d0=w[[1]]; dat0=w[[2]];

	w=pI2(d0,dat0,sg,tmu,tsig,reso,ln,iseed);
	d0=w[[1]]; dat0=w[[2]]; sg=w[[3]]; n12=0; n1=n11=nrow(d0);

	if(IIgo)
	{
	w=pI3(d0,dat0,sg,tmu,tsig,reso,ln,iseed);
	d0=w[[1]]; dat0=w[[2]]; n1=nrow(d0); n12=n1-n11;

	if(n2 > 0) {w=pII(d0,dat0,tmu,tsig,n2,reso,ln,iseed); d0=w[[1]]; dat0=w[[2]];}

	if(n3 > 0 & p*(1-p) > 0 & lam >= 0) {w=spIIIsim(d0,dat0,tmu,tsig,n3,p,lam,reso,ln,iseed); d0=w[[1]]; dat0=w[[2]]; jvec=w[[3]];}
	}
  } else
  {
	w=npI(mlo,mhi,sg,tmu,tsig,reso,ln,iseed);
	d0=w[[1]]; dat0=w[[2]]; n1=n11=nrow(d0); n12=0;

	if(IIgo)
	{
	if(n2 > 0) {w=pII(d0,dat0,tmu,tsig,n2,reso,ln,iseed); d0=w[[1]]; dat0=w[[2]];}
	if(n3 > 0 & p*(1-p) > 0 & lam >= 0) {w=spIIIsim(d0,dat0,tmu,tsig,n3,p,lam,reso,ln,iseed); d0=w[[1]]; dat0=w[[2]]; jvec=w[[3]];}
	}
  }

	en=c(n11,n12,n2,n3);
	v=glmmle(d0);
	ret=list(d0,tmu,tsig,v$mu,v$sig,en);
	names(ret)=c("d0","tmu","tsig","mhat","shat","en");

	if(!IIgo | n3 == 0) mat2=mat3=0;

	if(is.element(plt,c(1,2,3)))
	{
	abo=wabout13(M,mlo1,mhi1,sgrem,p,n11,n12,n2,n3,lam,reso);
	h1=""; if(ln) h1="log "; h2="3pod"; if(neyer) h2="Neyer"
	titl=paste(h1,h2,sep="");
	if(M==1)uni="X" else uni=paste(M,"X",sep="");
	ret=list(d0,jvec,tmu,tsig,v$mu,v$sig,en,abo,titl,uni,p,reso,ln,lam,neyer,M,dm,ds,iseed);
	names(ret)=c("d0","jvec","tmu","tsig","mhat","shat","en","about","title","units","p","reso","ln","lam","neyer","M","dm","ds","iseed")
	pStest(ret,plt);
	}
	return(ret);
}
