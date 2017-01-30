#' Title
#'
#' @param mlo 
#' @param mhi 
#' @param sg 
#' @param tmu 
#' @param tsig 
#' @param reso 
#' @param ln 
#' @param iseed 
#' @param dat0 
#'
#' @return
#' @export
#'
#' @examples
pI1 <-
function(mlo,mhi,sg,tmu,tsig,reso,ln,iseed,dat0=data.frame(numeric(0)))
{
nret=c("d0","dat0");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;
del=(mhi-mlo)/6;
epsi=del/1000;
mi=c(mlo,mhi);
a=matrix(c(.75,.25,.25,.75),ncol=2,byrow=T);
xx=t(a%*%mi);
	
for(i in 1:2)	{u=gd0(xx[i],d0,dat0,"I1",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0; }

i1=0;
	
x=d0$X; y=d0$Y;
if(all(y==c(0,0)))
	{
		xx=mi[2];
		while(1)
		{
		i1=i1+1;
		if(i1%%3 == 0) sg=2*sg;
		xx=xx+1.5*i1*sg;
		u=gd0(xx,d0,dat0,"I1(i)",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0; 		
		if(d0$Y[nrow(d0)] == 1) break;
		}
	}		

if(all(y==c(1,1)))
	{
		xx=mi[1];
		while(1)
		{
		i1=i1+1;
		if(i1%%3 == 0) sg=2*sg;
		xx=xx-1.5*i1*sg;
		u=gd0(xx,d0,dat0,"I1(ii)",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
		if(d0$Y[nrow(d0)] == 0) break;
		}
	}
	
if(all(y==c(0,1))) d0$ID=rep("I1(iii)",length(y));

if(all(y==c(1,0)))
	{
		xx=c(mlo-3*sg,mhi+3*sg);
		for(i in 1:2)
			{
			u=gd0(xx[i],d0,dat0,"I1(iv)",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;
			}
	}	
ret=list(d0,dat0); 
names(ret)=nret; 
return(ret);
}
