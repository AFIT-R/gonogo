#' Title
#'
#' @param dat0 
#' @param mlo 
#' @param mhi 
#' @param sg 
#' @param reso 
#' @param about 
#' @param titl 
#' @param unit 
#' @param ln 
#'
#' @return
#' @export
#'
#' @examples
phaseI1 <-
function(dat0,mlo,mhi,sg,reso,about,titl,unit,ln)
{
nret=c("d0","dat0","endi","sg");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;
endi=0;
mi=c(mlo,mhi);
a=matrix(c(.75,.25,.25,.75),ncol=2,byrow=T);
xx=t(a%*%mi);
	
for(i in 1:2)	{u=getd0(xx[i],d0,dat0,"I1",reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi; 	if(endi == 1) break; }
if(endi == 0)
{
x=d0$X; y=d0$Y;

i1=0;

if(all(y==c(0,0)))
	{
		while(1)
		{
		i1=i1+1;
		if(i1%%3 == 0) sg=2*sg;
		xx=mi[2]+1.5*i1*sg; 
		u=getd0(xx,d0,dat0,"I1(i)",reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;	
		if(d0$Y[nrow(d0)] == 1 | endi == 1) break;
		}
	}		

if(all(y==c(1,1)))
	{
		while(1)
		{
		i1=i1+1;
		if(i1%%3 == 0) sg=2*sg;
		xx=mi[1]-1.5*i1*sg; 
		u=getd0(xx,d0,dat0,"I1(ii)",reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;		
		if(d0$Y[nrow(d0)] == 0 | endi == 1) break;
		}
	}

if(all(y==c(0,1))) d0$ID=rep("I1(iii)",length(y));

if(all(y==c(1,0)))
	{
		xx=c(mlo-3*sg,mhi+3*sg);
		for(i in 1:2)
			{
			u=getd0(xx[i],d0,dat0,"I1(iv)",reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;
			if(endi == 1) break;
			}
	}	
}
ret=list(d0,dat0,endi,sg);
names(ret)=nret; 
return(ret);
}
