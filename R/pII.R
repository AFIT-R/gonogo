#' Title
#'
#' @param d0 
#' @param dat0 
#' @param tmu 
#' @param tsig 
#' @param n2 
#' @param reso 
#' @param ln 
#' @param iseed 
#'
#' @return
#' @export
#'
#' @examples
pII <-
function(d0,dat0,tmu,tsig,n2,reso,ln,iseed)
{
# Wu/Tian definition of n2 would be n2 = n2-nrow(d0);
xl=xu=xstar=mu2=mu4=sg2=sg4=rep(0,n2);
for(i in 1:n2)
	{
	nq=glmmle(d0);
	mu2[i]=nq$mu;
	sg2[i]=nq$sig;
	xl[i]=min(d0$X); xu[i]=max(d0$X);
	mu4[i]=max(xl[i],min(mu2[i],xu[i]));
	sg4[i]=min(sg2[i],xu[i]-xl[i]);
	j=ykpm(d0,mu4[i],sg4[i]);
	xstar[i]=j$xstar;
	id="II1";
	if(i > 1) id="II2";
	u=gd0(xstar[i],d0,dat0,"II",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
	}
ret=list(d0,dat0);
names(ret)=c("d0","dat0");		
return(ret);
}
