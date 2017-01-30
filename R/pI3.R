#' Title
#'
#' @param d0 
#' @param dat0 
#' @param sg 
#' @param tmu 
#' @param tsig 
#' @param reso 
#' @param ln 
#' @param iseed 
#'
#' @return
#' @export
#'
#' @examples
pI3 <-
function(d0,dat0,sg,tmu,tsig,reso,ln,iseed)
{
j=m.update(d0); M0=j$M0; m1=j$m1; del=m1-M0;
xx=(M0+m1)/2;
if(sg+del > 0)xx=xx+c(1,-1)*sg/2;
lxx=length(xx)
for(i in 1:lxx)
	{
	if(i < lxx) u=gd0(xx[i],d0,dat0,"I3",tmu,tsig,reso,ln,iseed); 
	if(i == lxx) u=gd0(xx[i],d0,dat0,"I3",tmu,tsig,reso,ln,iseed);
	d0=u$d0; dat0=u$dat0;
	}
ret=list(d0,dat0);
names(ret)=c("d0","dat0");		
return(ret);
}
