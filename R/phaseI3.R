#' Title
#'
#' @param d0 
#' @param dat0 
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
phaseI3 <-
function(d0,dat0,sg,reso,about,titl,unit,ln)
{
j=m.update(d0); M0=j$M0; m1=j$m1; del=m1-M0;
xx=(M0+m1)/2;
if(sg+del > 0)xx=xx+c(1,-1)*sg/2;
lxx=length(xx)
for(i in 1:lxx)
	{
	if(i < lxx) u=getd0(xx[i],d0,dat0,"I3",reso,about,titl,unit,ln); 
	if(i == lxx) u=getd0(xx[i],d0,dat0,"I3",reso,about,titl,unit,ln,cab=T);	
	d0=u$d0; dat0=u$dat0; endi=u$endi;
	if(endi == 1) break;		
	}
ret=list(d0,dat0,endi);
names(ret)=c("d0","dat0","endi");		
return(ret);
}
