#' Title
#'
#' @param rx 
#' @param ry 
#' @param m 
#' @param s 
#' @param uu 
#' @param levb 
#'
#' @return
#' @export
#'
#' @examples
clim <-
function(rx,ry,m,s,uu,levb)
{
done=0;
sigmax=.001;
xll=c(-5,5); yll=10;
len=50;
z0=matrix(rep(0,len^2),ncol=len);

while(done == 0)
{
xl=m+xll*s; yl=c(sigmax,yll*s);
x0=seq(xl[1],xl[2],length=len); y0=seq(yl[1],yl[2],length=len);
for(i in 1:len)for(j in 1:len) z0[i,j]=xyllik(rx,ry,x0[i],y0[j])/uu;
cl=contourLines(x0,y0,z0,levels=levb);
ncl=length(cl);
if(ncl > 0)
	{
	nxl=nyl=numeric(0);
	for(i in 1:ncl){g=cl[[i]];nxl=range(c(nxl,g$x));nyl=range(c(nyl,g$y));}
	done=1;
	if(nxl[1] == x0[1]) {xll[1]=1.5*xll[1]; done=0;}
	if(nxl[2] == x0[len]) {xll[2]=1.5*xll[2]; done=0;}
	if(nyl[1] == y0[1]) {sigmax=sigmax/1.5; done=0;}
	if(nyl[2] == y0[len]) {yll=1.5*yll; done=0;}
	} else
	{
	done=0;
	xll=1.5*xll; 
	yll=yll*1.5;
	sigmax=sigmax/1.5;
	}
}
return(c(nxl,nyl))
}
