#' Title
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
picdat <-
function(dat)
{
titl=dat$title; dat=dat$d0;
# put dat$d0 into simplified form (n may not be all 1's)
dat=simp(dat);
xx=dat$X; yy=dat$Y; n=dat$COUNT;

l0=l1=numeric(0);
if(any(yy==1)){m1=min(xx[yy==1]);l1=1;}
if(any(yy==0)){M0=max(xx[yy==0]);l0=1;}

del=.025; xr=range(xx); pm=c(-1,1); xl=xr+diff(xr)*pm/100; 
del=.03;
yl=c(0,1);
par(mar=c(0,0,0,0),pin=c(2.4,1.6));
plot(xx,yy,type="n",axes=F,ylim=yl,xlim=xr,xlab="",ylab="")
lines(c(xl[1],xl[2]),c(0,0),lty=3); lines(c(xl[1],xl[2]),c(1,1),lty=3);
cx=.6;
points(xx,yy,pch=16,cex=cx)
points(m1,1,pch=16,col=2,cex=cx); points(M0,0,pch=16,col=2,cex=cx);

for(i in 1:length(xx))
{
for(j in 1:n[i]) points(xx[i],yy[i]-sign(yy[i]-.5)*(j-1)*del,pch=16,cex=cx)
}
if(l0*l1 > 0 & m1 <= M0){lines(c(m1,m1),c(0,1),lty=3); lines(c(M0,M0),c(0,1),lty=3);}
reset();
return();
}
