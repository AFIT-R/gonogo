#' Title
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
simp <-
function(dat)
{
xx=dat$X; yy=dat$Y; n=dat$COUNT;
xx=rep(xx,n); yy=rep(yy,n);
sux=sort(unique(xx)); lsx=length(sux);
xxx=yyy=nnn=numeric(0);
for(i in 1:lsx)
{
i1=sum(yy[xx==sux[i]]); if(i1 > 0){xxx=c(xxx,sux[i]); yyy=c(yyy,1); nnn=c(nnn,i1);}
i0=sum(1-yy[xx==sux[i]]); if(i0 > 0){xxx=c(xxx,sux[i]); yyy=c(yyy,0); nnn=c(nnn,i0);}
}
dat=matrix(c(xxx,yyy,nnn),ncol=3); 
dat=data.frame(dat);
names(dat)=c("X","Y","COUNT");

return(dat)
}
