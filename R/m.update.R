#' Title
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
m.update <-
function(dat)
{
n1=sum(dat[,2]);
n0=ncol(t(dat))-n1;
M0=m1=NA
if(n0 > 0) M0=max(dat[dat[,2]==0,1],na.rm=T);
if(n1 > 0) m1=min(dat[dat[,2]==1,1],na.rm=T);
ret=list(M0,m1);
names(ret)=c("M0","m1");
return(ret);
}
