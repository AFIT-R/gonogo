#' Title
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
n.update <-
function(dat)
{
n1=sum(dat[,2]);
n0=ncol(t(dat))-n1;
ret=list(n0,n1);
names(ret)=c("n0","n1");
return(ret);
}
