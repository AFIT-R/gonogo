#' Title
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
mixed <-
function(dat)
{
	min1=min(dat$X[which(dat$Y==1)]);
	max0=max(dat$X[which(dat$Y==0)]);
	dif=min1-max0;		# dif < 0 <=> OVERLAP or ZONE OF MIXED RESULTS
	summ=min1+max0;
	con=sum(dat$Y)/length(dat$Y);
	result=list(min1,max0,summ,dif,con)
	names(result)=c("min1","max0","summ","dif","con");
	return(result); 
}
