#' Title
#'
#' @param data 
#' @param mu 
#' @param sig 
#'
#' @return
#' @export
#'
#' @examples
abllik <-
function(data,mu,sig)
	{
	x=data$X;y=data$Y;n=data$COUNT;
	x=rep(x,n); y=rep(y,n);
	i1=which(y==1);
	ll=log(n[i1]*pnorm((x[i1]-mu)/sig));
	ll=c(ll,log(n[-i1]*pnorm((mu-x[-i1])/sig)));
	return(sum(ll));
	}
