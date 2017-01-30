#' Title
#'
#' @param mydata 
#' @param mu 
#' @param sig 
#' @param response 
#'
#' @return
#' @export
#'
#' @examples
llik <-
function(mydata, mu, sig, response = 1)
{
	# Remove rows of data having NA's in them
	mydata=na.omit(mydata)
	x = mydata$X
	y = mydata$Y
	n = mydata$COUNT
	if(response == 0.) y = abs(y - 1.);
	i1 = which(y == 1)
	eps = 1e-006
	if(sig < eps) sig = eps
	ll = n[i1] * log(pnorm((x[i1] - mu)/sig))
	ll = c(ll, n[ - i1] * log(1 - pnorm((x[ - i1] - mu)/sig)))
	a=sum(ll); if(is.na(a))a=-Inf;
	return(a)
}
