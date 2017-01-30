#' Title
#'
#' @param kn 
#' @param b 
#'
#' @return
#' @export
#'
#' @examples
ydeldet <-
function(kn, b)
{
	p = pnorm(kn)
	q = sqrt(p * (1 - p))
	bb = b[1, 1] * kn^2 - 2 * b[1, 2] * kn + b[2, 2]
	d = dnorm(kn)/q
	return(bb * d^2)
}
