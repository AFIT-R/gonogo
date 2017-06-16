#' Support function
#'
#' @param dat
#' @param mu
#' @param sig
#'
#' @return
#' @export
#'
#' @examples
yinfomat <-
function(dat, mu, sig)
{
	n = dat$COUNT
	k = (dat$X - mu)/sig
	p = pnorm(k) * (1 -pnorm(k))

	z = dnorm(k)
	v = n*z^2/p
	v[which(v == Inf)]=0

	# z, e.g., z = 4.881666e-226, is s.t. z^2 == 0 exactly
	# so that, when p == 0 exactly, v = 0/0 = NA
	# Therefore, have to remove the NA's if there are any
	iy=which(is.na(v))
	if(length(iy) > 0){v=v[-iy]; k=k[-iy];}

	b11= sum(v)
	b21 = b12 = sum(v*k)
	b22 = sum(v*k^2)

	# FISHER INFORMATION MATRIX
	infm = matrix(c(b11, b12, b21, b22), nrow = 2, byrow = T)
	# DETERMINANT OF INFORMATION MATRIX
	deti = det(infm)
	# VARIANCE COVARIANCE MATRIX
	vcov1 = solve(infm)
	# CORRELATION COEFFICIENT
	rho=vcov1[1,2]/sqrt(vcov1[1,1]*vcov1[2,2]);
	xx=list(vcov1,infm,deti,rho);
	names(xx)=c("vcov1","infm","deti","rho");
	return(xx)
}
