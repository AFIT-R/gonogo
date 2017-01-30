#' Title
#'
#' @param c1 
#' @param nu 
#' @param tau2 
#' @param p 
#' @param be 
#'
#' @return
#' @export
#'
#' @examples
skewL <-
function(c1,nu,tau2,p,be)
{
	# tau2 = the square of tau
	# nu = sqrt(tau2)*c1 where c1 = f3point8(lambda) (solving 3.8)
	# Compute a by 3.9 & 3.12 (below	u=E(Zn*M(Zn)) & v=E(M(Zn))	)
		
		j=qnorm(p)+be*nu;
		k=sqrt(1+be^2*tau2);
		v=pnorm(j/k);
		u=be*tau2*dnorm(j/k)/k+nu*v;
		a=(u-nu*v)/(v*(1-v));

	# Compute next tau2 by 3.4 & 3.12

		ntau2=a^2*v*(1-v)-2*a*(u-nu*v)+tau2;

	# Compute next nu

		nnu=sqrt(ntau2)*c1;

	# Compute next b by 3.10 & 3.12
	
		b=v-(nu-nnu)/a;

	return(c(j,k,v,u,a,ntau2,nnu,b))
}
