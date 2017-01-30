#' Title
#'
#' @param x 
#' @param mu 
#' @param sig 
#' @param reso 
#' @param ln 
#' @param iset 
#'
#' @return
#' @export
#'
#' @examples
gxr <-
function(x,mu,sig,reso,ln,iset)
{
if(iset > 0) set.seed(iset);
xsav=x;
if(ln) x=exp(x);
rx=round(x,5); if(reso > 0)rx=round(x/reso)*reso;
# rx=the stress; Choose a random strength xx from N(mu,sig)
if(ln) xx=rlnorm(1, meanlog = mu, sdlog = sig) else xx=mu+rnorm(1)*sig;
# If xx (strength) is bigger than rx (stress) then r=0; 
r=0; if(xx <= rx) r=1;
tx=round(xx,5);
if(ln) x=log(x)
return(c(xsav,r,rx,tx));
}
