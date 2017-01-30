#' Title
#'
#' @param x 
#' @param l 
#'
#' @return
#' @export
#'
#' @examples
f38 <-
function(x,l)
{
a=(l-1)*pnorm(-x)+1;
b=(l-1)*dnorm(-x);
h=a*x-b;
return(h);
}
