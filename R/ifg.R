#' Title
#'
#' @param fg0 
#' @param fg1 
#'
#' @return
#' @export
#'
#' @examples
ifg <-
function(fg0,fg1)
{
m1=4*exp((fg0+3*fg1)/4);
m0=4*exp((3*fg0+fg1)/4);
mhi=(3*m1-m0)/8;
mlo=(3*m0-m1)/8;
v=c(mlo,mhi);
return(v)
}
