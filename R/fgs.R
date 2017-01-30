#' Title
#'
#' @param mlo 
#' @param mhi 
#' @param sg 
#'
#' @return
#' @export
#'
#' @examples
fgs <-
function(mlo,mhi,sg)
{
fg0=log((mhi+3*mlo)^3/(16*(3*mhi+mlo)))/2; 
fg1=log((3*mhi+mlo)^3/(16*(mhi+3*mlo)))/2;
fsg=(fg1-fg0)/7;
u=c(fg0,fg1,fsg);
return(u)
}
