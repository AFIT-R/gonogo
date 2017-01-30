#' Title
#'
#' @param w 
#' @param k 
#'
#' @return
#' @export
#'
#' @examples
fixw <-
function(w,k=1)
{
# lop off the last k entries and resume test 
if(k < 1) return(w);
for(i in 1:k)w=fixw1(w)
return(w)
}
