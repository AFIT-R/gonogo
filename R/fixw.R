#' @title fixw
#'
#' @description An error correction feature: Suppose an error was inadvertently entered at the nth previous console read.
#' The syntax to go back, fix and continue is: z=fixw(w,n); w=gonogo(newz=F)
#'
#' @param w A sensitivity test saved as a list produced by \code{gonogo}
#' @param k A positive integer to indicate the last \code{k} entries to fix
#'
#' @return The updated list of the sensitivity test
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
