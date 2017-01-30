#' Title
#'
#' @param dat 
#' @param plt 
#'
#' @return
#' @export
#'
#' @examples
pStest <-
function(dat,plt)
{
if(plt == 1) {pSdat1(dat); return();}
if(plt == 2) {v=pSdat2(dat); return(v);}
if(plt == 3) {pSdat3(dat); return();}
if(plt == 4) {picdat(dat); return();}
if(plt == 5) {v=jlrcb(dat); return(v);}
if(plt == 6) {v=lrcb(dat); return(v);}
if(plt == 7) {v=cbs(dat,plt); return(v);}
if(plt == 8) {v=cbs(dat,plt); return(v);}
u=paste("plt must be 1, 2, 3, 4, 5, 6, 7, or 8.\nTry again.\n\n",sep="");
cat(u);
return()
}
