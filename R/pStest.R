#' Plots Simulation version
#'
#' @param dat sensitivity tests "w"
#' @param plt 8 plots are included
#' 1: History plot
#' 2: MLE's of mu and sigma
#' 3: Response curve, with data
#' 4: A simple visual of the data
#' 5: Joint LR multi-confidence bounds
#' 6: Joint & Individual LR multi-confidence bounds
#' 7: Joint and/or individual LR confidence bounds
#' 8: Confidence bounds on probability (p) and quantile (q) computed via 3 methods: Likelihood Ratio (LR), Fisher Matrix (FM) and General Linear Model (GLM)
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
