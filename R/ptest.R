#' Plots Console Version
#'
#' @param dat A sensitivity test saved as a list produced by \code{gonogo}
#' @param plt An integer between 1 and 8 to indicate which plot is wanted:
#' \enumerate{
#' 		\item 1: History plot
#' 		\item 2: MLE's of mu and sigma
#' 		\item 3: Response curve (with data), the pooled adjacent violators (PAV) solution (an optimal step function solution),
#' 		and 95\% 1-sided confidence bounds computed via \code{glm}
#' 		\item 4: A simple visual of the data
#' 		\item 5: Joint LR multi-confidence bounds
#' 		\item 6: Joint & Individual LR multi-confidence bounds
#' 		\item 7: Joint and/or individual LR confidence bounds
#' 		\item 8: Confidence bounds on probability (p) and quantile (q) computed via 3 methods:
#' 		Likelihood Ratio (LR), Fisher Matrix (FM) and General Linear Model (GLM)
#' }
#'
#' pdat1,2,3 missing? no, they are in the file plotting_functions.R
#'
#' @return
#' @export
#'
#' @examples
ptest <-
function(dat,plt)
{
if(plt == 1) {pdat1(dat); return();}
if(plt == 2) {v=pdat2(dat); return(v);}
if(plt == 3) {pdat3(dat); return();}
if(plt == 4) {picdat(dat); return();}
if(plt == 5) {v=jlrcb(dat); return(v);}
if(plt == 6) {v=lrcb(dat); return(v);}
if(plt == 7) {v=cbs(dat,plt); return(v);}
if(plt == 8) {v=cbs(dat,plt); return(v);}

u=paste("plt must be 1, 2, 3, 4, 5, 6, 7 or 8.\nTry again.\n\n",sep="");
cat(u);
return()
}
