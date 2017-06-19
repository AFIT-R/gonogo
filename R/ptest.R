#' Plots Console Version
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
#' Besides a plot, ptest(w,8) also creates a text file cbs.txt in your R working directory.
#' What you may want do with cbs.txt (outside of R) is: copy its contents, paste it into a word document,
#' and highlight what you just pasted. Next, select the Table tab; select Table; and select Convert Text to Table.
#' This will produce a table that will yield: vertical limits (pl,pu) about p (for various p's)
#' and horizontal limits (ql,qu) about q (for various q's).
#' The first 15 lines are FM limits; the next 15 lines are LR limits; the last 15 lines are GLM limits.
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
