#' Title
#'
#' @param x 
#' @param nd0 
#' @param reso 
#' @param ln 
#'
#' @return
#' @export
#'
#' @examples
getxr <-
function(x,nd0,reso,ln)
{
buf=" ";
if(ln) x=exp(x);
if(nd0>9)buf="";
rx=round(x,5); if(reso > 0)rx=round(x/reso)*reso;
xx = paste(buf,nd0,". Test at X ~ ",rx,". Enter X & R: ", sep = "");
xx=readline(xx);
xx=as.numeric(unlist(strsplit(xx," ")));
tx=xx[1];
if(ln) xx[1]=round(log(tx),5);
return(c(xx,rx,tx));
}
