#' Title
#'
#' @param confv 
#' @param nam 
#'
#' @return
#' @export
#'
#' @examples
mkb0 <-
function(confv,nam)
{
# adapted from llik1.R
ncl=length(confv);
b0=round(confv,3);
b0=paste(b0,collapse=",")
b0=gsub(",0.",",.",b0,fixed=T);
b0=gsub("0.",".",b0,fixed=T);
w1=w2="";
if(ncl > 1) {w1="("; w2=")";}
b0=paste(nam," = ",w1,b0,w2,sep="");
return(b0)
}
