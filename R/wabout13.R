#' Title
#'
#' @param M 
#' @param cmlo 
#' @param cmhi 
#' @param csg 
#' @param p 
#' @param n11 
#' @param n12 
#' @param n2 
#' @param n3 
#' @param lam 
#' @param reso 
#'
#' @return
#' @export
#'
#' @examples
wabout13 <-
function(M,cmlo,cmhi,csg,p,n11,n12,n2,n3,lam,reso)
{
g=", "; f=",";
cp=as.character(p); cp=gsub("0.",".",cp); 
cl=as.character(lam); cl=gsub("0.",".",cl); 
cr=as.character(reso); cr=gsub("0.",".",cr);
#about=paste("{",M,"|",cmlo,f,cmhi,f,csg,"|",n11,f,n12,f,n2,f,n3,"|",cp,f,cl,f,cr,"}",sep="");
if(M == 1) about=paste("{",cmlo,f,cmhi,f,csg,"|",n11,f,n12,f,n2,f,n3,"|",cp,f,cl,f,cr,"}",sep="") else
about=paste("{",cmlo,f,cmhi,f,csg,"|",n11,f,n12,f,n2,f,n3,"|",cp,f,cl,f,cr,f,M,"}",sep="");
return(about)
}
