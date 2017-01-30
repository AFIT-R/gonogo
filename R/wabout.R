#' Title
#'
#' @param vv 
#'
#' @return
#' @export
#'
#' @examples
wabout <-
function(vv)
{
f=",";
c=sub("^(-?)0.", "\\1.", vv);
about=paste("{",c[1],f,c[2],f,c[3],"|",c[4],f,c[5],f,c[6],"|",c[7],f,c[8],f,c[9],"}",sep="");
return(about);
}
