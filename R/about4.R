#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
about4 <-
function(x)
{
x1=gsub("[|]",",",x);
x2=gsub("[{]","a=c(",x1);
x3=gsub("[}]",")",x2);
eval(parse(text=paste(x3)));
return(a[4:7]);
}
