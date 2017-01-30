#' Title
#'
#' @param about 
#' @param s47 
#' @param loc 
#'
#' @return
#' @export
#'
#' @examples
chabout <-
function(about,s47,loc)
{
x=about;
x1=gsub("[|]",",",x);
x2=gsub("[{]","vv=c(",x1)
x3=gsub("[}]",")",x2)
eval(parse(text=paste(x3)))
#vv[loc]=val;
vv[loc]=s47;
a=wabout(vv);
return(a)
}
