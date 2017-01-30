#' Title
#'
#' @param rx 
#' @param ry 
#' @param levs0 
#' @param em 
#' @param es 
#'
#' @return
#' @export
#'
#' @examples
ulik <-
function(rx,ry,levs0,em,es)
{
shi=es; slo=es/2; val1=val2=xyllik(rx,ry,em,slo);
while(val1 > levs0) {shi=slo; slo=slo/2; val1=xyllik(rx,ry,em,slo);}
while(val2 < levs0) {slo=shi; shi=2*shi; val2=xyllik(rx,ry,em,shi);}
eps=.00001; delt=1;
while(delt > eps)
{
s=(slo+shi)/2;
val=xyllik(rx,ry,em,s);
if(val > levs0) shi=s else slo=s;
delt=abs(val-levs0);
}
s=(slo+shi)/2;
return(s);
}
