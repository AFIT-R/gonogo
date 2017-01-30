#' Title
#'
#' @param rx 
#' @param ry 
#' @param muhat 
#' @param levs0 
#' @param con 
#'
#' @return
#' @export
#'
#' @examples
otherpoint <-
function(rx,ry,muhat,levs0,con)
{
k=1.5; slo=Inf; shi=-Inf;
s=1;
while(slo > shi)
{
m=muhat-qnorm(con)*s;
val=xyllik(rx,ry,m,s);
if(val > levs0) {slo=s; s=k*s;} else {shi=s; s=s/k;}
}
eps=.00001;
delt=1;
while(delt > eps)
{
s=(slo+shi)/2;
m=muhat-qnorm(con)*s;
val=xyllik(rx,ry,m,s);
if(val > levs0) slo=s else shi=s;
delt=abs(val-levs0);
}
s=(slo+shi)/2;
m=muhat-qnorm(con)*s;
return(c(m,s));
}
