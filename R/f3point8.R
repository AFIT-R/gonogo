#' Title
#'
#' @param l 
#'
#' @return
#' @export
#'
#' @examples
f3point8 <-
function(l)
{
if(l <= 0 | l == 1) return(0);
x=0; del=0.2; h=1; 
v=f38(x,l);
# l > 1
if(v < 0)
{
while(h > 0)
{
ll=x;
x=x+del;
h=f38(x,l)*v;
}
ul=x;
}

# l < 1
if(v > 0)
{
while(h > 0)
{
ul=x;
x=x-del;
h=f38(x,l)*v;
}
ll=x;
}

eps=.000001;
w=10;
m=(ll+ul)/2;
while(abs(w) > eps)
{
w=f38(m,l);

if(w < 0) ll=m else ul=m;

m=(ll+ul)/2;
}
m=(ll+ul)/2;
return(m);
}
