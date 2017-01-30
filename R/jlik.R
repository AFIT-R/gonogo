#' Title
#'
#' @param rx 
#' @param ry 
#' @param levs0 
#' @param ms 
#' @param op 
#' @param one23 
#'
#' @return
#' @export
#'
#' @examples
jlik <-
function(rx,ry,levs0,ms,op,one23)
{
# Contour (cx,cy): llik=levs0 needed for Graphs 1, 2 & 3

ndeg=361; ang=0; h=1; 
if(one23 == 2) {d=op-ms; d7=(op+ms)/2; ang=atan(d[1]/d[2])+pi/2;}
if(one23 == 3) h=2;
cx=cy=tr=2*(0:(ndeg-1))*pi/(h*(ndeg-1))-ang;

rl=0;
if(one23 == 1) 
{
ibot=1; itop=ndeg; 
x0=as.vector(ms[1]); y0=ru=as.vector(ms[2]);
}
if(one23 == 2) 
{
ibot=2; itop=ndeg-1;
x0=as.vector(d7[1]); y0=ru=as.vector(d7[2]);
cx[1]=cx[ndeg]=ms[1]; 
cy[1]=cy[ndeg]=ms[2];
}
if(one23 == 3) 
{
ibot=2; itop=ndeg-1;
x0=ms[1]; y0=0; ru=1;
cx[1]=op[1]; cx[ndeg]=op[2]; 
cy[1]=cy[ndeg]=0; 
}

eps=0.000001;
        
for(i in ibot:itop)
{
st=sin(tr[i]); 
ct=cos(tr[i]);

xl=x0+rl*ct;
yl=y0+rl*ct;

xu=x0+ru*ct; yu=y0+ru*st;
while(xyllik(rx,ry,xu,yu)>levs0){xu=xu+ct;yu=yu+st;}
x=(xl+xu)/2; y=(yl+yu)/2;
lval=xyllik(rx,ry,x,y);
zz=abs(lval-levs0)

while(zz>eps)
{
if(lval>levs0){xl=x;yl=y;} else {xu=x;yu=y;}
x=(xl+xu)/2; y=(yl+yu)/2
lval=xyllik(rx,ry,x,y);
zz=abs(lval-levs0)
}
cx[i]=(xl+xu)/2;cy[i]=(yl+yu)/2;
}
return(list(cx,cy))
}
