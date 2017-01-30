#' Title
#'
#' @param dat 
#' @param conf1 
#'
#' @return
#' @export
#'
#' @examples
lr.lims <-
function(dat,conf1)
{
# Check if there's a ZMR, and calculate muhat, sighat, and llik
# If No ZMR, muhat=mid point, sighat=.0001
	chk=mixed(dat); 
	dif=chk$dif; muhat=chk$summ/2; sighat=0.0001; min1=chk$min1; max0=chk$max0; con=chk$con;
	conf2=pchisq(qchisq(conf1,1),2);
	if(dif >= 0) conf2=(conf2+3)/4;

	if(dif < 0)
		{
		rx=rep(dat$X,dat$COUNT); ry=rep(dat$Y,dat$COUNT); 
		xglm=glm(ry~rx,family=binomial(link=probit),maxit=10.0,epsilon=1e-006);	
		ab=as.vector(xglm$coef);
		muhat=-ab[1]/ab[2];
		sighat=1/ab[2];
		}
		
		uu=llik(dat,muhat,sighat);
	
# conf2=pchisq(qchisq(conf1,1),2); # qchisq(conf2,2) == qchisq(conf1,1); # conf1=pchisq(qchisq(conf2,2),1);
# Unravel the following equations: -2 * log( exp(llik)/exp(uu) ) >= qchisq(conf,2)
# levs0=uu-qchisq(conf2,2)/2; levs1=uu-qchisq(conf1,1)/2; 
# levs0 & levs1 (both above) & levs0 (below) are all the same 

	levs0=uu+log(1-conf2); levs1=levs2=uu-qnorm((1-conf1)/2)^2/2;

options(warn=-1); # SUPPRESSES OUT OF BOUNDS WARNINGS (AND OTHERS)

# Contour (cx,cy): llik=levs0 needed for Graphs 1, 2 & 3
degs=180;
if(dif > 0)degs=360;
st=ct=cx=cy=tr=(0:360)*pi/degs;

x0=as.vector(muhat); 
y0=ru=as.vector(sighat);
eps=0.000001; eps4=.0001;
ibot=2; itop=361;
if(dif > 0){cy[1]=cy[361]=0;cx[1]=min1;cx[361]=max0;ibot=2;itop=360;ru=1;}
	
for(i in 0:361) {st[i]=sin(tr[i]); ct[i]=cos(tr[i]);}

for(i in 1:181)
{
xl=x0; yl=y0;
xu=x0+ru*ct[i]; yu=y0+ru*st[i];
while(llik(dat,xu,yu)>levs0){xl=xu; yl=yu; xu=xu+ct[i];yu=yu+st[i];}
x=(xl+xu)/2; y=(yl+yu)/2;

lval=llik(dat,x,y);
zz=abs(lval-levs0)

while(zz>eps)
{
if(lval>levs0){xl=x;yl=y;} else {xu=x;yu=y;}
x=(xl+xu)/2; y=(yl+yu)/2
lval=llik(dat,x,y);
zz=abs(lval-levs0)
}
cx[i]=(xl+xu)/2;cy[i]=(yl+yu)/2;
}

for(i in 182:360)
{
dm=ct[i]*y0/st[i]; i2=25; i1=i2-1;
xu=x0+dm*(1-i1/i2); yu=y0*i1/i2;
while(llik(dat,xu,yu)>levs0){i2=i2+1; xl=xu; yl=yu; xu=x0+dm*(1-i1/i2); yu=y0*i1/i2;}

x=(xl+xu)/2; y=(yl+yu)/2;

lval=llik(dat,x,y);
zz=abs(lval-levs0)

while(zz>eps)
{
if(lval>levs0){xl=x;yl=y;} else {xu=x;yu=y;}
x=(xl+xu)/2; y=(yl+yu)/2
lval=llik(dat,x,y);
zz=abs(lval-levs0)
}
cx[i]=(xl+xu)/2;cy[i]=(yl+yu)/2;
}
cx=c(cx[1:181],cx[360:182]);
cy=c(cy[1:181],cy[360:182]);
cx[361]=cx[1]; cy[361]=cy[1];

a1=c(1,10,100,1000,10000,100000,250000)/1000000;
lrmat=matrix(rep(0,90),ncol=6);lrmat[,5]=c(a1,.5,sort(1-a1));
lrmat[,2]=muhat+qnorm(lrmat[,5])*sighat;	
for(i in 1:15)
{
	lrmat[i,1]=min(cx+qnorm(lrmat[i,5])*cy,na.rm=T); 
	lrmat[i,3]=max(cx+qnorm(lrmat[i,5])*cy,na.rm=T);
	lrmat[i,4]=min(pnorm((lrmat[i,2]-cx)/cy),na.rm=T); 
	lrmat[i,6]=max(pnorm((lrmat[i,2]-cx)/cy),na.rm=T);
}
ret=list(cx,cy,lrmat,muhat,sighat,dif,con);
names(ret)=c("cx","cy","lrmat","muhat","sighat","dif","con");
return(ret);
}
