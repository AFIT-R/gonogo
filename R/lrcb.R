#' Title
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
lrcb <-
function(dat)
{
ncl=nclu=0;

xx="Enter conf1's (separated by blanks): ";
xx=readline(xx); cat("\n");
xx=as.numeric(unlist(strsplit(xx," ")));
conf1=sort(unique(xx));
conf2=pchisq(qchisq(conf1,1),2);
if(any(conf1 <= 0) | any(conf1 >= 1)) {cat("All conf1's must be between 0 & 1\n"); return();}

xx="Enter p and q (one must be 0): ";
xx=readline(xx); cat("\n");
xx=as.numeric(unlist(strsplit(xx," ")));

# Prepare (len) X (len) Grids (z's) for Response Surface
	len=401; 	
	z0=matrix(rep(0,len*len),ncol=len);
	siglow=.001;
	meth=1;
	tit1=dat$title; dat=dat$d0; 
	
# Compute muhat & sighat if there's overlap

rx=rep(dat$X,dat$COUNT); ry=rep(dat$Y,dat$COUNT); ny=length(ry);
m1=min(rx[ry==1]);M0=max(rx[ry==0]);
if(m1 == M0 & ny == 2){cat("More data is needed to compute valid confidence regions\n\n");return();}
if(m1 <= M0)overlap=T else overlap=F;
sigmin=.001;
if(overlap)
	{
	if(m1 < M0)
		{
		xglm=glm(ry~rx,family=binomial(link=probit),maxit=10.0,epsilon=1e-006);	
		ab=xglm$coef;
		sighat=1/ab[2];
		muhat=-ab[1]*sighat;
		uu=xyllik(rx,ry,muhat,sighat)
		} else
		{
		muhat=m1; sighat=sigmin; mx=ry[rx == m1]; s1=sum(mx); l1=length(mx);
		uu=s1*log(s1)+(l1-s1)*log(l1-s1)-l1*log(l1);
		}
	} else 
		{
		muhat=(m1+M0)/2; sighat=(m1-M0)/2;  
		nconf2=(3+pchisq(qchisq(conf1,1),2))/4;
		uu=1;
		}

#(qq,pp) is a point on MLE RESPONSE CURVE
if(xx[1] > 0 & xx[1] < 1) {pp=xx[1]; qq=muhat+qnorm(pp)*sighat;}
if(xx[1] == 0) {qq=xx[2]; pp=((qq-muhat)/sighat);}

nobo=F; pcl=T;
# Rough calculation of limits
if(overlap)
{
	levs=1-qchisq(conf1,1)/(2*uu);
	con=sum(ry)/length(ry); 
	llc=sum(log(con^ry*(1-con)^(1-ry)));
	c1max=pchisq(2*(uu-llc),1);
	bcon1=conf1[conf1 < c1max];
	ucon1=conf1[conf1 >= c1max];
	nu1=length(ucon1);
	endpr=paste("All conf1's are > c1max (",round(c1max,5),")\n\n",sep="");
	# Address all unbounded contours
	if(length(bcon1) == 0)  
		{
		cat(endpr); 
		nobo=T;
		}
	if(!nobo) bconm=max(bcon1,na.rm=T) else bconm=c1max/2;;
	levm=1-qchisq(bconm,1)/(2*uu);
	a=clim(rx,ry,muhat,sighat,uu,levm);
} else
	{
	uu=1;
	levs=(1-conf2)/4;
	con=sum(ry)/length(ry); 
	lc=prod(con^ry*(1-con)^(1-ry));
	c2max=1-4*lc; 
	c1max=pchisq(qchisq(c2max,2),1);
	bcon2=conf2[conf2 < c2max];
	ucon2=conf2[conf2 >= c2max];
	nu1=length(ucon2);
	endpr=paste("All conf2's are > conf2 (",round(c2max,5),")\n\n",sep="");
	# Address all unbounded contours
	if(length(bcon2) == 0)  
		{
		cat(endpr); 
		nobo=T; pcl=F;
		}
	if(!nobo) bconm=max(bcon2,na.rm=T) else bconm=c2max/2;
	levm=(1-bconm)/4; 
	a=clim0(rx,ry,muhat,sighat,levm);
}

# Expand limits a tad
sigmax=0;
a1=c(floor(a[1]), ceiling(a[2]), min(sigmax, .1*a[3]), ceiling(a[4]))+c(-1,1,0,1);
x0=seq(a1[1],a1[2],length=len); y0=seq(a1[3],a1[4],length=len);
if(meth == 1){for(i in 1:len)for(j in 1:len)	z0[i,j]=xyllik(rx,ry,x0[i],y0[j])/uu;}
if(meth==2){for(i in 1:len)for(j in 1:len)	z0[i,j]=uu-xyllik(rx,ry,x0[i],y0[j]);}
if(!overlap)z0=exp(z0);

# Neyer CL's provided on his (Mu,Sig) contour plot
# Levels of z0 relate to conf by: -2 * log( exp(xyllik)/exp(uu) ) >= qchisq(conf,2)
# levs=1-qchisq(conf2,2)/(2*uu); cl=contourLines(x0,y0,z0,levels=levs);

if(!nobo) {if(overlap)levb=1-qchisq(bcon1,1)/(2*uu) else levb=(1-bcon2)/4;}
if(nobo) {if(overlap)levb=1-qchisq(bconm,1)/(2*uu) else levb=(1-bconm)/4;}

cl=contourLines(x0,y0,z0,levels=levb); ncl=length(cl);

nxl=nyl=numeric(0);

		if(!nobo) {for(i in 1:ncl){g=cl[[i]];nxl=range(c(nxl,g$x));nyl=range(c(nyl,g$y));}}

if(nu1 > 0)
	{
	if(overlap) levu=1-qchisq(ucon1,1)/(2*uu) else levu=(1-ucon2)/4
	clu=contourLines(x0,y0,z0,levels=levu);
	nclu=length(clu);
	if(nclu > 0)for(i in 1:nclu){g=clu[[i]];nxl=range(c(nxl,g$x));nyl=range(c(nyl,g$y));}
	}

nco=length(conf1);
#-----------------------------------------------------------------------------
# Limits for plot 
xl=yl=numeric(0);
if(!nobo)for(i in 1:ncl){g=cl[[i]];xl=range(c(xl,g$x));yl=range(c(yl,g$y));}
if(nclu > 0) {for(i in 1:nclu){g=clu[[i]]; my=min(g$y); yl=range(c(yl,my));}}
if(!pcl)
{
xl=yl=numeric(0);
for(i in 1:nclu){g=clu[[i]];xl=range(c(xl,g$x));yl=range(c(yl,g$y));}
}

xll=c(floor(xl[1]),ceiling(xl[2]));
yll=c(floor(yl[1]),ceiling(yl[2]));
pxl=pretty(xl); pyl=pretty(yl);

par(mfrow = c(2, 2), oma = c(0,.4,2,0),  mar = c(3,3,2,1));
#-----------------------------------------------------------------------------
ex=cl[[1]]$x; ey=cl[[1]]$y;

dtp="l"; if(nobo) dtp="n";
plot(ex,ey,type=dtp,xlim=xl[1:2],ylim=yl[1:2],xlab="",ylab="",xaxt="n",yaxt="n");
ilt=3; abline(v=pxl,lty=ilt); abline(h=pyl,lty=ilt);
points(muhat,sighat,pch=16,cex=.7,col=2);
if(nu1 > 0)
	{
	for(i in 1:length(clu)){exx=clu[[i]]$x; eyy=clu[[i]]$y; points(exx,eyy,type="l",col=2);}
	}

axis(1,at=pxl,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pyl,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext("m",side=1,line=1.3,cex=.9);
mtext("s",side=2,line=2.5,cex=.9,las=2);

if(ncl > 1 & !nobo)for(i in 1:ncl){ex=cl[[i]]$x; ey=cl[[i]]$y; points(ex,ey,type="l");}

b0=round(conf2,3);
b0=paste(b0,collapse=",")
b0=gsub("0.",".",b0,fixed=T); 
w1=w2="";
if(nco > 1) {w1="("; w2=")";}
b0=paste("c2=",w1,b0,w2,sep="");

mtext(b0,side=3,line=.6,cex=.8);
con=sum(ry)/length(ry); llc=sum(log(con^ry*(1-con)^(1-ry)));
m0=-1/qnorm(con);
if(con == .5) abline(v=muhat,col=1,lty=2) else abline(sighat-m0*muhat,m0,col=1,lty=2);
rngx=xl; rngy=yl;

#-----------------------------------------------------------------------------
# Limits for plot 2
xl=numeric(0);

for(i in 1:ncl){g=cl[[i]];xl=range(c(xl,g$x+qnorm(pp)*g$y));}

if(!pcl)
{
xl=numeric(0);
for(i in 1:nclu){g=clu[[i]];xl=range(c(xl,g$x+qnorm(pp)*g$y));}
}

xll=c(floor(xl[1]),ceiling(xl[2]));
pxl2=pretty(xl);
#-----------------------------------------------------------------------------
ex=cl[[1]]$x+qnorm(pp)*cl[[1]]$y; 
ey=cl[[1]]$y;

plot(ex,ey,type=dtp,xlim=xl,ylim=yl[1:2],xlab="",ylab="",xaxt="n",yaxt="n");
ilt=3; abline(v=pxl2,lty=ilt); abline(h=pyl,lty=ilt);
points(qq,sighat,pch=16,cex=.7,col=2);

axis(1,at=pxl2,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pyl,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext("q",side=1,line=1.3,cex=.9);

if(ncl > 1 & pcl)for(i in 1:ncl){ex=cl[[i]]$x+qnorm(pp)*cl[[i]]$y;; ey=cl[[i]]$y; points(ex,ey,type="l");}
if(nu1 > 0)for(i in 1:length(clu)){exx=clu[[i]]$x+qnorm(pp)*clu[[i]]$y;; eyy=clu[[i]]$y; points(exx,eyy,type="l",col=2);}
b1=round(conf1,3);
b1=paste(b1,collapse=",")
b1=gsub("0.",".",b1,fixed=T);
b1=paste("c1=",w1,b1,w2,sep="");
b1=paste("p=",round(pp,3),", ",b1,sep="");
b1=gsub("=0.","=.",b1,fixed=T);

mtext(b1,side=3,line=.6,cex=.8);
if(pp == con) abline(v=muhat+qnorm(pp)*sighat,col=1,lty=2) else
	{ 
	m3=1/(qnorm(pp)+1/m0);
	abline(sighat-m3*(muhat+qnorm(pp)*sighat),m3,col=1,lty=2);
	}

#-----------------------------------------------------------------------------
#Limits for plot 3
xl=numeric(0);

for(i in 1:ncl){g=cl[[i]];xl=range(c(xl,pp,pnorm((qq-g$x)/g$y)));}

xll=c(floor(xl[1]),ceiling(xl[2]));
pxl3=pretty(xl);
#-----------------------------------------------------------------------------
ex=pnorm((qq-cl[[1]]$x)/cl[[1]]$y); 
ey=cl[[1]]$y;

plot(ex,ey,type=dtp,xlim=xl,ylim=yl[1:2],xlab="",ylab="",xaxt="n",yaxt="n");
ilt=3; abline(v=pxl3,lty=ilt); abline(h=pyl,lty=ilt);
points(pp,sighat,pch=16,cex=.7,col=2);

axis(1,at=pxl3,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pyl,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext("p",side=1,line=1.3,cex=.9);
mtext("s",side=2,line=2.5,cex=.9,las=2);

if(ncl > 1 & pcl)for(i in 1:ncl){ex=pnorm((qq-cl[[i]]$x)/cl[[i]]$y); ey=cl[[i]]$y; points(ex,ey,type="l");}
if(nu1 > 0)for(i in 1:length(clu)){exx=pnorm((qq-clu[[i]]$x)/clu[[i]]$y); eyy=clu[[i]]$y; points(exx,eyy,type="l",col=2);}

b1=round(conf1,3);
b1=paste(b1,collapse=",")
b1=gsub("0.",".",b1,fixed=T);
b1=paste("c1=",w1,b1,w2,sep="");
b1=paste("q=",round(qq,3),", ",b1,sep="")
mtext(b1,side=3,line=.6,cex=.8);
abline(v=con,col=1,lty=2);
#-----------------------------------------------------------------------------
# linearized probability plot (for a single contour cl[[icl]])
{
al=10^(6:2); al=c(1/al,1-1/al); al=c(al,seq(25,975,by=25)/1000); al=sort(al);
nal=length(al);
lrmat1=matrix(rep(0,6*nal),ncol=6);
lrmat1[,5]=al;
lrmat1[,2]=muhat+qnorm(al)*sighat;
lra=list(1);
limx=numeric(0);
if(nobo) ncl=0;
for(j in 1:(ncl+nclu))
{
if(j > ncl) {x=clu[[j-ncl]]$x; y=clu[[j-ncl]]$y;} else {x=cl[[j]]$x; y=cl[[j]]$y;}
for(i in 1:nal)
	{
	lrmat1[i,c(1,3)]=lim=range(x+qnorm(lrmat1[i,5])*y,finite=T);
	if(i > 2 & i < 48) limx=range(c(limx,lim));
	lrmat1[i,c(4,6)]=range(pnorm((lrmat1[i,2]-x)/y),finite=T);
	}
lra[[j]]=lrmat1
}

grafl(limx);
for(j in (ncl+nclu):1)
{
u=lra[[j]]; 
if(j > ncl) pcol=2 else pcol=1; 
lines(u[,1],qnorm(u[,5]),type="l",col=pcol);
lines(u[,3],qnorm(u[,5]),type="l",col=pcol);
}
lines(u[,2],qnorm(u[,5]),type="l",col=8);
cn=c("ql","q","qh","pl","p","pu");
options(scipen=999);
write.table(round(lra[[1]],6),file="lrcb.txt",quote=F,sep=",",na="i",
col.names=cn,row.names=F);
if(ncl > 1)for(j in 2:ncl)
{
suppressWarnings(write.table(round(lra[[j]],6),file="lrcb.txt",quote=F,sep=",",na="i",append=T,
col.names=cn,row.names=F));
}
options(scipen=0);
}
# main title
par(mfrow=c(1,1));
par(oma = c(0,0,1,0),  mar = c(5,4,4,2)+.1);
tit1=paste(tit1," (c1max =",round(c1max,5),")",sep="");
mtext(tit1,side=3,line=-.6,outer=T);
return();
}
