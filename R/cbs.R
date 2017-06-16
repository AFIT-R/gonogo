#' Confidence Bounds Plots
#'
#' @param w sensitivity tests "w"
#' @param plt
#' cbs(w,1) = joint and/or individual lr confidence bound
#' cbs(w,2) = confidence bounds on probability (p) and quantile (q) computed via 3 methods:
#' Likelihood Ratio (LR), Fisher Matrix (FM), and General Linear Model (GLM)
#' @param maxitt
#' @param response
#'
#' @return
#' @export
#'
#' @examples
cbs <-
function(w,plt,maxitt=10,response=1)
{
# gs for graph sheet 1 or 2
# gs = 1 with neither pp nor qq ==> jms = 1 for (mu sig contour plot)
# gs = 1 with a valid pp or qq ==> jms = 3 for 3 plots on one
# gs = 2

fmmat=spmat=lrmat=matrix(rep(NA,6*15),ncol=6);
mat=matrix(rep(NA,3*11*6),ncol=6);

gs=0;
if(plt == 1 | plt == 7) gs=1;
if(plt == 2 | plt == 8) gs=2;
if(gs == 0) return();

if(gs == 1)
{
	cflag=0;
	xx="Enter conf1 and conf2 (one must be 0): ";
	xx=readline(xx); cat("\n");
	xx=as.numeric(unlist(strsplit(xx," ")));
	if(length(xx) != 2) cflag=-1;
	if(cflag == 0)
	{
	if(xx[1]>0 & xx[1]<1){cflag=1; conf1=xx[1];}
	if(cflag == 0 & xx[2]>0 & xx[2]<1){cflag=2; conf2=xx[2];}
	if(cflag == 1) {conf2=pchisq(qchisq(conf1,1),2); cn="c1"; cee=round(100*conf1,2);}
	if(cflag == 2) {conf1=pchisq(qchisq(conf2,2),1); cn="c2"; cee=round(100*conf2,2);}
	}
	if(cflag <= 0) return()

	pflag=0; q0=.001;
	xx="Enter p and q (at least one must be 0): ";
	xx=readline(xx); cat("\n");
	xx=as.numeric(unlist(strsplit(xx," ")));
	if(length(xx) != 2) return();
	if(pflag == 0)
	{
	if(xx[1]>0 & xx[1]<1) {pflag = 1; pp=xx[1];}
	if(pflag == 0 & xx[1] == 0 & xx[2] != 0) {qq=xx[2]; pflag = 2;}
	if(pflag == 2 & xx[2] == q0) xx[2]=0;
	}
	if(pflag == 0) jms=1 else jms=3;
} else
{
	# jms must be defined here, as it's used in an if test later
	cflag=0; jms=0;
	xx="Enter conf1: ";
	xx=readline(xx); cat("\n");
	xx=as.numeric(unlist(strsplit(xx," ")));
	if(length(xx) != 1 | xx[1] <= 0 | xx[1] >= 1) return();
	conf1=xx[1];
}
	g=lrmax(w); c1max=g$c1max; one23=g$one23;
	if(one23 > 1) {cat(paste("Need interval overlap for this particular plot.\n\n",sep="")); return(); } else
	if(conf1 >= c1max & gs == 1) {cat(paste("Need conf1 < ",round(c1max,4),", for this particular plot: Try again.\n\n",sep="")); return();}

	dat=w$d0; tit0=w$titl;

# do LR (needed if gs = 1 or 2) if conf1 < c1max and FM & SP (if gs = 2 and dif < 0)
	if(conf1 < c1max)
	{
	a=lr.lims(dat,conf1);
	cx=a$cx; cy=a$cy; lrmat=a$lrmat; dif=a$dif; con=a$con;
	muhat=a$muhat; sighat=a$sighat; ab=c(muhat,sighat);
	if(dif >= 0) {conf2=(conf2+3)/4; ab=c(muhat,Inf);}
	}

	if(gs == 1 & jms == 3)
	{
	if(pflag == 1) qq=muhat+qnorm(pp)*sighat else pp=pnorm((qq-muhat)/sighat);
	}

# FM & SP for eventual use and for the return

	if(dif<0)fmmat=ml.lims(dat,conf1);
	if(dif<0)spmat=sp.lims(dat,conf1);

#===========================Graph Sheet 1 (Graphs 1, 2 and 3)=============================
# To get: just graphsheet 1, set gs=1; just graphsheet 2, set gs=2.
options(warn=-1); # SUPPRESSES OUT OF BOUNDS WARNINGS (AND OTHERS)
isiz=0.9; rd=6;
#=========================================Graph 1========================================

rd=4;
ilt=3;

rx=range(cx); ry=range(cy); prx=pretty(rx); pry=pretty(ry);

if(gs == 1)
{
if(jms == 3) {par(mfrow = c(2, 2), oma = c(0,.4,1.5,0),  mar = c(3,3,3,1)); isiz=0.9;}
plot(cx,cy,type="l",xlim=rx,ylim=ry,xlab="",ylab="",xaxt="n",yaxt="n");
axis(1,at=prx,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pry,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext("m",side=1,line=1.3,cex=.9);
mtext("s",side=2,line=2.5,cex=.9,las=2);
abline(v=prx,lty=ilt); abline(h=pry,lty=ilt);
points(muhat,sighat,pch=16,cex=.7,col=2);
tit2a=paste(round(rx[1],rd)," < m < ",round(rx[2],rd),sep="");
tit2b=paste("   ",round(ry[1],rd)," < s < ",round(ry[2],rd),sep="");
mtext(tit2a,side=3,line=1.4,cex=.9);
mtext(tit2b,side=3,line=0.2,cex=.9);
m0=-1/qnorm(con);
if(con == .5) abline(v=muhat,col=1,lty=2) else abline(sighat-m0*muhat,m0,col=1,lty=2);

#=========================================================================================

if(jms == 3)
{
#=========================================Graph 2========================================
rx=range(cx+qnorm(pp)*cy); prx=pretty(rx);
plot(cx+qnorm(pp)*cy,cy,type="l",xlim=rx,ylim=ry,xlab="",ylab="",xaxt="n",yaxt="n");
axis(1,at=prx,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pry,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext("q",side=1,line=1.3,cex=.9);
points(qq,sighat,pch=16,cex=.7,col=2);
abline(v=pretty(rx),lty=ilt); abline(h=pretty(ry),lty=ilt);
tit1=paste("p = ",round(pp,rd),sep="");
tit2=paste(round(rx[1],rd)," < q < ",round(rx[2],rd),sep="");
mtext(tit1,side=3,line=1.4,cex=.9);
mtext(tit2,side=3,line=0.2,cex=.9);
if(pp == con) abline(v=muhat+qnorm(pp)*sighat,col=1,lty=2) else
	{
	m3=1/(qnorm(pp)+1/m0);
	abline(sighat-m3*(muhat+qnorm(pp)*sighat),m3,col=1,lty=2);
	}
#==========================Graph 3 (Null Plot Just for Text)============================

plot(c(0,1),c(0,1),type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
titone=paste("conf1 = ",round(conf1,5),sep="");
tittwo=paste("conf2 = ",round(conf2,5),sep="");
mtext(titone,side=3,line=-4.6,cex=.9);
mtext(tittwo,side=3,line=-6.6,cex=.9);


#=========================================Graph 4========================================

rx=range(pnorm((qq-cx)/cy)); prx=pretty(rx);
plot(pnorm((qq-cx)/cy),cy,xlab="",ylab="",type="l",xlim=rx,ylim=ry,xaxt="n",yaxt="n");
axis(1,at=prx,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pry,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext("p",side=1,line=1.3,cex=.9);
mtext("s",side=2,line=2.5,cex=.9,las=2);
points(pp,sighat,pch=16,cex=.7,col=2);
abline(v=pretty(rx),lty=ilt); abline(h=pretty(ry),lty=ilt);
abline(v=con,col=1,lty=2);
tit1=paste("q = ",round(qq,rd),sep="");
tit2=paste(round(rx[1],rd)," < p < ",round(rx[2],rd),sep="");
mtext(tit1,side=3,line=1.4,cex=.9);
ds=0;
mtext(tit2,side=3,line=0.2-ds,cex=.9);
}

par(mfrow=c(1,1));par(oma = c(0,0,1,0),  mar = c(5,4,4,2)+.1);
if(jms == 3) tit9=paste(tit0,", LR CB's",sep="") else
tit9=paste(tit0,", LR CB's (",cn," = ",cee,"%)",sep="")
mtext(tit9,side=3,line=-.5,cex=1.2,outer=T);
}

# Done with graph sheet 1 (gs = 1) containing either graph 1 or graphs 1, 2 3 and 4

#=======Define stuff needed for Graph Sheet 2 (next 4 plots) define lrmat,fmmat,spmat=======

if(gs == 2)
{
par(mfrow = c(2, 2), oma = c(0,0,1,0),  mar = c(3,4,3,1));
tq2="(qlo,qhi) about q (given p)";	tp2="(plo,phi) about p (given q)";
tq1="(qlo,qhi) about q (supressing p)";	tp1="(plo,phi) about p (supressing q)";
# To plot Linearized Response, and CL's versus q
# For Graph Sheet 2, need Range & delta on q-axis (qmin,qmax,bi) to cover FM, LR, and dose.p qlo's and qhi's

# Limits for the q axis for the two graphs using the function graf1
big=0;
isiz=.8;
legq=c("Likelihood Ratio","Fisher Matrix","SPlus (dose.p)");
legp=c("Likelihood Ratio","Fisher Matrix","SPlus (GLM)");
if(big==0){par(mfrow=c(2,2));isiz=.5; legq=legp=c("LR","FM","GLM")};
if(big!=0)	par(mfrow=c(1,1));

# pr (graf1) is of length 15. For purposes of graphs 4 & 6,
# Skip 1st two & last 2 two - corresponding to 1/million, 1/100000, 99999/10000, 999999/1000000

ih=3:13;

if(dif < 0)
{
if(conf1 < c1max) {mat[1:11,]=lrmat[ih,]; mat[12:22,]=fmmat[ih,]; mat[23:33,]=spmat[ih,]; i3=3;} else
{mat[12:22,]=fmmat[ih,]; mat[23:33,]=spmat[ih,]; i3=2;}
} else
{
if(conf1 < c1max) {mat[1:11,]=lrmat[ih,]; i3=1;}
}
jj4=range(c(mat[,1],mat[,3]),na.rm=T);

#======================Graph 5 (Linearized Response with (qlo,qhi))=======================
# FM Color = 4 (Blue); LR Color = 1 (Black); SP Color = 2 (RED)
fmc=4; lrc=1; spc=2;

	graf1(jj4,tq2,1,big,1);
	lw=1.9;
# LCL Curves and UCL Curves about q(p)
if(i3 == 1 | i3 == 3)
{
w=lrmat;cl=lrc;
lines(w[,1],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
lines(w[,3],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,5]),type="l",col=8,cex=2,lwd=lw);
}
if(i3 == 2 | i3 == 3)
{
w=fmmat;cl=fmc;
lines(w[,1],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
lines(w[,3],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
w=spmat;cl=spc;
lines(w[,1],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
lines(w[,3],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,5]),type="l",col=8,cex=2,lwd=lw);
}

#==========================Graph 6 (qlo & qhi versus q  Plots)============================

ih=1:15
if(i3 == 2 | i3 == 3) jj5=range(c(lrmat[ih,2],fmmat[ih,2],spmat[ih,2]),na.rm=T) else jj5=muhat+c(-1,1);
plot(lrmat[,2],lrmat[,3],xlim=range(jj5),ylim=range(pretty(jj4)),type="n",xlab="",ylab="",xaxt="n",yaxt="n");
mtext("Predicted q",side=1,line=1.6,cex=.8);mtext("qlo & qhi",side=2,line=2,cex=.8);
qtic=pretty(jj5);
axis(1,at=qtic,labels=T,tck=.01,cex=.8,mgp=c(3,.5,0));
qtic=pretty(jj4);
axis(2,at=qtic,labels=T,tck=.01,cex=.8,mgp=c(3,.5,0));

box();
abline(v=pretty(jj5),lty=ilt);
abline(h=pretty(jj4),lty=ilt);

if(i3 == 2 | i3 == 3)
{
lines(fmmat[,2],fmmat[,3],col=fmc,lwd=lw);lines(lrmat[,2],lrmat[,3],col=lrc,lwd=lw);lines(spmat[,2],spmat[,3],col=spc,lwd=lw);
lines(fmmat[,2],fmmat[,1],col=fmc,lwd=lw);lines(lrmat[,2],lrmat[,1],col=lrc,lwd=lw);lines(spmat[,2],spmat[,1],col=spc,lwd=lw);
lines(lrmat[,2],lrmat[,2],col=8,lwd=lw);
}

if(i3 == 1 | i3 == 3)
{
lines(lrmat[,2],lrmat[,3],col=lrc,lwd=lw);
lines(lrmat[,2],lrmat[,1],col=lrc,lwd=lw);
lines(lrmat[,2],lrmat[,2],col=8,lwd=lw);
}
mtext(tq1,side=3,line=1,cex=.9);

#=====================Graph 7 (Linearized Response with (plo,phi))========================

	graf1(jj5,tp2,2,big,0);

# LCL Curves and UCL Curves about p(q)
	ep0=0.000001; ep1=1-ep0;

if(i3 == 1 | i3 == 3)
{
w=lrmat;cl=lrc;
lines(w[,2],qnorm(w[,4]+ep0),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,6]-ep0),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,5]),type="l",col=8,cex=2,lwd=lw);
}
if(i3 == 2 | i3 == 3)
{
w=fmmat;cl=fmc;
lines(w[,2],qnorm(w[,4]+ep0),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,6]-ep0),type="l",col=cl,cex=2,lwd=lw);

w=spmat;cl=spc;
lines(w[,2],qnorm(w[,4]+ep0),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,6]-ep0),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,5]),type="l",col=8,cex=2,lwd=lw);
}

#==========================Graph 8 (plo & phi versus p  Plots)============================

h=100;
plot(h*lrmat[,5],h*lrmat[,6],xlim=h*c(0,1),ylim=h*c(0,1),type="n",xlab="",ylab="",xaxt="n",yaxt="n");
mtext("Predicted p (in %)",side=1,line=1.6,cex=.8);mtext("plo & phi (in %)",side=2,line=2,cex=.8);
qtic=pretty(c(0,100));
axis(1,at=qtic,labels=T,tck=.01,cex=.8,mgp=c(3,.5,0));
axis(2,at=qtic,labels=T,tck=.01,cex=.8,mgp=c(3,.5,0));

abline(v=pretty(h*c(0,1)),lty=ilt);abline(h=pretty(c(0,100)),lty=ilt);

if(i3 == 2 | i3 == 3)
{
lines(h*fmmat[,5],h*fmmat[,6],col=fmc,lwd=lw);lines(h*spmat[,5],h*spmat[,6],col=spc,lwd=lw);
lines(h*fmmat[,5],h*fmmat[,4],col=fmc,lwd=lw);lines(h*spmat[,5],h*spmat[,4],col=spc,lwd=lw);
}
if(i3 == 1 | i3 == 3)
{
lines(h*lrmat[,5],h*lrmat[,6],col=lrc,lwd=lw);
lines(h*lrmat[,5],h*lrmat[,4],col=lrc,lwd=lw);
}
box();
lines(c(0,100),c(0,100),lwd=lw,col=8);
mtext(tp1,side=3,line=1,cex=.9);
par(mfrow=c(1,1));
par(oma = c(0,0,1,0),  mar = c(5,4,4,2)+.1);
c1=round(100*(1+conf1)/2,2);
mtext(paste(tit0,", 1-Sided ",c1,"% CL's",sep=""),side=3,line=-.5,cex=1.2,outer=T);
#==========================================================================================
}

matt=rbind(fmmat,lrmat,spmat);
cn=c("ql","q","qh","pl","p","pu");
options(scipen=999);
rmatt=round(matt,6);
write.table(rmatt,file="cbs.txt",quote=F,sep=",",na="i",col.names=cn,row.names=F)
reset();
return(rmatt)
}
