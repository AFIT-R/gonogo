#' MLE's of mu and sigma plot, simulation version
#'
#' @param dat sensitivity tests "w"
#'
#' @return
#' @export
#'
#' @examples
pSdat2 <-
function(dat)
{
dt=dtt=dat$d0; about=dat$about; titl=dat$titl; pee=dat$p;
ln=dat$ln; iseed=dat$iseed;
tmu=dat$tmu; tsig=dat$tsig;
M=dat$M; dm=dat$dm; ds=dat$ds;
rmzm=round(tmu,4); rmzs=round(tsig,4);
#titl1=substitute(paste(titl,": (",mu[t],", ",sigma[t],") = (",rmzm,", ",rmzs,"), ",delta[t]," = (",dm,", ",ds,")",sep=""));
if(iseed < 0) titl1=substitute(paste(titl,": (",mu[t],", ",sigma[t],") = (",rmzm,", ",rmzs,"), ",delta[t]," = (",dm,", ",ds,")",sep="")) else
titl1=substitute(paste(titl,": (",mu[t],", ",sigma[t],") = (",rmzm,", ",rmzs,"), ",delta[t]," = (",dm,", ",ds,"), ",i[seed]," = ",iseed,sep=""))

if(length(pee) == 0) pee=0;
id=dt$ID; nid=length(id);
fini=0; if(id[nid]=="III3") fini=1;
if(fini == 1) {dtt=dtt[-nid,]; id=id[-nid]; nid=nid-1;}

if(M == 1) about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[11],",",n[12],",",n[2],",",n[3],"|p,",lambda,",res}",sep="")) else
about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[11],",",n[12],",",n[2],",",n[3],"|p,",lambda,",res,M}",sep=""));

kp=0;
        for(j in 1:nid)
        {
        jj=m.update(dtt[1:j,]);
        M0=jj$M0; m1=jj$m1;
        uv=c(M0,m1);
        if(!any(is.na(uv))) {if(M0 > m1) kp=j;}
        if(kp > 0) break;
        }

	mus=sigs=zee=rep(0,nid-kp+1);

if(kp == 0) cat("pStest(z,plt=2) option requires having completed Phase I2 (i.e., achieving overlap)\n");
if(kp > 0)
{
for(j in kp:nid) {g=glmmle(dtt[1:j,]); mus[j-kp+1]=g$mu; sigs[j-kp+1]=g$sig;}
if(pee > 0 & pee < 1)zee=mus+qnorm(pee)*sigs;
par(mfrow=c(2,1), mar=c(1.5,2.5,.5,.5), oma=c(2,2,3.5,2));
lmu=pretty(mus); lsig=pretty(sigs); lx=pretty(c(kp,nid)); rx=kp:nid; rxx=range(rx);
if(diff(rxx)==0)rxx=rxx+c(-1,1)
plot(kp:nid,mus,type="l",xlab="",ylab="",xlim=rxx,xaxt="n",ylim=range(lmu),yaxt="n");
axis(1,at=kp:nid,labels=T,tck=-.03,mgp=c(1,.4,0),cex.axis=.8);
axis(2,at=lmu,labels=T,tck=-.02,mgp=c(1,.4,0),las=2,cex.axis=.8);
if(ln) mtext("Mean(Log)",side=2,line=3,cex=1) else mtext("Mean",side=2,line=3,cex=1);

lt=3; abline(h=lmu,lty=lt); abline(v=lx,lty=lt);
points(kp:nid,mus,pch=16,cex=.8);
if(kp == nid) nlx=2 else nlx=nid-kp;
plot(kp:nid,sigs,type="l",xlab="",ylab="",ylim=range(lsig),yaxt="n",xlim=rxx,xaxt="n");
axis(1,at=kp:nid,labels=T,tck=-.03,mgp=c(1,.4,0),cex.axis=.8);
axis(2,at=lsig,labels=T,tck=-.02,mgp=c(1,.4,0),las=2,cex.axis=.8);
mtext("Cumulative Test Size (n)",side=1,line=0,cex=1,outer=T);
if(ln) mtext("SD(Log)",side=2,line=3,cex=1) else mtext("SD",side=2,line=3,cex=1);

abline(h=lsig,lty=lt); abline(v=lx,lty=lt);
points(kp:nid,sigs,pch=16,cex=.8);
par(mfrow=c(1,1));
els=c(2.5,1);

mtext(titl1,line=2.7,cex=1.1);
mtext(about1,side=3,line=1.4,cex=1.1);
mtext(about,side=3,line=.3,cex=1.1);
}
reset();
return(matrix(c(mus,sigs,zee),ncol=3));
}
