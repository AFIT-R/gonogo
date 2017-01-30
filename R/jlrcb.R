#' Title
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
jlrcb <-
function(dat)
{
# c2max = function of uu(muhat,sighat,rx,ry) and llc(con(rx,ry))
# AN IDENTITY: levs = uu+log(1-conf2) = 1-qchisq(conf1,1)/(2*uu);

dt=dat$d0; titl1=dat$title;

xx="Enter conf1's (separated by blanks): ";
xx=readline(xx); cat("\n");
xx=as.numeric(unlist(strsplit(xx," ")));
vconf1=sort(unique(xx));
vconf2=pchisq(qchisq(vconf1,1),2);
nc1=length(vconf1);
rx=dt$X; ry=dt$Y; nc=dt$COUNT;
rx=rep(rx,nc); ry=rep(ry,nc); 
nt=sum(nc);
con=sum(ry)/length(ry);
llc=sum(log(con^ry*(1-con)^(1-ry)));
 
r0=rx[ry==0]; r1=rx[ry==1]; 
mix=length(r0)*length(r1);
lux=length(unique(rx));
if(mix == 0 | lux == 1)	
{
cat(paste("Need to do more testing\n",sep="")); 
stopQuietly();
}
M0=max(r0); m1=min(r1); del=m1-M0; one23=2+sign(del);

bl=ul=list(1)		# placeholder for bounded & unbounded lists
numb=numu=0;		# eventual number of bounded & unbounded plots
mlim=0;			# default value to pass into unbd (if all are unbounded)
icbl=T;
for(i in 1:nc1)
{
conf2=pchisq(qchisq(vconf1[i],1),2);

switch(one23,
{	# OVERLAP (Interval) (use log lik)

	xglm=glm(ry~rx,family=binomial(link=probit),maxit=10.0,epsilon=1e-006);	
	ab=as.vector(xglm$coef);
	muhat=-ab[1]/ab[2]; sighat=1/ab[2];
	uu=xyllik(rx,ry,muhat,sighat);
	levs=uu+log(1-conf2); 
	c2max=pchisq(2*(uu-llc),2);
	bnd=T; if(conf2 > c2max) bnd=F;
	if(bnd) 
		{
		ms=op=c(muhat,sighat); numb=numb+1;
		bl[[numb]]=jlik(rx,ry,levs,ms,op,one23);
		} else {
			if(icbl & numb > 0) {cbl=calcblim(bl,ul); mlim=cbl$mlim; icbl=F;}
			numu=numu+1; 
			ul[[numu]]=unbd(rx,ry,levs,muhat,muhat,sighat,mlim);}
},
{	# OVERLAP (Point) (use lik)

	muhat=(m1+M0)/2; sighat=0; 
	mx=ry[rx == m1]; s1=sum(mx); s2=length(mx)-s1;
	uu=s1*log(s1) + s2*log(s2) - (s1+s2)*log(s1+s2);
	levs=uu+log(1-conf2);
	c2max=pchisq(2*(uu-llc),2);
	bnd=T; if(conf2 > c2max) bnd=F;
	if(bnd){
		op=otherpoint(rx,ry,muhat,levs,con);
		ms=c(muhat,sighat); numb=numb+1;
		bl[[numb]]=jlik(rx,ry,levs,ms,op,one23);
		} else {
			if(icbl & numb > 0) {cbl=calcblim(bl,ul); mlim=cbl$mlim; icbl=F;}
			numu=numu+1; 
			ul[[numu]]=unbd(rx,ry,levs,muhat,muhat,sighat,mlim);}
},
{	# NO OVERLAP (use lik)

	muhat=(m1+M0)/2; sighat=0; 
	uu=0;

ig=2; 
if(ig == 1) {conf2=(3+conf2)/4; levs=log(1-conf2);} else
		{c3=(3+conf2)/4; levs=log(1-c3);}

	c2max=pchisq(2*(uu-llc),2);
	c2max=1-4*exp(llc);
	bnd=T; if(conf2 > c2max) bnd=F;
	if(bnd){
		op=c(m1,M0);
		ms=c(muhat,sighat); numb=numb+1;
		bl[[numb]]=jlik(rx,ry,levs,ms,op,one23);
		} else {
			if(icbl & numb > 0) {cbl=calcblim(bl,ul); mlim=cbl$mlim; icbl=F;}
			numu=numu+1; 
			ul[[numu]]=unbd(rx,ry,levs,M0,m1,sighat,mlim);}
}
);

}

cbl=calcblim(bl,ul);
g=list(bl,ul,cbl); 
b=g[[1]]; u=g[[2]]; mlim=g[[3]]$mlim; slim=g[[3]]$slim;
plot(1,1,type="n",xlim=mlim,ylim=slim,xlab="m",ylab="s");
abc=", Joint LR CB's";
titl1=paste(titl1," (c2max =",round(c2max,5),")",abc,sep="");
mtext(titl1,side=3,line=2.9,cex=1);
pxl=pretty(mlim); pyl=pretty(slim); ilt=3;
abline(v=pxl,lty=ilt); abline(h=pyl,lty=ilt);
m0=-1/qnorm(con);
if(con == .5) abline(v=muhat,col=1,lty=2) else 
abline(sighat-m0*muhat,m0,col=1,lty=2);
points(muhat,sighat,pch=16,cex=.7,col=2);
if(numb > 0) {for(k in 1:numb){bk=b[[k]]; 
	lines(bk[[1]],bk[[2]],type="l");}}
if(numu > 0) {for(k in 1:numu){ uk=u[[k]]; 
	lines(uk[[1]],uk[[2]],type="l",col=2);
	lines(uk[[3]],uk[[4]],type="l",col=2);}}	
print(vconf1)
nam1="c1"; b0=mkb0(vconf1,nam1);
nam2="c2"; b1=mkb0(vconf2,nam2);
mtext(b0,side=3,line=1.6,cex=1);
mtext(b1,side=3,line=.3,cex=1);

return(g);
}
