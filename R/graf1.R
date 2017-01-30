#' Title
#'
#' @param limx 
#' @param t1 
#' @param k 
#' @param big 
#' @param legnd 
#'
#' @return
#' @export
#'
#' @examples
graf1 <-
function(limx,t1,k,big,legnd)
	{
	lw=3;
	spl="SPlus (dose.p)"; if(k!=1)spl="SPlus (GLM)";	if(big==0)sp1="SP";
	leg=c("FM","LR","GLM"); if(big!=0)leg=c("Fisher Matrix","Likelihood Ratio",spl);
	qtic=pretty(limx);
	pr=c(1,10,100,1000,5000,9000,9900,9990,9999)/10000; q=qnorm(pr);
	xl=range(limx); yl=c(-3.8,3.8); 
	plot(xl,yl,type="n",xlim=xl,ylim=yl,xaxt="n",yaxt="n",xlab="",ylab="",cex=.8);
	xl1="quantile (q)"; yl1="Probability of Response (p%)";
	mtext(xl1,side=1,line=1.6,cex=.8); mtext(yl1,side=2,line=2.3,cex=.8);
	mtext(t1,side=3,line=1,cex=.9);
	isiz1=.7;if(big==0)isiz1=.4;
	axis(1,at=qtic,labels=T,tck=.01,cex=isiz1,mgp=c(3,.5,0));
	axis(2,at=q,labels=paste(100*pr," ",sep=""),tck=.01,cex.axis=.8,mgp=c(3,0,0),las=2);
	axis(3,at=qtic,labels=F,tck=.01,cex=isiz1);
	# Horizontal Grid Lines
	delx=.75
	ilt=3
	for(i in 1:length(pr))	lines(xl+c(-delx,delx),c(q[i],q[i]),lty=ilt);
	# Verticle Grid Lines
	abline(v=qtic,lty=ilt);
	del1=diff(range(limx))/20;
	if(legnd>0)legend(xl[1]+del1,qnorm(.997),legend=leg,lty=c(1,1,1),col=c(4,1,2),lwd=lw,cex=.6,bg="white");
	return()
	}
