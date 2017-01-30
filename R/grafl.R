#' Title
#'
#' @param limx 
#'
#' @return
#' @export
#'
#' @examples
grafl <-
function(limx)
	{
	lw=3;
	titl="Liklihood Ratio CL's"
	qtic=pretty(limx); qtr=range(qtic);
	pr=c(1,10,100,1000,5000,9000,9900,9990,9999)/10000; q=qnorm(pr);
	xl=range(limx); yl=c(-3.8,3.8); 
	plot(xl,yl,type="n",xlim=xl,ylim=yl,xaxt="n",yaxt="n",xlab="",ylab="",cex=.8);
	xl1="q";yl1="Probability of Response (p%)";
	mtext(xl1,side=1,line=1.3,cex=.9); mtext(yl1,side=2,line=2.3,cex=.8);
	mtext(titl,side=3,line=.6,cex=.8);
	isiz1=.7;
	axis(1,at=qtic,labels=T,tck=.01,cex=isiz1,mgp=c(3,.2,0));
	axis(2,at=q,labels=paste(100*pr," ",sep=""),tck=.01,cex.axis=.8,mgp=c(3,0,0),las=2);
	axis(3,at=qtic,labels=F,tck=.01,cex=isiz1);
	# Horizontal Grid Lines
	delx=.75; delx=0;
	ilt=3
	for(i in 1:length(pr))	lines(qtr,c(q[i],q[i]),lty=ilt);
	# Verticle Grid Lines
	abline(v=qtic,lty=ilt);
	del1=diff(range(limx))/20;
	return()
	}
