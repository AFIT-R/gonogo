#' Title
#'
#' returns several important characteristics of the test data
#' @param w sensitivity test "w"
#' @param plt
#'
#' @return
#' the overlap category "one23" (1 for interval overlap; 2 for point overlap; and 3 for no overlap);
#' c1max and c2max, which indicate whether the LR joint confidence regions are bounded (conf1 < c1max, or conf2 < c2max);
#' and con = # of responses/total tested (which determines the axis of the joint LR confidence region).
#' conf1 and conf2 are related by: c1max=pchisq(qchisq(c2max,2),1).
#' @export
#'
#' @examples
lrmax <-
function(w,plt=F)
{
dat=w$d0; title=w$title;

rx=dat$X; ry=dat$Y; nc=dat$COUNT;
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
}
M0=max(r0); m1=min(r1); del=m1-M0; one23=2+sign(del);

switch(one23,
{	# OVERLAP (Interval) (use log lik)
	xglm=glm(ry~rx,family=binomial(link=probit),maxit=10.0,epsilon=1e-006);
	ab=as.vector(xglm$coef);
	muhat=-ab[1]/ab[2]; sighat=1/ab[2];
	uu=xyllik(rx,ry,muhat,sighat);
},
{	# OVERLAP (Point) (use lik)
	muhat=(m1+M0)/2; sighat=0;
	mx=ry[rx == m1]; s1=sum(mx); s2=length(mx)-s1;
	uu=s1*log(s1) + s2*log(s2) - (s1+s2)*log(s1+s2);
},
{	# NO OVERLAP (use lik)
	muhat=(m1+M0)/2; sighat=0; uu=0;
}
);

c2max=pchisq(2*(uu-llc),2);
c1max=pchisq(qchisq(c2max,2),1);
a=c(con,llc,c1max,c2max);

con=round(con,5); llc=round(llc,5);
c1max=round(c1max,5); c2max=round(c2max,5);
wx=list(dat,title,one23,con,llc,c1max,c2max);
names(wx)=c("d0","title","one23","con","llc","c1max","c2max")
if(plt) picdat(wx);
return(wx)
}
