#' Title
#'
#' retrieve some basic information about a test w and the ML fit that was obtained
#'
#' @param dat sensitivity test "w"
#' @param lgit
#' @param ln
#' @param xmin
#' @param xmax
#' @param conf
#' @param small
#' @param response
#' @param labx
#' @param laby
#' @param maxitt
#' @param eps
#' @param zee
#'
#' @return
#' @export
#'
#' @examples
nyqrda <-
function(dat, lgit = F, ln = F, xmin = -9999., xmax = 9999, conf = 0.95, small
	 = F, response = 0., labx = "", laby = "PROBABILITY OF RESPONSE", maxitt
	 = 10., eps = 1e-006, zee = 0)
{
	# nyqrda is a pared down version of yqrda that suits our purposes
	ldot=3.;
	x = xsav=rep(dat$X, dat$COUNT);
	y = ysav=rep(dat$Y, dat$COUNT);
	if(!lgit)	xglm = glm(y ~ x, family = binomial(link = probit), maxit = maxitt, epsilon = eps)
	if(lgit)  	xglm = glm(y ~ x, family = binomial(link = logit),  maxit = maxitt, epsilon = eps)
	ab = as.vector(xglm$coef);
	a=ab[1]
	b=ab[2]
	mu= -a/b
	sig=1/b
	if(ln) k=2.5 else k=3.5;
	pm=c(-1,1); pee=pnorm(pm*k);
	if(!ln) a1=pretty(mu+k*sig*pm) else a1=pretty(qlnorm(pee,meanlog=mu,sdlog=sig))
	if(!ln) a2 = range(c(a1,range(x))) else a2=range(c(a1,range(dat$RX)));
	if(ln) a2[2]=min(a2[2],1.5*max(exp(x)));
	xs = seq(a2[1], a2[2], length = 100.);
	if(ln) {if(xs[1] == 0) xs[1]=xs[2]/100; xs=log(xs);}
	yy = predict(xglm, list(x = xs), se.fit = T);
	w = qt(conf, xglm$df.resid);
	if(zee == 1) 	w = qnorm(conf);
	yu = yy$fit + w * yy$se.fit;
	yl = yy$fit - w * yy$se.fit;
	if(!lgit)
		{
		ynu = pnorm(yu);
		ynl = pnorm(yl);
		yn = pnorm(yy$fit);
		}
	if(lgit)
		{
		ynu = plogis(yu);
		ynl = plogis(yl);
		yn = plogis(yy$fit);
		}
	if(ln) xs = exp(xs)
	plot(xs, yn, ylim = c(0,1), type = "n", las = 1., cex=.6, xlab = labx, ylab = laby)
	abline(h = 0.1 * c(0.:10.), lty = ldot)
	if(!ln) {abline(v = pretty(a2), lty = ldot); dpts=dnorm(xs,mean=mu,sd=sig);} else
	{abline(v=pretty(range(xs)),lty=ldot);
	dpts=dlnorm(xs,meanlog=mu,sdlog=sig);}
	em=max(dpts);
	lines(xs,dpts/em,type="l",col=8,lwd=2);
	lines(xs, yn, lwd=2);
	lines(xs, ynl, lty = 4)
	lines(xs, ynu, lty = 4)
	pavdf(dat, ln, response = 1, plotit = T, lineit = T)
	nxv=length(xsav);
	if(!ln) {for(i in 1:nxv) points(xsav[i],ysav[i]+(ysav[i]-.5)/25,pch=4,lwd=1.5,cex=.5);} else
	{for(i in 1:nxv) points(exp(xsav[i]),ysav[i]+(ysav[i]-.5)/25,pch=4,lwd=1.5,cex=.5);}
	xx=list(xglm, a, b, mu, sig);
	names(xx)=c("xglm","a","b","mu","sig");
	reset()
	return(xx)
}
