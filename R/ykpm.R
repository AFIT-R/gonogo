#' Title
#'
#' @param dat 
#' @param m 
#' @param es 
#' @param iplot 
#'
#' @return
#' @export
#'
#' @examples
ykpm <-
function(dat, m, es, iplot = 0)
{
	m1 = min(dat$X[which(dat$Y == 1)],na.rm=T)
	M0 = max(dat$X[which(dat$Y == 0)],na.rm=T)
	mx1 = mean(dat$X[dat$Y == 1])
	mx0 = mean(dat$X[dat$Y == 0])
	if(m1 < M0 & mx1 <= mx0) 
		{
		# This how Neyer's SenTest handles Flat, sg = Inf cases
		kopt=1.31525757633905*sign(.5-dat$Y[nrow(dat)])
		xstar=list(m+kopt*es)
		names(xstar)="xstar"
		return(xstar)
		}
	bb = yinfomat(dat, m, es)
	det0 = bb$deti
	b = bb$infm
	vcov1 = bb$vcov1
	# Make sure it knows it's symmetrical
	b[2, 1] = b[1, 2] = (b[1, 2] + b[2, 1])/2
	k = (-4000:4000)/1000
	dk = ydeldet(k, b)
	delstar = max(dk,na.rm=T)
	ik = which(dk == delstar)
	ok = "Y"
	if(ik == 1 | ik == length(k))
		ok = "N"
	kstar = k[ik]
	xstar = m + kstar * es
	deti = det0 + delstar
	if(iplot != 0) {
		plot(k, dk, type = "l")
		mtext(paste("max(dk)=", round(delstar, 5), " at k=",
			k[ik], ", deti=", round(det0 + delstar, 4),
			sep = ""), side = 3, line = 1.2)
		points(k[ik], delstar, pch = 16, col = 3, cex = 0.8)
	}
	xx=list(det0, kstar, xstar, delstar, deti, ok, vcov1);
	names(xx)=c("det0","kstar","xstar","delstar","deti","ok","vcov1");
	return(xx);
}
