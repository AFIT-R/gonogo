#' Title
#'
#' @param data.df 
#' @param ln 
#' @param plotit 
#' @param response 
#' @param lineit 
#' @param labx 
#' @param laby 
#' @param titl 
#'
#' @return
#' @export
#'
#' @examples
pavdf <-
function(data.df, ln, plotit = F, response = 0, lineit = F, labx = "STIMULUS", laby = 
	"PROBABILITY OF RESPONSE", titl = "PAV SOLUTION")
{
	# 
	#	FUNCTION TO COMPUTE AND PLOT POOLED ADJACENT VIOLATORS (PAV) ALGORITHM
	#	responses are either 0 or 1. response = 0 (default, as in ml2002) 
	#	means that, as x = stress increases, Pr[response = 0] increases 
	#	data.df is a 3 Column dataframe who's Column Names are:
	#	X=Stresses, Y=Responses, COUNT=Number of Y's per X, respectively.  
	#	RETURNS list with components:	$full, matrix with a number of rows = length(events)
	#				 		$unique, matrix with number of rows = length(unique(x))
	#				 		$coords, matrix with each row a point for plotting pav 
	#							   solution vs x for each unique prob estimate
	#
	events = data.df$Y
	if(response==0) events = 1 - data.df$Y
	trials = data.df$COUNT
	x = data.df$X
	x = rep(x, trials)
	events = rep(events, trials)
	trials = rep(1, length(x))
	k = length(events)
	if(length(x) == 0.)
		x = 1.:k
	else {
		events = events[order(x)]
		trials = trials[order(x)]
		x = sort(x)
		xuniq = unique(x)
		k = length(xuniq)
		evtmp = rep(0., k)
		tritmp = rep(0., k)
		for(i in 1.:k) {
			evtmp[i] = sum(events[x == xuniq[i]])
			tritmp[i] = sum(trials[x == xuniq[i]])
		}
	}
	events = evtmp
	trials = tritmp
	#print(events)
	#print(trials)
	p = matrix(0., k, 1.)
	pp = matrix(0., k, k)
	for(i in 1.:k) {
		sum1 = 0.
		sum2 = 0.
		for(j in i:k) {
			sum1 = sum1 + events[j]
			sum2 = sum2 + trials[j]
			pp[i, j] = sum1/sum2
		}
		temp = as.matrix(pp[(1.:i), (i:k)])
		p[i] = ifelse(i > 1., max(apply(temp, 1., min)), min(temp))
	}
	puniq = unique(p)
	kk = length(puniq)
	xp = rep(0., kk)
	for(i in 1.:kk) {
		xp[i] = min(xuniq[p == puniq[i]])
	}

	if(ln) {xp=exp(xp); xplt = c(xp[1.], rep(xp[-1.], rep(2., length(xp) - 1.)), max(exp(x)));} else
	xplt = c(xp[1.], rep(xp[-1.], rep(2., length(xp) - 1.)), max(x))
	pplt = c(rep(puniq, rep(2., length(puniq))))
	if(plotit) {
		if(!lineit)
			plot(xplt, pplt, type = "n", xlab = labx, ylab = laby, main
				 = titl)
		lines(xplt, pplt, lwd = 2.)
	}
	xx = list(full = cbind(xuniq, events/trials, p), unique = cbind(xp, puniq),
		coords = cbind(xplt, pplt))
	return(xx)
}
