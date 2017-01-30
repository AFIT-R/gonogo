#' Title
#'
#' @param rx 
#' @param ry 
#' @param levs0 
#' @param em1 
#' @param es 
#' @param em2 
#'
#' @return
#' @export
#'
#' @examples
uliknext <-
function(rx,ry,levs0,em1,es,em2)
{
if(es == 0) es=.1;
shi=es; slo=es/2; 
val1=rem1=xyllik(rx,ry,em2,slo);
val2=rem2=xyllik(rx,ry,em2,shi);

# val may at first decrease (good) but then increase (bad)
while(val1 > levs0) 
{
shi=slo; slo=slo/2; val1=xyllik(rx,ry,em2,slo);
if(val1 > rem1) 
	{
	cat(paste("Message from uliknext: conf1 is LARGER & TOO NEAR c1max.\n",sep=""))
	cat(paste("The specific problem is: for m = ",round(em2,4),", val1(s) > levs0 for all s.\n",sep=""));
	cat(paste("Increasing conf1 can produce a more clearly defined UNBOUNDED region.\n",sep=""));
	stopQuietly();	
	} else rem1=val1;
}

# val2 may at first increase (good) but then decrease (bad)
while(val2 < levs0) 
{
slo=shi; shi=2*shi; val2=xyllik(rx,ry,em2,shi);
if(val2 < rem2) 
	{
	cat(paste("Message from uliknext: conf1 is LARGER & TOO NEAR c1max.\n",sep=""))
	cat(paste("The specific problem is: for m = ",round(em2,4),", val2(s) < levs0 for all s.\n",sep=""));
	cat(paste("Increasing conf1 can produce a more clearly defined UNBOUNDED region.\n",sep=""));
	stopQuietly();
	} else rem2=val2;
}

eps=.0001; delt=1;
while(delt > eps)
{
s=(slo+shi)/2;
val=xyllik(rx,ry,em2,s);
if(val > levs0) shi=s else slo=s;
delt=abs(val-levs0);
}
s=(slo+shi)/2;
return(s);
}
