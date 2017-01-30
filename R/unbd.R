#' Title
#'
#' @param rx 
#' @param ry 
#' @param levs 
#' @param mh1 
#' @param mh2 
#' @param es 
#' @param mlim 
#'
#' @return
#' @export
#'
#' @examples
unbd <-
function(rx,ry,levs,mh1,mh2,es,mlim)
{
		mh=(mh1+mh2)/2;
		if(length(mlim) == 2){L=mlim[1]; U=mlim[2];} else
		{
		pm=c(-1,1); I=c(mh1,mh2)+0.5*mh*pm; L=I[1]; U=I[2];
		}  
		if(es > 0) S0=ST1=ulik(rx,ry,levs,mh1,es) else S0=ST1=0;
		num=51;
		J1=seq(mh1,L,length=num); 
		for(i in 2:num)
		{
		S1=uliknext(rx,ry,levs,J1[i-1],S0,J1[i]);
		ST1=c(S1,ST1);
		S0=S1;
		}		
		if(es > 0) S0=ST2=ulik(rx,ry,levs,mh2,es) else S0=ST2=0;
		J2=seq(mh2,U,length=num); 
		for(i in 2:num)
		{
		S1=uliknext(rx,ry,levs,J2[i-1],S0,J2[i]);
		ST2=c(ST2,S1);
		S0=S1;
		}
	mt1=J1[num:1]; mt2=J2; 
	return(list(mt1,ST1,mt2,ST2));
}
