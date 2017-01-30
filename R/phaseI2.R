#' Title
#'
#' @param d0 
#' @param dat0 
#' @param sg 
#' @param reso 
#' @param about 
#' @param titl 
#' @param unit 
#' @param ln 
#'
#' @return
#' @export
#'
#' @examples
phaseI2 <-
function(d0,dat0,sg,reso,about,titl,unit,ln)
{
nret=c("d0","dat0","endi","sg");
endi=0;
idii="";
while(1)
	{
	# del = m1-M0; del < 0 ==> OVERLAP
	j=m.update(d0); m1=j$m1; M0=j$M0; del=m1-M0;
	while(del >= 1.5*sg & endi == 0)
		{
		if(endi == 0)
			{
			xx=(M0+m1)/2; 
			id=paste(idii,"I2(ib)",sep="");
			u=getd0(xx,d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;		
			j=m.update(d0); m1=j$m1; M0=j$M0; del=m1-M0;
			}
		}
	if(del < 0 | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret);}		
	j=n.update(d0); n0=j$n0; n1=j$n1;
	if(del >= 0 & endi == 0) 
		{
		if(n0 > n1 & endi == 0)
			{
			xx=m1+0.3*sg;
			id=paste(idii,"I2(ic)",sep="");
			u=getd0(xx,d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;		
			if(d0$Y[nrow(d0)] == 0 | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret);}
			if(d0$Y[nrow(d0)] == 1 & endi == 0)
				{
				xx=M0-.3*sg;	
				id=paste(idii,"I2(ic)",sep="");
				u=getd0(xx,d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;		
				if(d0$Y[nrow(d0)] == 1 | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret);}
				if(d0$Y[nrow(d0)] == 0 & endi == 0)
					{
					sg=2*sg/3;	
					idii=paste(idii,"r",sep="");	
					}
				}
			}
		if(n0 <= n1 & endi == 0)
			{
			xx=M0-.3*sg;
			id=paste(idii,"I2(id)",sep="");
			u=getd0(xx,d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;		
			if(d0$Y[nrow(d0)] == 1 | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret);}
			if(d0$Y[nrow(d0)] == 0 & endi == 0)
				{
				xx=m1+.3*sg;	
				id=paste(idii,"I2(id)",sep="");
				u=getd0(xx,d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;		
				if(d0$Y[nrow(d0)] == 0 | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret);}
				if(d0$Y[nrow(d0)] == 1 & endi == 0)
					{
					sg=2*sg/3;	
					idii=paste(idii,"r",sep="");	
					}
				}
			}
		}
}
ret=list(d0,dat0,endi,sg); names(ret)=nret; return(ret);
}
