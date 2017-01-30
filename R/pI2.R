#' Title
#'
#' @param d0 
#' @param dat0 
#' @param sg 
#' @param tmu 
#' @param tsig 
#' @param reso 
#' @param ln 
#' @param iseed 
#'
#' @return
#' @export
#'
#' @examples
pI2 <-
function(d0,dat0,sg,tmu,tsig,reso,ln,iseed)
{
nret=c("d0","dat0","sg");
idii="";
while(1)
	{
	# del = m1-M0; del < 0 ==> OVERLAP
	j=m.update(d0); m1=j$m1; M0=j$M0; del=m1-M0;
	while(del >= 1.5*sg)
		{
		xx=(M0+m1)/2;
		id=paste(idii,"I2(ib)",sep="");
		u=gd0(xx,d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0; 		
		j=m.update(d0); m1=j$m1; M0=j$M0; del=m1-M0;
		}
	if(del < 0) {ret=list(d0,dat0,sg); names=nret; return(ret);}
	j=n.update(d0); n0=j$n0; n1=j$n1;
	if(del >= 0) 
		{
		if(n0 > n1)
			{
			xx=m1+0.3*sg;
			id=paste(idii,"I2(ic)",sep="");
			u=gd0(xx,d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
			if(d0$Y[nrow(d0)] == 0) {ret=list(d0,dat0,sg); names=nret; return(ret);}
			if(d0$Y[nrow(d0)] == 1)
				{
				xx=M0-.3*sg;	
				id=paste(idii,"I2(ic)",sep="");
				u=gd0(xx,d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
				if(d0$Y[nrow(d0)] == 1) {ret=list(d0,dat0,sg); names=nret; return(ret);}
				if(d0$Y[nrow(d0)] == 0)
					{
					sg=2*sg/3;	
					idii=paste(idii,"r",sep="");	
					}
				}
			}
		if(n0 <= n1)
			{
			xx=M0-.3*sg;
			id=paste(idii,"I2(id)",sep="");
			u=gd0(xx,d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
			if(d0$Y[nrow(d0)] == 1) {ret=list(d0,dat0,sg); names=nret; return(ret);}
			if(d0$Y[nrow(d0)] == 0)
				{
				xx=m1+.3*sg;	
				id=paste(idii,"I2(id)",sep="");
				u=gd0(xx,d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
				if(d0$Y[nrow(d0)] == 0) {ret=list(d0,dat0,sg); names=nret; return(ret);}
				if(d0$Y[nrow(d0)] == 1)
					{
					sg=2*sg/3;	
					idii=paste(idii,"r",sep="");	
					}
				}
			}
		}
}
ret=list(d0,dat0,sg); names(ret)=nret; return(ret);
}
