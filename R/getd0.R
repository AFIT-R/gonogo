#' Title
#'
#' @param xx 
#' @param d0 
#' @param dat0 
#' @param ID 
#' @param reso 
#' @param about 
#' @param titl 
#' @param unit 
#' @param ln 
#' @param cab 
#'
#' @return
#' @export
#'
#' @examples
getd0 <-
function(xx,d0,dat0,ID,reso,about,titl,unit,ln,cab=F)
{
nret=c("d0","dat0","endi");
mret=c("d0","about","title","units","ln");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6))); d1=cbind(d1,"END"); 
names(d1)=names(d0)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d1[-1,];
n0=nrow(dat0); nd0=nrow(d0)+1;
endi=0;

if(n0 == 0)
	{
	u=getxr(xx,nd0,reso,ln);
	d1[1,1:6]=c(u[1:2],1,u[3],xx,u[4]); d1$ID=ID;
	if(u[2]*(1-u[2])!=0)
		{endi=1; ret=list(d0,dat0,endi); names(ret)=nret; 
			#if(cab)about=chabout(about,nrow(d0),4)
		ret5=list(d0,about,titl,unit,ln); names(ret5)=mret; 
		if(nrow(d0) > 0) {ptest(ret5,1);} 
		return(ret);
		}	
	} 	
if(n0 > 0)	{d1=dat0[1,]; dat0=dat0[-1,]; if(is.null(dat0)) dat0=d1[-1,]; n0=nrow(dat0);}
d0=rbind(d0,d1);
ret=list(d0,dat0,endi);
	if(cab)about=chabout(about,nrow(d0),4);
ret5=list(d0,about,titl,unit,ln); names(ret5)=mret;
if(nrow(d0) > 1) ptest(ret5,1);	
names(ret)=nret;
return(ret);
}
