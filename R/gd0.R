#' Title
#'
#' @param xx 
#' @param d0 
#' @param dat0 
#' @param ID 
#' @param mu 
#' @param sig 
#' @param reso 
#' @param ln 
#' @param iseed 
#'
#' @return
#' @export
#'
#' @examples
gd0 <-
function(xx,d0,dat0,ID,mu,sig,reso,ln,iseed=-1)
{
nret=c("d0","dat0");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6))); d1=cbind(d1,"END"); 
names(d1)=names(d0)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d1[-1,];
n0=nrow(dat0); nd0=nrow(d0)+1;
# print(c(n0,nd0));
iset=0; if(iseed >= 0)iset=nd0+iseed;
if(n0 == 0)
	{
	u=gxr(xx,mu,sig,reso,ln,iset);
	d1[1,1:6]=c(u[1:2],1,u[3],xx,u[4]); d1$ID=ID;
	} 	
if(n0 > 0)	{d1=dat0[1,]; dat0=dat0[-1,]; if(is.null(dat0)) dat0=d1[-1,]; n0=nrow(dat0);}
d0=rbind(d0,d1);
ret=list(d0,dat0);	
names(ret)=nret;
return(ret);
}
