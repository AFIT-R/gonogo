#' Title
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
pSdat3 <-
function(dat)
{
dt=dtt=dat$d0; about=dat$about; titl=dat$titl; unit=dat$unit;
ln=dat$ln; iseed=dat$iseed;
tmu=dat$tmu; tsig=dat$tsig;
M=dat$M; dm=dat$dm; ds=dat$ds;
rmzm=round(tmu,4); rmzs=round(tsig,4);
if(iseed < 0)titl1=substitute(paste(titl,": (",mu[t],", ",sigma[t],") = (",rmzm,", ",rmzs,"), ",delta[t]," = (",dm,", ",ds,")",sep="")) else
titl1=substitute(paste(titl,": (",mu[t],", ",sigma[t],") = (",rmzm,", ",rmzs,"), ",delta[t]," = (",dm,", ",ds,"), ",i[seed]," = ",iseed,sep=""))
id=dt$ID; nid=length(id);
fini=0; if(id[nid]=="III3") fini=1;
if(fini == 1) {dtt=dtt[-nid,]; nid=nid-1;}

if(M == 1)about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[11],",",n[12],",",n[2],",",n[3],"|p,",lambda,",res}",sep="")) else
about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[11],",",n[12],",",n[2],",",n[3],"|p,",lambda,",res,M}",sep=""));

kp=0; 
        for(j in 1:nid) 
        {       
        jj=m.update(dtt[1:j,]); 
        M0=jj$M0; m1=jj$m1; 
        uv=c(M0,m1); 
        if(!any(is.na(uv))) {if(M0 > m1) kp=j;}
        if(kp > 0) break;
        }

if(kp == 0) cat("pStest(z,plt=3) option requires having completed Phase I2 (i.e., achieving overlap)\n");
if(kp > 0)
{
if(ln) z=nyqrda(dtt,ln=T,response=1,labx=unit) else 
z=nyqrda(dtt,response=1,labx=unit);
mtext(titl1,side=3,line=2.8,cex=1.1);
mtext(about1,side=3,line=1.4,cex=1.1);
mtext(about,side=3,line=.2,cex=1.1);
}
return()
}
