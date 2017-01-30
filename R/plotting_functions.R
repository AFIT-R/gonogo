#' History plot
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
pdat1 <- function(dat) {
  dt <- dtt <- dat$d0
  about <- dat$about
  titl <- dat$titl
  unit <- dat$unit
  ln <- dat$ln

  # pee & neyer aren't defined while running the test. Need to infer neyer.
  pee <- dat$p
  neyer <- dat$neyer

  x <- dt$X
  y <- dt$Y
  id <- dt$ID
  nid <- length(id)

  if(is.null(about)) {
    cat("This function only works for lists created by gonogo\n\n")
    return()
    }

  if(is.null(neyer)) {
    neyer <- F
    b <- gsub('[0-9]+', '', id[1])

    if(b == "B"){
      neyer <- T
    }
  }

  if(length(pee) == 0) {
    pee=0
  }

  fini <- 0
  if(id[nid] == "III3") {
    fini <- 1
  }

  if(fini == 1) {
    dtt <- dtt[-nid,]
    x <- x[-nid]
    y <- y[-nid]
    id <- id[-nid]
    nid <- nid-1
  }

  zee=x[1]

  if(pee*(1-pee) > 0 & fini == 1) {
    yu <- glmmle(dtt)
    zee <- yu$mu + qnorm(pee) * yu$sig
  }

  about1 <- expression(paste("{", mu[lo], ",", mu[hi], ",", sigma[g], "|", n[1],
                             ",", n[2], ",", n[3], "|p,", lambda, ",res}", sep=""))

  w <- pretty(x, n = 10)
  ens <- 1:nid
  rd <- which(y == 1)
  gr <- which(y == 0)
  ylm <- range(pretty(c(x, max(x, na.rm = TRUE) + diff(range(x))/80), n = 10))

  # for tick locations
  lb <- nid-1
  if(lb > 30) {
    lb <- ceiling(lb/2)
  }

  if(nid == 1) {
    return()
  }

  if(nid > 1) {
    par(mar = c(4,4,5,2) + 0.1)
    lnum <- 2.3

    if(!ln) {
      plot(c(ens, 1), c(x, zee), type = "n", xlab = "", ylab = "", ylim = ylm, lab = c(lb,5,7))
      } else {
      par(mar = c(4,3,5,3) + 0.1)
      plot(c(ens, 1), c(x,zee), type = "n", xlab = "", ylab = "", ylim = ylm, yaxt = "n")
      w7 <- pretty(exp(x), n = 6)
      axis(2, at = log(w7), lab = round(w7,1), srt = 90, tcl = -.4, mgp = c(1,.5,0))
      w8 <- pretty(x, n = 6)
      axis(4, at = w8, lab = round(w8,1), srt = 90, tcl = -.4, mgp = c(1,.5,0))
      mtext("Log Scale", side = 4, line = 1.6)
      lnum <- 1.8
    }

    mtext(paste("Test Level (", unit, ")", sep = ""), side = 2, line = lnum)
    mtext("Trial Number", side = 1, line = 2.2)

    points(ens[rd], x[rd], pch = 25, cex = .7, bg = 4)
    points(ens[gr], x[gr], pch = 24, cex = .7, bg = 3)

    if(neyer) {
      tf <- addneyr(dtt, ylm)
      }else {
        tf <- add3pod(dtt, ylm)
      }

    mtext(titl, side = 3,line = 3.4, cex = 1.2, col = 1)
    mtext(about1, side = 3, line = 1.8, cex = 1.2)
    if(tf[1] & neyer) {
      about <- chabout(about, nrow(dtt), 4)
    }

    mtext(about, side = 3, line = 0.5, cex = 1.2)

    if(fini == 1) {
      axis(4, label = F, at = dt$RX[nid + 1], tcl = .25, lwd = 2) 	# Next EX had test cont'd (BL, Inside Box)
      axis(4, label = F, at = zee, tcl = -.25, lwd =2)		# zee = pth quantile (BL, Outside Box)
    }
    reset()
  }
}

#' MLE plot
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
pdat2=function(dat)
{
  dt=dtt=dat$d0; about=dat$about; titl=dat$titl; ln=dat$ln;
  if(is.null(about)) {cat("This function only works for lists created by gonogo\n\n"); return();}
  # pee & neyer aren't defined while running the test. Need to infer neyer.
  pee=dat$p;
  id=dt$ID; nid=length(id);
  if(length(pee) == 0) pee=0;
  fini=0; if(id[nid]=="III3") fini=1;
  if(fini == 1) {dtt=dtt[-nid,]; id=id[-nid]; nid=nid-1;}
  if(pee*(1-pee) > 0 & fini == 1) { yu=glmmle(dtt); zee=yu$mu+qnorm(pee)*yu$sig; }
  about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[1],",",n[2],",",n[3],"|p,",lambda,",res}",sep=""));

  kp=0;
  for(j in 1:nid)
  {
    jj=m.update(dtt[1:j,]);
    M0=jj$M0; m1=jj$m1;
    uv=c(M0,m1);
    if(!any(is.na(uv))) {if(M0 > m1) kp=j;}
    if(kp > 0) break;
  }
  mus=sigs=zee=zee0=rep(0,nid-kp+1);

  if(kp == 0) cat("ptest(z,plt=2) option requires having completed Phase I2 (i.e., achieving overlap)\n");
  if(kp > 0)
  {
    for(j in kp:nid) {g=glmmle(dtt[1:j,]); mus[j-kp+1]=g$mu; sigs[j-kp+1]=g$sig;}
    if(pee > 0 & pee < 1)zee=mus+qnorm(pee)*sigs;
    par(mfrow=c(2,1), mar=c(1.5,2.5,.5,.5), oma=c(2,2,3.5,2));
    lmu=pretty(mus); lsig=pretty(sigs); lx=pretty(c(kp,nid)); rx=kp:nid; rxx=range(rx);
    if(diff(rxx)==0)rxx=rxx+c(-1,1)
    plot(kp:nid,mus,type="l",xlab="",ylab="",xlim=rxx,xaxt="n",ylim=range(lmu),yaxt="n");
    axis(1,at=kp:nid,labels=T,tck=-.03,mgp=c(1,.4,0),cex.axis=.8);
    axis(2,at=lmu,labels=T,tck=-.02,mgp=c(1,.4,0),las=2,cex.axis=.8);
    if(ln) mtext("Mean(Log)",side=2,line=3,cex=1) else mtext("Mean",side=2,line=3,cex=1);

    lt=3; abline(h=lmu,lty=lt); abline(v=lx,lty=lt);
    points(kp:nid,mus,pch=16,cex=.8);
    if(kp == nid) nlx=2 else nlx=nid-kp;
    plot(kp:nid,sigs,type="l",xlab="",ylab="",ylim=range(lsig),yaxt="n",xlim=rxx,xaxt="n");
    axis(1,at=kp:nid,labels=T,tck=-.03,mgp=c(1,.4,0),cex.axis=.8);
    axis(2,at=lsig,labels=T,tck=-.02,mgp=c(1,.4,0),las=2,cex.axis=.8);
    mtext("Cumulative Test Size (n)",side=1,line=0,cex=1,outer=T);
    if(ln) mtext("SD(Log)",side=2,line=3,cex=1) else mtext("SD",side=2,line=3,cex=1);

    abline(h=lsig,lty=lt); abline(v=lx,lty=lt);
    points(kp:nid,sigs,pch=16,cex=.8);
    par(mfrow=c(1,1));
    els=c(2.5,1);
    mtext(paste(titl,": Sequence of MLE's",sep=""),side=3,line=2.8,cex=1.1);
    mtext(about1,side=3,line=1.4,cex=1.1);
    mtext(about,side=3,line=.3,cex=1.1);
  }
  reset();
  if(pee == 0) return(matrix(c(mus,sigs),ncol=2)) else return(matrix(c(mus,sigs,zee),ncol=3));
}

#' Response curve plot
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
pdat3 <- function(dat)
{
  dt=dtt=dat$d0; about=dat$about; titl=dat$titl; unit=dat$unit; ln=dat$ln; pee=dat$p;
  if(is.null(about)) {cat("This function only works for lists created by gonogo\n\n"); return();}
  id=dt$ID; nid=length(id);
  if(length(pee) == 0) pee=0;
  fini=0; if(id[nid]=="III3") fini=1;
  if(fini == 1) {dtt=dtt[-nid,]; id=id[-nid]; nid=nid-1;}
  about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[1],",",n[2],",",n[3],"|p,",lambda,",res}",sep=""));

  kp=0;
  for(j in 1:nid)
  {
    jj=m.update(dtt[1:j,]);
    M0=jj$M0; m1=jj$m1;
    uv=c(M0,m1);
    if(!any(is.na(uv))) {if(M0 > m1) kp=j;}
    if(kp > 0) break;
  }
  if(kp == 0) cat("ptest(z,plt=3) option requires having completed Phase I2 (i.e., achieving overlap)\n");
  if(kp > 0)
  {
    if(ln) z=nyqrda(dtt,ln=T,response=1,labx=unit) else
      z=nyqrda(dtt,response=1,labx=unit);
    rmzm=round(z$mu,3); rmzs=round(z$sig,3);
    about2=substitute(paste(titl,", (",hat(mu),",",hat(sigma),",n) = (",rmzm,",",rmzs,",",nid,")",sep=""));
    mtext(about2,side=3,line=2.7,cex=1.1);
    mtext(about1,side=3,line=1.4,cex=1.1);
    mtext(about,side=3,line=.3,cex=1.1);
  }
  return()
}
