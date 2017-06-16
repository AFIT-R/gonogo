#' gonogo
#'
#' gonogo description
#' Dynamic sequential algorithms for sensitivity testing (binary response information).
#' Choose stress levels to quickly achieve an overlapping data pattern such that parameter estimability is ensured
#'
#' @param mlo Guess for mu_min
#' @param mhi Guess for mu_max
#' @param sg Guess for standard deviation, should satisfy: mhi - mlo >= 6*sg
#' @param newz With the newz=F option, it looks for a list, saved as "z", in the immediate environment
#' @param reso A "reso=.125" option, e.g., recommends test levels rounded to the nearest one eighth.
#' @param ln test in log(X) units (Experimental ln=T)
#' @param neyer Neyer test (neyer=T) or three-phase optimal design (3pod) test (neyer=F)
#'
#' @return list(d0,about,titl,unit,en,p,reso,n2n3,ln,init,lam,neyer,savinit,jvec)
#' @export
#'
#' @examples
gonogo <-
function(mlo=0,mhi=0,sg=0,newz=T,reso=0,ln=F,neyer=F)
{
	jvec=NULL;

	if(mlo == 0 & mhi == 0 & sg == 0 & newz == T)
	{
	cat("Minimal entries to gonogo are: (1) mlo, mhi, sg; or (2) newz=F.\nTry again.\n\n");
	return();
	} else {	n2n3=0; endi=0;	sgrem=sg; 	g=", "; }
	if(!newz) {neyer=z$neyer; about = z$about;}
	if(neyer) blrb2() else blrb3();

	del5=(mhi-mlo)/6;
	epsi=del5/1000;
	if(sg>(mhi-mlo)/6+epsi){cat(paste("sg is too big (sg <= ",round(del5,4),")\nTry again\n\n",sep="")); return();}

	xx="Enter title (without quotes): "; yy="Enter units (without quotes): ";
	if(newz) {dat0=data.frame(numeric(0)); titl=readline(xx); unit=readline(yy); cat("\n"); n1=n2=n3=p=lam=0;
	en=c(n1,n2,n3); about=wabout(c(mlo,mhi,sg,n1,n2,n3,p,lam,reso)); savinit=c(mlo,mhi,sg);} else
	{dat0=z$d0; about=z$about; titl=z$title; unit=z$units; en=z$en; n1=en[1]; n2=en[2]; n3=en[3];
	p=z$p; reso=z$reso; n2n3=z$n2n3; ln=z$ln; init=z$init; mlo=init[1]; mhi=init[2]; sg=init[3];
	lam=z$lam; neyer=z$neyer; savinit=z$savinit;}

	if(newz)
	{
	if(ln) h="log " else h="";
	if(neyer) h=paste(h,"Neyer: ",sep="") else h=paste(h,"3pod: ",sep="");
	titl=paste(h,titl,sep="");
	}

	if(ln & newz) { u=fgs(mlo,mhi,sg); mlo=u[1]; mhi=u[2]; sg=u[3]; }

	init=c(mlo,mhi,sg)

	if(!newz){n2n3=prd0(z);}

	# n1 is the random test quantity eventually required to complete Phase I

	if(!neyer)
{
	if(endi == 0)
	{w=phaseI1(dat0,mlo,mhi,sg,reso,about,titl,unit,ln);
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; sg=w[[4]];}	# NEW with 3podm

	if(endi == 0)
	{w=phaseI2(d0,dat0,sg,reso,about,titl,unit,ln);
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; sg=w[[4]]; }

	if(endi == 0)
	{w=phaseI3(d0,dat0,sg,reso,about,titl,unit,ln);
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; }
	n1=nrow(d0);
} else
{
	if(endi == 0)
	{w=nphaseI(dat0,mlo,mhi,sg,reso,about,titl,unit,ln);
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; sg=w[[4]];}	# NEW with 3podm
	n1=nrow(d0);
}
	# Read here n2: the number of Phase II (D-Optimal) tests to run

	if(endi == 0 & n2n3 != 2 & n2n3 != 3 & n2n3 != 4 & n2n3 != 5)
	{
		gg=glmmle(d0); g1=round(gg$mu,5); g2=round(gg$sig,5);
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		xx=paste("\nPhase I complete, (Mu, Sig) = (",g1,", ",g2,").\nEnter Phase II (D-Optimal) size n2: ",sep="");
		if(n2 == 0) {n2=as.numeric(readline(xx)); if(n2==0)n2n3=2; cat("\n");}
		if(n2 < 0) {n2=0; endi=1;}
		if(n2 > 0)
		{
		en[2]=n2;

		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		w=phaseII(d0,dat0,n2,reso,about,titl,unit,ln); d0=w[[1]]; dat0=w[[2]]; endi=w[[3]];
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		}
	}

	if(endi == 0)
		{
		if(n3 == 0)
			{
			gg=glmmle(d0); g1=round(gg$mu,5); g2=round(gg$sig,5);
			jkl="complete"; if(n2 == 0) jkl="skipped";
			zz="";
			if(ln)zz=paste("\n\n** Starting values (tau2[1] & be) for Phase III, ln=T may need tweaking.\n",sep="");
			xx=paste("\nPhase II ",jkl,", (Mu, Sig) = (",g1,", ",g2,").",zz,"\nEnter Phase III (S-RMJ) size n3: ",sep="");
			xx=readline(xx); n3=as.numeric(xx);
			if(n3 == 0)endi=8;
			if(n3 < 0){n3=0; endi=1;}
			about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
			}
		}

	if(endi == 0 & n3 > 0 & p == 0)
		{
		xx="Enter p lam: ";
		if(p==0)
			{
			xx=readline(xx);
			xx=strsplit(xx," ",fixed=T);
			xx=as.numeric(xx[[1]]);
			nxx=length(xx);
			p=xx[1]; if(nxx > 1)lam=xx[2];
			cat("\n");
			}
		if(p >= 1 | p <= 0 | lam <= 0 | nxx != 2)
			{p=0; lam=0; endi=1; n2n3=3; if(n2 > 0) n2n3=6; }
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		}
	if(endi == 0) {
		n2n3=4; if(n2 > 0) n2n3=7;
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		w=sphaseIII(d0,dat0,n3,p,reso,about,titl,unit,ln,lam); d0=w[[1]];
		dat0=w[[2]]; endi=w[[3]]; jvec=w[[4]];	}

		#	Adapted from bottom of prd0
		if(endi == 0 | endi == 2 | endi == 8) {
		gg=glmmle(d0); g1=round(gg$mu,5); g2=round(gg$sig,5);
		xx=paste("\nPhase III complete, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
		cat(xx);
		} else cat("Test Suspended\n")

	en=c(n1,n2,n3);
	ret=list(d0,about,titl,unit,en,p,reso,n2n3,ln,init,lam,neyer,savinit,jvec);
	names(ret)=c("d0","about","title","units","en","p","reso","n2n3","ln","init","lam","neyer","savinit","jvec");
	if(endi == 2 & n1 > 1) ptest(ret,1);

	rd0=d0; rd0$X=round(rd0$X,5); names(rd0)[1]="i,X"; rd0$EX=round(rd0$EX,5);
	write.table(rd0,file="gonogo.txt",quote=F,sep=",",na="i");
	return(ret);
}
