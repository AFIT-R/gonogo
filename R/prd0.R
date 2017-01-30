#' Title
#'
#' @param z 
#'
#' @return
#' @export
#'
#' @examples
prd0 <-
function(z)
{
        d00=z$d0;
        en=z$en;
        pp=z$p;
	  llam=z$lam;
	  n2n3=z$n2n3;
        n=cumsum(en);
        n0=nrow(d00);
		ln=z$ln;
	  	u=d00$X;
		if(ln) u=round(exp(u),5);
        cat(paste("Enter title (without quotes): ",z$title,"\n",sep=""));
        cat(paste("Enter units (without quotes): ",z$units,"\n\n",sep=""));
        if(n0 == 0)return(n2n3)
        for(i in 1:n0)
        {
                buf=" ";if(i>9)buf="";
		xx = paste(buf,i,". Test at X ~ ",d00$RX[i],". Enter X & R: ",u[i]," ",d00$Y[i],"\n", sep = "");
		    cat(xx);

                if(i == en[1] & (en[2] != 0 | n2n3 ==2)){
                gg=glmmle(d00[1:n[1],]); g1=round(gg$mu,5); g2=round(gg$sig,5);
                xx=paste("\nPhase I complete, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
                cat(xx);
                        
				if(en[2] == 0 & i == en[1] & n2n3 == 2)
                        {
 				xx=paste("Enter Phase II (D-Optimal) size n2: ",en[2],"\n",sep="");
                        cat(xx);
                        #xx=paste("Phase II Skipped\n\n",sep="");
                        #cat(xx);
                        }

                        if(en[2] == 0 & en[3] > 0)
                        {
                        xx=paste("Enter Phase II (D-Optimal) size n2: ",en[2],"\n",sep="");
                        cat(xx);
                        #xx=paste("Phase II Skipped\n\n",sep="");
                        #cat(xx);
                        }

                        if(en[2] > 0)
                        {
                        xx=paste("Enter Phase II (D-Optimal) size n2: ",en[2],"\n\n",sep="");
                        cat(xx);
                        }
                }
                        
                if(en[2] != 0 & i == n[2]){
                if(en[3]>0)
		    {
		    gg=glmmle(d00[1:n[2],]); g1=round(gg$mu,5); g2=round(gg$sig,5);
                xx=paste("\nPhase II complete, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
                cat(xx);
		    }
                        i7=0
                        if(en[3] > 0)
                        {
                        xx=paste("Enter Phase III (RMJ) size n3: ",en[3],"\n",sep="");
                        cat(xx);
                        i7=1
                        }
                        if(pp > 0 & pp < 1)
                        {
                        xx=paste("Enter Phase III (RMJ) size n3: ",en[3],"\n",sep="");
                        if(i7 == 0)cat(xx);
                        xx=paste("Enter p lam: ",pp," ",llam,"\n\n",sep=""); 
                        cat(xx);
                        }
                }

                if(en[2] == 0 & en[3] > 0 & i >= n[2] & n2n3 != 5){
 		    gg=glmmle(d00[1:n[2],]); g1=round(gg$mu,5); g2=round(gg$sig,5);
                xx=paste("\nPhase I complete, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
                cat(xx);
                xx=paste("Enter Phase II (D-Optimal) size n2: ",en[2],"\n",sep="");
                cat(xx);
		    xx=paste("\n\nPhase II skipped, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
                cat(xx);
                xx=paste("Enter Phase III (RMJ) size n3: ",en[3],"\n",sep="");
                cat(xx);

		    if(n2n3 == 4) {xx=paste("Enter p lam: ",pp," ",llam,"\n\n",sep=""); cat(xx); }
		    n2n3=5;		
                }
        }
return(n2n3)
}
