#' Title
#'
#' @param w 
#'
#' @return
#' @export
#'
#' @examples
fixw1 <-
function(w)
{
d0=w$d0;
n=nrow(d0);

about=w$about;
en=w$en;
p=w$p;
n2n3=w$n2n3;
neyer=w$neyer;

l=about4(about);
l0=length(which(l==0));

if(l0 == 4)
{
	# Case 1
	cas=1; d0=d0[-n,]; if(n > 0)en[1]=n-1;
}

if(l0 == 3)
{
	# Case 2
	cas=2; d0=d0[-n,]; en[1]=en[1]-1; #l[1]=0;
}

if(l0 == 2)
{
	if(n > l[1] & n < l[1]+l[2])
	{
	# Case 4;
	cas=4; d0=d0[-n,]; #en[2]=en[2]-1;
	}

	if(n == l[1]+l[2])
	{
	# Case 5;
	cas=5; d0=d0[-n,]; #en[2]=en[2]-1;
	}

	if(n == l[1])
	{
	# Case 3;
	cas=3; en[2]=0; #l[2]=0;
	} 
}

if(l0 == 1)
{
	# Case 6
	cas=6; en[3]=0; n2n3=0; #l[3]=0;
}

if(l0 == 0)
{
	if(n == l[1]+l[2])
	{
	# Case 7
	cas=7; p=0; n2n3=6; #l[4]=0;
	}
	if(n > l[1]+l[2] & n <= l[1]+l[2]+l[3])
	{
	# Case 8
	cas=8; d0=d0[-n,]; #en[3]=en[3]-1;
	}
	if(n > l[1]+l[2]+l[3])
	{
	# Case 9
	cas=9; d0=d0[-n,]; d0=d0[-(n-1),]; #en[3]=en[3]-1;
	}
}

w$d0=d0;
w$en=en;
w$p=p;
w$n2n3=n2n3;

nen1=en[1]; if(en[2] == 0) nen1=0;

s47=c(nen1,en[2:3],p); loc=4:7;

if(cas > 1 | neyer) w$about=chabout(about,s47,loc);

rd0=d0; rd0$X=round(rd0$X,5); names(rd0)[1]="i,X"; 
rd0$EX=round(rd0$EX,5);
rd0$TX=round(rd0$TX,5);
write.table(rd0,file="fixw.txt",quote=F,sep=",",na="i");

return(w)
}
