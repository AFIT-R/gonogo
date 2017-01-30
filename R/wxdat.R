#' Title
#'
#' @param ic 
#'
#' @return
#' @export
#'
#' @examples
wxdat <-
function(ic)
{
# run wxdat.R. Then, wx=wxdat(i), for i= 1,...,25 defines various data
# sets (in list format) considered while developing these LR graphics

# needs picdat (from jlrcb.R) and lrmax (from cbs.R)
# These are some data sets I've used to benchmark the LR plots, particularly 
# jlrbc and lrbc and cbs. The source("wxdat.R") defines a function wxdat(ic)
# for ic: 1 <= ic ,+ 25. The oddest data sets, corresponding to ic=3,5,6,15,16,
# 17 through 24 were used primarily to develop the jlrcb plot function (ptest(w,5) 
# and pstest(w,5)).

# 
# 		  ic 	                              Description
#		-----	--------------------------------------------------------------------
#  		    1 My Neyer Demo Data, n=8 (SenTest input - My8Shot.sen)
#  		    2	Neyer Data from MIL-STD-331, n=20 (Appendix G, p228)
#  		    3	Neyer No ZMR Data, n=8 (SenTest input: nozmr.sen)
#  		    4	Neyer Data, n=30
#  		    5	No ZMR, n=17
#  		    6	Infinite Sigma Data, n=4 (SenTest input - InfSig.sen)
#  		    7	Velocity, n=15 
#  		    8	VariDensity, n=24
#  		    9	VariGap, n=21
# 		   10 NO ZMR Example
# 		   11 Dror & Steinberg, RP SOR 0607
#		   12	Eli Data n=73
#		   13	A Neyer Data Set
#		   14	JF's Data
#		   15	An n=3 Ex.
#		   16	An n=4, con=.5 Ex.
#		17-19	No Overlap Ex's, (n=3, 2, 4).
#		20-24	One Point of Overlap Examples
#		   25 A Simulated example

ad="";
switch(ic,
		{	# 1
			titl1="My SenTest Ex.";
			xx=c(13,15,14,15.814,14.5,13.2197,15.0273,13.6665); 
			yy=c(0,1,0,1,1,0,1,1);
			n=rep(1,length(xx));
 		},
		{	# 2
			titl1="MIL-STD-331 Ex.";
			xx=c(1.00,1.20,1.40,1.80,2.60,4.20,3.40,3.80,4.00,4.10,
			4.28,4.52,5.55,5.24,6.37,6.08,7.38,7.09,6.89,6.74);
			yy=c(0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,1,1,1,1);
			n=rep(1,length(xx));
		},
		{	# 3
			titl1="No Overlap Ex.";
			xx=c(14,16,15,16.814,15.5,16.8058,14.9669,16.3314);
			yy=c(0,1,0,1,0,1,0,1);
			n=rep(1,length(xx));	
		},
		{	# 4
			titl1="Neyer Data, n=30";
			xx=c(60,70,65,68,64,60,60,68,61,67,62,70,62,69,
			63,69,63,63,69,71,70,70,62,62,70,62,70,61,60,58);
			yy=c(0,1,0,1,1,0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0);
			n=rep(1,length(xx));	
		},
		{	# 5
			titl1="No ZMR, n=17";
			xx=c(23,17,14,12.5,9.75,11.13,12.57,11.85,12.21,14.61,13.41,12.63,11.19,11.91,12.66,12.4,12.3);
			yy=c(1,1,1,1,0,0,1,0,0,1,1,1,0,0,1,0,0);
			n=rep(1,length(xx));	
		},
		{	# 6
			titl1="Infinite Sigma Case";
			# When the S >= 7, that's when Sigma=Infinity, and Response Prob is Constant (3/4)
			S=8;
			xx=c(10,12,11,S);
			yy=c(0,1,1,1);
			n=rep(1,length(xx));	
		},
		{	# 7
			titl1="Velocity, n=15";
			xx=c(656,900,950,984,1000,1022,1145,1164,1305,1313,1450,1457,1500,1625,1750)/1000;
			yy=c(0,0,0,0,0,0,1,0,1,1,1,1,0,1,1);
			n=c(1,1,4,1,1,1,1,1,1,1,3,1,1,1,1); 
			xx=rep(xx,n); yy=rep(yy,n);
			n=rep(1,length(xx));
		},
		{	# 8
			titl1="VariDensity, n=24";
			xx=c(1.8510,1.8505,1.8505,1.5530,1.6590,1.7580,1.7040,1.7560,1.8000,1.6590,1.7090,
			1.7570,1.8020,1.7040,1.7580,1.7266,1.7400,1.7472,1.7450,1.7540,1.8010,1.7241,1.7080,1.7311);
			yy=c(1,1,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,0,1,1,1,0,1);
			n=rep(1,length(xx));
		},
		{	# 9
			titl1="VariGap, n=21";
			xx=c(4975,6260,5850,5310,6080,5950,5775,5910,5740,5970,5890,5800,5730,5630,5420,5470,5500,5535,5495,5510,5610)/10000;
			yy=c(0,1,0,0,1,1,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0);
			n=rep(1,length(xx)); 
		},
		{	# 10
			titl1="NO ZMR Example"; 	
			xx=c(45,23,34,29,26,31.5,27.5,23.5,29.1,25,26.8,24.9,27.6,26.4,25.3,26.9,25.6,25.8,26.2);
			yy=c(1,0,1,1,0,1,1,0,1,0,1,0,1,1,0,1,0,0,0);
			n=rep(1,length(xx));	
		},
		{	# 11
			titl1="Dror & Steinberg (Ref H, n=40)"; 
			xx=c(18.00,19.00,20.00,21.00,20.00,19.00,18.00,19.00,18.00,18.00,18.25,18.50,18.75,19.00,19.25,19.00,
			18.75,19.00,18.75,19.00,19.25,19.00,19.25,19.00,18.75,19.00,18.75,19.00,19.25,19.50,19.25,19.00,
			18.75,18.50,18.75,19.00,18.75,18.50,18.75,18.50);
			yy=c(0,0,0,1,1,1,0,1,0,0,0,0,0,0,1,1,0,1,0,0,1,0,1,1,0,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0); 
			n=rep(1,length(xx));	
			#xx=c(18.0,18.25,18.5,18.75,18.75,19.0,19.0,19.25,19.25,19.5,20.0,20.0,21.0);
			#yy=c(0,0,0,0,1,0,1,0,1,1,0,1,1); n=c(4,1,4,8,1,6,7,1,4,1,1,1,1);
		},
		{	# 12
			titl1="Eli data (n=73)"; 
			xx=c(3.5,4.0,4.0,4.5,4.5,5.0,5.0,5.5,5.5,6.0);
			fac=1; xx=fac*xx;
			yy=c(0,0,1,0,1,0,1,0,1,1);
			n=c(5,23,1,6,5,2,6,3,7,15);
		},
		{	# 13
			titl1="A Neyer Test";
			xx=c(800,807,884,900,910,913,923,961,968,969,972,993,1000,
			1012,1013,1015,1025,1033,1038,1051,1060,1072,1129,1150,1219);
			yy=c(0,0,0,0,0,0,0,1,1,0,0,0,1,1,1,0,1,1,0,1,1,1,1,1,1);
			n=rep(1,length(xx));
		},
		{	#14
			titl1="JF's Data";
			xx=c(1.375,1.53,1.275,1.32,1.351,1.334,1.32,1.34,1.349,1.327,1.315,1.344,1.318,1.337,
			1.322,1.336,1.324,1.345,1.344,1.348,1.32,1.376,1.373,1.371,1.369,1.387,1.384,1.382,1.302,
			1.379,1.376,1.307,1.309,1.285,1.399,1.289)
			yy=c(1,1,0,0,1,1,0,0,1,1,0,1,0,1,0,0,0,0,0,0,1,1,1,1,0,1,1,1,0,0,1,1,0,0,1,1);
			n=rep(1,length(xx));
		},
		{ 	#15 
			titl1="An n=3 Ex."
			xx=c(1,3,4,5); yy=c(0,1,1,0); n=rep(1,length(xx));
			xx=c(1,3,4); yy=c(0,1,0); n=rep(1,length(xx));
		},
		{	#16
			titl1="An n=4, con=.5 Ex."
			xx=c(1,3,4,5); yy=c(0,1,1,0); n=rep(1,length(xx));
		},
		{	# 17
			titl1="No overlap (A)"; 
			xx=c(14,15,16); yy=c(0,1,1); tit1="No Overlap (n=3) Ex.";
			n=rep(1,length(xx));

		},
		{	# 18
			titl1="No overlap (B)"; 
			xx=c(14,16); yy=c(0,1); tit1="No Overlap (n=2) Ex.";
			n=rep(1,length(xx));

		},
		{	# 19
			titl1="No overlap (C)"; 
			xx=c(13,14,15,16); yy=c(0,0,1,1); tit1="No Overlap (n=4) Ex.";
			n=rep(1,length(xx));

		},
		{	# 20
			titl1="One point overlap (A)"; 
			xx=c(14,14); yy=c(0,1); #Need to do more testing
			n=rep(1,length(xx));
		},
		{	# 21
			titl1="One point overlap (B)"; 
			xx=c(14,16,16); yy=c(0,0,1); 
			n=rep(1,length(xx));
		},
		{	# 22
			titl1="One point overlap (C)"; 
			xx=c(14,16,16,16); yy=c(0,0,1,1); 
			n=rep(1,length(xx));
		},
		{	# 23
			titl1="One point overlap (D)"; 
			xx=c(14,16,16,18); yy=c(0,0,1,1); 
			n=rep(1,length(xx));
		},
		{	# 24
			titl1="One point overlap (E)"; 
			xx=c(14,16,16,16,16); yy=c(0,0,0,1,1); 
			n=rep(1,length(xx));
		},
		{ 	# 25
			titl1="A Simulated Test";
			xx=c(5.50000,16.50000,9.52628,7.23841,6.58790,10.46693,6.79797,10.14349,
			8.30392,6.94172,8.65883,7.03924,7.47790,6.81379,8.16104,8.86730,
			9.73525,9.57780,10.63076,10.45669,11.68522,23.80358,19.33242,17.14054,
			15.87435,15.04495,14.45109,17.11908,16.67776,16.31401,16.00650,15.74132,
			15.50899,15.30279,15.11780);
			yy=c(0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,0,1,0,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1);
			n=rep(1,length(xx));
		}
);

titl1=paste(ic,ad,". ",titl1,sep="");
dat=matrix(c(xx,yy,n),ncol=3); 
dat=data.frame(dat);
names(dat)=c("X","Y","COUNT");

w=list(dat,titl1);
names(w)=c("d0","title");
g=lrmax(w,plt=T);
return(g)
}
