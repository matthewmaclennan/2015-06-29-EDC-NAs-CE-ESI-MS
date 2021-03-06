#Here is the script for performing polynomial regression on the data for 5 factors.
#The data (not listed here) are:
###pwidth, swathz, migz, current, and atime.
#Convert matrix into appropriate format for lm() function
pwidthlm<-cbind(c(0,10,30,50,70),c(rep(0.5,5),rep(2,5),rep(3.5,5)),c(pwidth))
swathzlm<-cbind(c(0,10,30,50,70),c(rep(0.5,5),rep(2,5),rep(3.5,5)),c(swathz))
migzlm<-cbind(c(0,10,30,50,70),c(rep(0.5,5),rep(2,5),rep(3.5,5)),c(migz))
currentzlm<-cbind(c(0,10,30,50,70),c(rep(0.5,5),rep(2,5),rep(3.5,5)),c(currentz))
atimezlm<-cbind(c(0,10,30,50,70),c(rep(0.5,5),rep(2,5),rep(3.5,5)),c(atimez))
#Perform polynomial regression(s) on these. Since columns 1 and 2 of pwidthlm are identical to columns 1 and 2 of the others, 
#we'll just use the same columns for all regressions.
pwidth.poly<-lm(pwidthlm[,3]~I(pwidthlm[,1])+I(pwidthlm[,2])+I(pwidthlm[,1]*pwidthlm[,2])+I((pwidthlm[,1])^2)+I((pwidthlm[,2])^2))
swathz.poly<-lm(swathzlm[,3]~I(pwidthlm[,1])+I(pwidthlm[,2])+I(pwidthlm[,1]*pwidthlm[,2])+I((pwidthlm[,1])^2)+I((pwidthlm[,2])^2))
migz.poly<-lm(migzlm[,3]~I(pwidthlm[,1])+I(pwidthlm[,2])+I(pwidthlm[,1]*pwidthlm[,2])+I((pwidthlm[,1])^2)+I((pwidthlm[,2])^2))
currentz.poly<-lm(currentzlm[,3]~I(pwidthlm[,1])+I(pwidthlm[,2])+I(pwidthlm[,1]*pwidthlm[,2])+I((pwidthlm[,1])^2)+I((pwidthlm[,2])^2))
atimez.poly<-lm(atimezlm[,3]~I(pwidthlm[,1])+I(pwidthlm[,2])+I(pwidthlm[,1]*pwidthlm[,2])+I((pwidthlm[,1])^2)+I((pwidthlm[,2])^2))
#
#
pwidthlmw<-cbind(pwidthlm[,-3],apply(pwidthlm[,-3],1,function(x) 100-sum(x)),pwidthlm[,3])
swathzlmw<-cbind(swathzlm[,-3],apply(swathzlm[,-3],1,function(x) 100-sum(x)),swathzlm[,3])
migzlmw<-cbind(migzlm[,-3],apply(migzlm[,-3],1,function(x) 100-sum(x)),migzlm[,3])
currentzlmw<-cbind(currentzlm[,-3],apply(currentzlm[,-3],1,function(x) 100-sum(x)),currentzlm[,3])
atimezlmw<-cbind(atimezlm[,-3],apply(atimezlm[,-3],1,function(x) 100-sum(x)),atimezlm[,3])
#Add in the columns for % water and approximate pH. Function percent2pH() is available as *.R file also at
#www.github.com/matthewmaclennan/R-chemistry/Response-Surface/percent2pH.R
pwidthlmwph<-cbind(pwidthlm[,-3],apply(pwidthlm[,-3],1,function(x) 100-sum(x)),
-logb(t(apply(pwidthlmw,1,function(x) percent2pH(x[2],1.22,46,1.77e-4,x[3])))[,1],10),
pwidthlm[,3])
#
#
#
swathzlmwph<-cbind(swathzlm[,-3],apply(swathzlm[,-3],1,function(x) 100-sum(x)),
-logb(t(apply(swathzlmw,1,function(x) percent2pH(x[2],1.22,46,1.77e-4,x[3])))[,1],10),
swathzlm[,3])
migzlmwph<-cbind(migzlm[,-3],apply(migzlm[,-3],1,function(x) 100-sum(x)),
-logb(t(apply(migzlmw,1,function(x) percent2pH(x[2],1.22,46,1.77e-4,x[3])))[,1],10),
migzlm[,3])
currentzlmwph<-cbind(currentzlm[,-3],apply(currentzlm[,-3],1,function(x) 100-sum(x)),
-logb(t(apply(currentzlmw,1,function(x) percent2pH(x[2],1.22,46,1.77e-4,x[3])))[,1],10),
currentzlm[,3])
atimezlmwph<-cbind(atimezlm[,-3],apply(atimezlm[,-3],1,function(x) 100-sum(x)),
-logb(t(apply(atimezlmw,1,function(x) percent2pH(x[2],1.22,46,1.77e-4,x[3])))[,1],10),
atimezlm[,3])
