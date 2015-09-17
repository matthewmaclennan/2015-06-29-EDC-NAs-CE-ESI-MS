#Code for generating rainbow coloured plots according to Z-number in a formula set (provided by function nagenmz())
#This example mainly shows examples of usage of versatile.points.match() and versatile.points.move().
versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==2 & edcna[,"Z-number"]==(-2) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=0.5,col="red",pch=1)
versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==2 & edcna[,"Z-number"]==(-4) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=0.5,col="orange",pch=1)
versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==2 & edcna[,"Z-number"]==(-6) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=0.5,col="yellow",pch=1)
versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==2 & edcna[,"Z-number"]==(-8) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=0.5,col="green",pch=1)
versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==2 & edcna[,"Z-number"]==(-10) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=0.5,col="blue",pch=1)
versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==2 & edcna[,"Z-number"]==(-12) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=0.5,col="purple",pch=1)

versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-4) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=1,col="red",pch="x")
versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-6) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=1,col="orange",pch="x")
versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-8) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=1,col="yellow",pch="x")
versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-10) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=1,col="green",pch="x")
versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-12) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=1,col="blue",pch="x")

versatile.points.match(dmsoNaM,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-14) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=7,cex=1,col="purple",pch="x")

roygbiv.dmeda2<-function(data,thresh,cex=1,pch=1,DBE){
for(i in 1:DBE){
versatile.points.match(data,edcna[edcna[,"N"]==2 & edcna[,"Z-number"]==(-2*i) & edcna[,"O"]==1 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=thresh,cex=cex,col=rampPalette(DBE)[i],pch=pch)
}
}

roygbiv.edc<-function(data,thresh,cex=1,pch=1){
versatile.points.match(data,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-4) & edcna[,"O"]==2 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=thresh,cex=cex,col="red",pch=pch)
versatile.points.match(data,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-6) & edcna[,"O"]==2 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=thresh,cex=cex,col="orange",pch=pch)
versatile.points.match(data,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-8) & edcna[,"O"]==2 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=thresh,cex=cex,col="yellow",pch=pch)
versatile.points.match(data,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-10) & edcna[,"O"]==2 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=thresh,cex=cex,col="green",pch=pch)
versatile.points.match(data,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-12) & edcna[,"O"]==2 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=thresh,cex=cex,col="blue",pch=pch)
versatile.points.match(data,edcna[edcna[,"N"]==3 & edcna[,"Z-number"]==(-14) & edcna[,"O"]==2 & edcna[,"Charge"]==1 & edcna[,"S"]==0,],thresh=thresh,cex=cex,col="purple",pch=pch)
}


image(ospwNaM[1,350:850],ospwNaM[100:1950,2],log(t(ospwNaM[100:1950,350:850])),col=rampPalette(1000))

versatile.points.move<-function(data,move,stretch,mzvec,thresh,cex,col,ylim=NULL,xlim=NULL,xlab="",ylab="",pch=1){
  points(stretch*(data[round(data[,2],0) %in% mzvec,][which(data[round(data[,2],0) %in% mzvec,]>thresh,arr.in=T)[,1],2]+move)~data[1,][which(data[round(data[,2],0) %in% mzvec,]>thresh,arr.in=T)[,2]],cex=cex,col=col,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,pch=pch)
}


#Function for plotting, as a line, a batch of mass channels in a CE-ESI-MS dataset based on a formula subset of the result of 
#nagenmz().
plot.batch<-function(data,nagenmz,xlim=xlim,ylim=ylim,col){
plot(data[nagenmz[1]-48,],type="l",col=rampPalette(length(nagenmz))[1],xlim=xlim,ylim=ylim)
for(i in 2:length(nagenmz)){
points(data[nagenmz[i]-48,],type="l",col=rampPalette(length(nagenmz))[i])
}
}


