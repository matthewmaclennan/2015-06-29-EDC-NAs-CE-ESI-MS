#Special plotting
#Do not need to add "m/z" in the column index for mzvec
#mzvec is the 9-column matrix result from nagenmz()
versatile.plot.match<-function(data,mzvec,thresh,cex,col,ylim=NULL,xlim=NULL,xlab="",ylab="",pch){
  plot(data[round(data[,2],0) %in% mzvec,][which(data[round(data[,2],0) %in% mzvec,]>thresh,arr.in=T)[,1],2]~data[1,][which(data[round(data[,2],0) %in% mzvec,]>thresh,arr.in=T)[,2]],cex=cex,col=col,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,pch=pch)
}
#Special plotting
versatile.points.match<-function(data,mzvec,thresh,cex,col,ylim=NULL,xlim=NULL,xlab="",ylab="",pch){
  points(data[round(data[,2],0) %in% mzvec,][which(data[round(data[,2],0) %in% mzvec,]>thresh,arr.in=T)[,1],2]~data[1,][which(data[round(data[,2],0) %in% mzvec,]>thresh,arr.in=T)[,2]],cex=cex,col=col,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,pch=pch)
}
