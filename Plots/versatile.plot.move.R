#for translating cutoff mass spectrometry data in the m/z direction and stretching in the m/z direction
versatile.points.move<-function(data,movemz,stretchmz,movemt,stretchmt,mzvec,thresh,cex,col,ylim=NULL,xlim=NULL,xlab="",ylab="",pch=""){
  points(stretchmz*(data[round(data[,2],0) %in% mzvec,][which(data[round(data[,2],0) %in% mzvec,]>thresh,arr.in=T)[,1],2]+movemz)~
  stretchmt*(data[1,][which(data[round(data[,2],0) %in% mzvec,]>thresh,arr.in=T)[,2]]+movemt),
  cex=cex,col=col,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,pch=pch)
}
