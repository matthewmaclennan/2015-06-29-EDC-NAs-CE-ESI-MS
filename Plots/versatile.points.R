#soc
versatile.points<-function(data,data.window,thresh,ret,cex,xlim=NULL,ylim=NULL,xlab="",ylab="",colour){
  ifelse(colour=="ramp",
    points(data[which(data.window>thresh,arr.in=T)[,1]]~ret[which(data.window>thresh,arr.in=T)[,2]],
    cex=cex,xlab=xlab,ylab=ylab,
    col=rampPalette(length(data[which(data.window>thresh,arr.in=T)]))[rank(data[which(data.window>thresh,arr.in=T)])],
    xlim=xlim, ylim=ylim),
    points(data[which(data.window>thresh,arr.in=T)[,1]]~ret[which(data.window>thresh,arr.in=T)[,2]],
    cex=cex,xlab=xlab,ylab=ylab,
    col=colour,
    xlim=xlim,
    ylim=ylim)
  )
}
#eoc
thepoints<-function(data,data.window,thresh,ret,cex,xlim=NULL,ylim=NULL,xlab="",ylab="",colour){
  pointcoords<-cbind(data[which(data.window>thresh,arr.in=T)[,1]],ret[which(data.window>thresh,arr.in=T)[,2]])
}
