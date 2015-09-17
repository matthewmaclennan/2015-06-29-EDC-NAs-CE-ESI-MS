#######################################
dmsoENNaMrcC<-apply(dmsoENNaM,2,function(x) (x-mean(x))/sd(x))
dmsoENNaMrcR<-t(apply(dmsoENNaM,1,function(x) (x-mean(x))/sd(x)))
dmsoENNaMrcRC<-apply(dmsoENNaMrcR,2,function(x) (x-mean(x))/sd(x))
dmsoENNaMrcRC[,1]<-dmsoENNaM[,1]
dmsoENNaMrcRC[,2]<-dmsoENNaM[,2]
dmsoENNaMrcRC[1,]<-dmsoENNaM[1,]
dmsoENNaMrcCR<-t(apply(dmsoENNaMrcC,1,function(x) (x-mean(x))/sd(x)))
dmsoENNaMrcCR[,1]<-dmsoENNaM[,1]
dmsoENNaMrcCR[,2]<-dmsoENNaM[,2]
dmsoENNaMrcCR[1,]<-dmsoENNaM[1,]

rampPalette<-colorRampPalette(c("white","black"))
#Row-wise rounded and centred data
image(dmsoENNaMrcR[1,550:850],dmsoENNaMrcR[2:800,2],sqrt(t(dmsoENNaMrcR[2:800,550:850])),col=rampPalette(1000))
#points cutoff over top
versatile.points(cbind(dmsoENNaMrcR[-1,-1],sqrt(dmsoENNaMrcR[-1,-c(1,2)])),dmsoENNaMrcR[-1,-c(1,2)],dmsoENNaMrcR[1,-c(1,2)],cex=0.3,colour="blue",thresh=.8)
#save points
bEN<-thepoints(cbind(dmsoENNaMrcR[-1,-1],sqrt(dmsoENNaMrcR[-1,-c(1,2)])),dmsoENNaMrcR[-1,-c(1,2)],dmsoENNaMrcR[1,-c(1,2)],cex=0.3,colour="blue",thresh=.8)
#use above lister to convert into list mz/rt pairs
#Put the bob2 or "b" mz/rt matrix into a list
listEN<-list()
for(i in 1:length(unique(unique(bEN[,1])))){
listEN[[i]]<-bEN[which(bEN[,1]==unique(bEN[,1])[i]),]
}
#Plot points according to formula match from nagenmz

#grab mz-rt pairs between values, shutting off the 1-D points using is.matrix condition. 1-D 'points' are considered 
#numeric and not matrix
zoomEN<-lapply(listEN,function(x) if(is.matrix(x)){x[x[,2]>620 & x[,2]<880,]})
#Get unique values and summed intensity within the window
areaENNa<-cbind(unique(unlist(lapply(zoomEN,function(x) if(is.matrix(x)){x[,1]}))),
	rowSums(dmsoENNaM[floor(unique(unlist(lapply(zoomEN,function(x) if(is.matrix(x)){x[,1]}))))-48,550:850]))

ENNaZCseries<-list()
for(i in 1:7){
ENNaZCseries[[i]]<-cbind(areaENNa[floor(areaENNa[,1]) %in% dmeda[dmeda[,9] %in% floor(areaENNa[,1]) & dmeda[,"Z-number"]==((-2)*i),9],],
dmeda[dmeda[,9] %in% floor(areaENNa[,1]) & dmeda[,"Z-number"]==((-2)*i),"C"])
}
CnumIntEN<-lapply(ENNaZCseries,function(x) cbind(x[,3],x[,2]))
CnumIntFullEN<-mzUnpackBin2(CnumIntEN,c(1:7),mzby=1,tbins=7)
rampPalette<-colorRampPalette(c("red","orange","yellow","green","blue","purple","pink"))
barplot(t(CnumIntFullEN[[1]][-1,-1]),beside=F,col=rampPalette(7),legend=as.character(CnumIntFullEN[[3]]*(-2)),
	names.arg=CnumIntFullEN[[2]][-1]-4,xlab="Carbon Number",ylab="Approximate Total Pea Area")
title(main="Approximate Peak Area of EDC/NHS-derivatized\nAldrich Naphthenic Acid Fraction Compounds in dimethyl sulfoxide\nas a Function of Carbon Number and Z-Number")

ENNaZCseriesEDC<-list()
for(i in 2:7){
ENNaZCseriesEDC[[i]]<-cbind(areaENNa[floor(areaENNa[,1]) %in% edc[edc[,9] %in% floor(areaENNa[,1]) & edc[,"Z-number"]==((-2)*i),9],],
edc[edc[,9] %in% floor(areaENNa[,1]) & edc[,"Z-number"]==((-2)*i),"C"])
}
CnumIntENEDC<-lapply(ENNaZCseriesEDC,function(x) cbind(x[,3],x[,2]))
CnumIntFullENEDC<-mzUnpackBin2(CnumIntENEDC,c(1:7),mzby=1,tbins=7)
barplot(t(CnumIntFullENEDC[[1]][-1,-1]),beside=F,col=rampPalette(6),legend=as.character(CnumIntFullENEDC[[3]][-1]*(-2)),
	names.arg=CnumIntFullENEDC[[2]][-1],xlab="Carbon Number",ylab="Approximate Total Pea Area")
title(main="Approximate Peak Area of EDC/NHS-derivatized\nAldrich Naphthenic Acid Fraction Compounds in dimethyl sulfoxide\nas a Function of Carbon Number and Z-Number")

#############################################

dmsoNaMrcC<-apply(dmsoNaM,2,function(x) (x-mean(x))/sd(x))
dmsoNaMrcC[,1]<-dmsoNaM[,1]
dmsoNaMrcC[,2]<-dmsoNaM[,2]
dmsoNaMrcC[1,]<-dmsoNaM[1,]
dmsoNaMrcR<-t(apply(dmsoNaM,1,function(x) (x-mean(x))/sd(x)))
dmsoNaMrcR[,1]<-dmsoNaM[,1]
dmsoNaMrcR[,2]<-dmsoNaM[,2]
dmsoNaMrcR[1,]<-dmsoNaM[1,]
dmsoNaMrcRC<-apply(dmsoNaMrcR,2,function(x) (x-mean(x))/sd(x))
dmsoNaMrcRC[,1]<-dmsoNaM[,1]
dmsoNaMrcRC[,2]<-dmsoNaM[,2]
dmsoNaMrcRC[1,]<-dmsoNaM[1,]
dmsoNaMrcCR<-t(apply(dmsoNaMrcC,1,function(x) (x-mean(x))/sd(x)))
dmsoNaMrcCR[,1]<-dmsoNaM[,1]
dmsoNaMrcCR[,2]<-dmsoNaM[,2]
dmsoNaMrcCR[1,]<-dmsoNaM[1,]


rampPalette<-colorRampPalette(c("white","black"))
#Row-wise rounded and centred data
image(dmsoNaMrcR[1,550:850],dmsoNaMrcR[2:800,2],sqrt(t(dmsoNaMrcR[2:800,550:850])),col=rampPalette(1000))
#points cutoff over top
versatile.points(dmsoNaMrcR[-1,-c(1,2)],dmsoNaMrcR[-1,-c(1,2)],dmsoNaMrcR[1,-c(1,2)],cex=0.3,colour="blue",thresh=5)
#save points
bdmso<-thepoints(cbind(dmsoNaMrcR[-1,-1],sqrt(dmsoNaMrcR[-1,-c(1,2)])),dmsoNaMrcR[-1,-c(1,2)],dmsoNaMrcR[1,-c(1,2)],cex=0.3,colour="blue",thresh=.8)
#use above lister to convert into list mz/rt pairs
listdmso<-list()
for(i in 1:length(unique(unique(bdmso[,1])))){
listdmso[[i]]<-bdmso[which(bdmso[,1]==unique(bdmso[,1])[i]),]
}

#Plot points according to formula match from nagenmz

#grab mz-rt pairs between values, shutting off the 1-D points using is.matrix condition. 1-D 'points' are considered 
#numeric and not matrix
zoomdmso<-lapply(listdmso,function(x) if(is.matrix(x)){x[x[,2]>620 & x[,2]<880,]})
#Get unique values
areadmsoNa<-cbind(unique(unlist(lapply(zoomdmso,function(x) if(is.matrix(x)){x[,1]}))),
	rowSums(dmsoNaM[floor(unique(unlist(lapply(zoomdmso,function(x) if(is.matrix(x)){x[,1]}))))-48,550:850]))



dmsoNaZCseries<-list()
for(i in 1:7){
dmsoNaZCseries[[i]]<-cbind(areadmsoNa[floor(areadmsoNa[,1]) %in% dmeda[dmeda[,9] %in% floor(areadmsoNa[,1]) & dmeda[,"Z-number"]==((-2)*i),9],],
dmeda[dmeda[,9] %in% floor(areadmsoNa[,1]) & dmeda[,"Z-number"]==((-2)*i),"C"])
}
CnumIntdmso<-lapply(dmsoNaZCseries,function(x) cbind(x[,3],x[,2]))
CnumIntFulldmso<-mzUnpackBin2(CnumIntdmso,c(1:7),mzby=1,tbins=7)
rampPalette<-colorRampPalette(c("red","orange","yellow","green","blue","purple","pink"))
barplot(t(CnumIntFulldmso[[1]][-1,-1]),beside=F,col=rampPalette(7),legend=as.character(CnumIntFulldmso[[3]]*(-2)),
	names.arg=CnumIntFulldmso[[2]][-1]-4,xlab="Carbon Number",ylab="Approximate Total Pea Area")
title(main="Approximate Peak Area of EDC-derivatized\nAldrich Naphthenic Acid Fraction Compounds in dimethyl sulfoxide\nas a Function of Carbon Number and Z-Number")

#Insert edc list instead of dmeda list
##
##
##
dmsoNaZCseriesEDC<-list()
for(i in 1:7){
dmsoNaZCseriesEDC[[i]]<-cbind(areadmsoNa[floor(areadmsoNa[,1]) %in% dmeda[dmeda[,9] %in% floor(areadmsoNa[,1]) & dmeda[,"Z-number"]==((-2)*i),9],],
dmeda[dmeda[,9] %in% floor(areadmsoNa[,1]) & dmeda[,"Z-number"]==((-2)*i),"C"])
}
CnumIntdmsoEDC<-lapply(dmsoNaZCseriesEDC,function(x) cbind(x[,3],x[,2]))
CnumIntFulldmsoEDC<-mzUnpackBin2(CnumIntdmsoEDC,c(1:7),mzby=1,tbins=7)
rampPalette<-colorRampPalette(c("red","orange","yellow","green","blue","purple","pink"))
barplot(t(CnumIntFulldmsoEDC[[1]][-1,-1]),beside=F,col=rampPalette(7),legend=as.character(CnumIntFulldmsoEDC[[3]]*(-2)),
	names.arg=CnumIntFulldmsoEDC[[2]][-1]-4,xlab="Carbon Number",ylab="Approximate Total Pea Area")
title(main="Approximate Peak Area of EDC-derivatized\nAldrich Naphthenic Acid Fraction Compounds in dimethyl sulfoxide\nas a Function of Carbon Number and Z-Number")



#################################################

dcmNaMrcR<-t(apply(dcmNaM,1,function(x) (x-mean(x))/sd(x)))
dcmNaMrcR[,1]<-dcmNaM[,1]
dcmNaMrcR[,2]<-dcmNaM[,2]
dcmNaMrcR[1,]<-dcmNaM[1,]
dcmNaMrcC<-apply(dcmNaM,2,function(x) (x-mean(x))/sd(x))
dcmNaMrcC[,1]<-dcmNaM[,1]
dcmNaMrcC[,2]<-dcmNaM[,2]
dcmNaMrcC[1,]<-dcmNaM[1,]
dcmNaMrcRC<-apply(dcmNaMrcR,2,function(x) (x-mean(x))/sd(x))
dcmNaMrcRC[,1]<-dcmNaM[,1]
dcmNaMrcRC[,2]<-dcmNaM[,2]
dcmNaMrcRC[1,]<-dcmNaM[1,]
dcmNaMrcCR<-t(apply(dcmNaMrcC,1,function(x) (x-mean(x))/sd(x)))
dcmNaMrcCR[,1]<-dcmNaM[,1]
dcmNaMrcCR[,2]<-dcmNaM[,2]
dcmNaMrcCR[1,]<-dcmNaM[1,]

image(dcmNaMrcR[1,550:850],dcmNaMrcR[2:800,2],sqrt(t(dcmNaMrcR[2:800,550:850])),col=rampPalette(1000))
#points cutoff over top
versatile.points(cbind(dcmNaMrcR[-1,-1],sqrt(dcmNaMrcR[-1,-c(1,2)])),dcmNaMrcR[-1,-c(1,2)],dcmNaMrcR[1,-c(1,2)],cex=0.3,colour="blue",thresh=0.5)
#save points
bDCM<-thepoints(cbind(dcmNaMrcR[-1,-1],sqrt(dcmNaMrcR[-1,-c(1,2)])),dcmNaMrcR[-1,-c(1,2)],dcmNaMrcR[1,-c(1,2)],cex=0.3,colour="blue",thresh=.5)
#use above lister to convert into list mz/rt pairs
listdcm<-list()
for(i in 1:length(unique(unique(bDCM[,1])))){
listdcm[[i]]<-bDCM[which(bDCM[,1]==unique(bDCM[,1])[i]),]
}

#Plot points according to formula match from nagenmz

#grab mz-rt pairs between values, shutting off the 1-D points using is.matrix condition. 1-D 'points' are considered 
#numeric and not matrix
zoomdcm<-lapply(listdcm,function(x) if(is.matrix(x)){x[x[,2]>620 & x[,2]<880,]})
#Get unique values
areadcmNa<-cbind(unique(unlist(lapply(zoomdcm,function(x) if(is.matrix(x)){x[,1]}))),
	rowSums(dcmNaM[floor(unique(unlist(lapply(zoomdcm,function(x) if(is.matrix(x)){x[,1]}))))-48,550:850]))



dcmNaZCseries<-list()
for(i in 1:7){
dcmNaZCseries[[i]]<-cbind(areadcmNa[floor(areadcmNa[,1]) %in% dmeda[dmeda[,9] %in% floor(areadcmNa[,1]) & dmeda[,"Z-number"]==((-2)*i),9],],
dmeda[dmeda[,9] %in% floor(areadcmNa[,1]) & dmeda[,"Z-number"]==((-2)*i),"C"])
}
CnumIntdcm<-lapply(dcmNaZCseries,function(x) cbind(x[,3],x[,2]))
CnumIntFulldcm<-mzUnpackBin2(CnumIntdcm,c(1:7),mzby=1,tbins=7)
rampPalette<-colorRampPalette(c("red","orange","yellow","green","blue","purple","pink"))
barplot(t(CnumIntFulldcm[[1]][-1,-1]),beside=F,col=rampPalette(7),legend=as.character(CnumIntFulldcm[[3]]*(-2)),
	names.arg=CnumIntFulldcm[[2]][-1]-4,xlab="Carbon Number",ylab="Approximate Total Pea Area")
title(main="Approximate Peak Area of EDC-derivatized\nAldrich Napthenic Acid Fraction Compounds in dichloromethane\nas a Function of Carbon Number and Z-Number")

dcmNaZCseriesEDC<-list()
for(i in 1:7){
dcmNaZCseriesEDC[[i]]<-cbind(areadcmNa[floor(areadcmNa[,1]) %in% dmeda[dmeda[,9] %in% floor(areadcmNa[,1]) & dmeda[,"Z-number"]==((-2)*i),9],],
dmeda[dmeda[,9] %in% floor(areadcmNa[,1]) & dmeda[,"Z-number"]==((-2)*i),"C"])
}
CnumIntdcmEDC<-lapply(dcmNaZCseriesEDC,function(x) cbind(x[,3],x[,2]))
CnumIntFulldcmEDC<-mzUnpackBin2(CnumIntdcmEDC,c(1:7),mzby=1,tbins=7)
rampPalette<-colorRampPalette(c("red","orange","yellow","green","blue","purple","pink"))
barplot(t(CnumIntFulldcmEDC[[1]][-1,-1]),beside=F,col=rampPalette(7),legend=as.character(CnumIntFulldcmEDC[[3]]*(-2)),
	names.arg=CnumIntFulldcmEDC[[2]][-1]-4,xlab="Carbon Number",ylab="Approximate Total Pea Area")
title(main="Approximate Peak Area of EDC-derivatized\nAldrich Napthenic Acid Fraction Compounds in dichloromethane\nas a Function of Carbon Number and Z-Number")

###############################################



###############################################


##################################################

#plot line graphs on top of each other
plot(CnumIntFull[[1]][-1,-1][,1],type="l",col=rampPalette(ncol(CnumIntFull)[1]))
for(i in 1:ncol(CnumIntFull[[1]][-1,-1])){
points(CnumIntFull[[1]][-1,-1][,i],type="l",col=rampPalette(ncol(CnumIntFull[[1]][-1,-1]))[i])
}
