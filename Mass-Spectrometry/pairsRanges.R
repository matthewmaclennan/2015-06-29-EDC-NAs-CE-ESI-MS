pairsRanges<-function(matrix,matrix.window,mzcol,retrow,thresh){
#pairs is the migration time-m/z pairs two-column matrix. The vector supairs is the sorted unique m/z values which registered
#in pairs. The empty pairsranges is opened for the for loop.
#The matrix is the data matrix itself. The m/z vector must be the second column and migration time must be the first row.
  pairs<-cbind(matrix[which(matrix.window>thresh,arr.in=T)[,1],mzcol],matrix[retrow,which(matrix.window>thresh,arr.in=T)[,2]])
  supairs<-sort(unique(pairs[,1]))
  pairsranges<-c(0,0,0)
#pairsranges is the vector of migration (retention) time values for a given unique m/z derived from supairs (or pairs).
#col 1 contains the unique m/z hits; col 2 contains the lowest migration time value; 
#col 3 contains the highest migraton time value.
  for(i in 1:length(supairs)){
    rangevec<-c(supairs[i],range(pairs[pairs[,1]==supairs[i],2]))
    pairsranges<-rbind(pairsranges,rangevec)
  }
  pairsranges<-pairsranges[-1,]
  pairsranges<-cbind(pairsranges,pairsranges[,3]-pairsranges[,2])
#output is list object
  list<-list(pairsranges,pairs)
}
