
RunKmeansInMfuzz = function(aString1,aString2,aMatrix,OptNumCl){
A<-aString1
B<-aString2
eset=list()
eset[[A]]<-new("ExpressionSet",exprs=aMatrix)

intKmeansRes=list()
kmInMfuzz=vector()

  intKmeansRes[[A]] <- kmeans2(eset[[A]],k=OptNumCl)
  kmInMfuzz<-intKmeansRes[[A]]$"cluster"
  TMP_CluMethRes[[A]][[B]]<-kmInMfuzz #$"kmInMfuzz"
  

return(TMP_CluMethRes[[A]][[B]])



}
