
RunMfuzz = function(aString1,aString2,aMatrix,OptNumCl){

### fuzzyfier estimated by mestimate function (inside Mfuzz r package)

A<-aString1
B<-aString2
eset=list()

myVecOfFuzzifiers=list()
mfuzzClRes=list()
Mfuzz=vector()

#TMP_CluMethRes=list()

#            ******* generate Mfuzz vector collector *******
eset[[A]]<-new("ExpressionSet",exprs=aMatrix) #

fuzz1 <- mestimate(eset[[A]])
myVecOfFuzzifiers[[A]]<-fuzz1


mfuzzClRes[[A]] <- mfuzz(eset[[A]],c=OptNumCl,m=fuzz1)
Mfuzz<-mfuzzClRes[[A]]$"cluster"
TMP_CluMethRes[[A]][[B]]<-Mfuzz # $"Mfuzz"

  
  # mfuzz.plot(eset[[A]],cl=mfuzzClRes[[A]] ,mfrow=c(2,2),time.labels=c("Age 11","Age 12","Age 13","Age 14","Age 15","Age 16")) #pl
  # end of generate Mfuzz Res
  

return(TMP_CluMethRes[[A]][[B]])



}
