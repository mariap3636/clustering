
RunTScluCORTpam = function(aString1,aString2,aMatrix,OptNumCl){

### pam strategy

A<-aString1
B<-aString2

#using diss mat
#diss mat

IP.dis <- diss(aMatrix, "CORT", k=OptNumCl, deltamethod="DTW")


IP.pamclus <- pam(IP.dis, k=OptNumCl)$clustering

tscluCORTpam<-IP.pamclus  
TMP_CluMethRes[[A]][[B]]<-tscluCORTpam

return(TMP_CluMethRes[[A]][[B]])



}