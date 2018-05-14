# Rscript --vanilla THIS_SCRIPT.R  INPUT_DATASET.csv  OUTPUT.RData
# EG.: Rscript --vanilla THIS_SCRIPT.R  norm_cellcycle_384_17.csv  OUTPUT.RData

##


args <- commandArgs(trailingOnly = TRUE)

hh <- paste(unlist(args),collapse=' ')
listoptions <- unlist(strsplit(hh,'--'))[-1]
options.args <- sapply(listoptions,function(x){
         unlist(strsplit(x, ' '))[-1]
        })
options.names <- sapply(listoptions,function(x){
  option <-  unlist(strsplit(x, ' '))[1]
})
names(options.args) <- unlist(options.names)
print(options.args)

####### libraries - R packages - local code 
library(plyr);library(NbClust);library(Mfuzz);library(Biobase);library(mclust);library(longclust);library(TSclust)
## modified R code
##fix(NbClust)
source("NbClust.modified.R")
# modify solve(w * statement by using a smaller tolerance, like solve(..., tol = 1e-17)



TMP_CluMethRes=list()


A<-"Cho" # Cho dataset affymetrix

########################################################## default input Cho dataset affymetrix ###########################################################
df = read.table(args[1], header=TRUE)
rownames(df)<-df$"Main"
vec.groundTruth<-df$"Gp"
#
df<-df[,c(-1,-2)]
########################################################## @@@@@@@@@@@@@@@@@@@@@@@@ ###########################################################
# raw dataset needs to be transformed in a matrix or an expression set or in a dataframe according to the specific requirements of each single clustering algorithm
df.mat<-as.matrix(df)


cat("generate mfuzz results","\n")
B<-"Mfuzz"
source("runMfuzz.R")
# df.mat is the input dataset
# 5 is the optimal number of clusters
# TMP_CluMethRes[[A]] is the list R object storing the memberships obtained using as input dataset the one associated to generic string A (raw file is loaded in R by args[1])

TMP_CluMethRes[[A]][[B]]<-RunMfuzz(A,B,df.mat,5)
save(TMP_CluMethRes,file=args[2]) # 

B<-"kmInMfuzz"
source("runKmeansInMfuzz.R")
# df.mat is the input dataset
# 5 is the optimal number of clusters
# TMP_CluMethRes[[A]] is the list R object storing the memberships obtained using as input dataset the one associated to generic string A (raw file is loaded in R by args[1])
TMP_CluMethRes[[A]][[B]]<-RunKmeansInMfuzz(A,B,df.mat,5)
save(TMP_CluMethRes,file=args[2]) #
##

B<-"nbclust1"
source("RunNBCLUST1.R")
# df.mat is the input dataset
# 5 is the optimal number of clusters
# TMP_CluMethRes[[A]] is the list R object storing the memberships obtained using as input dataset the one associated to generic string A (raw file is loaded in R by args[1])
TMP_CluMethRes[[A]][[B]]<-RunNBCLUST1(A,B,df.mat,5)
save(TMP_CluMethRes,file=args[2]) #

##

B<-"nbclust3"
source("RunNBCLUST3.R")
# df.mat is the input dataset
# 5 is the optimal number of clusters
# TMP_CluMethRes[[A]] is the list R object storing the memberships obtained using as input dataset the one associated to generic string A (raw file is loaded in R by args[1])
TMP_CluMethRes[[A]][[B]]<-RunNBCLUST3(A,B,df.mat,5)
save(TMP_CluMethRes,file=args[2]) #

##
B<-"tscCRTpam"
source("RunTScluCORTpam.R")
# df.mat is the input dataset
# 5 is the optimal number of clusters
# TMP_CluMethRes[[A]] is the list R object storing the memberships obtained using as input dataset the one associated to generic string A (raw file is loaded in R by args[1])
TMP_CluMethRes[[A]][[B]]<-RunTScluCORTpam(A,B,df.mat,5)
save(TMP_CluMethRes,file=args[2]) #
##

##

B<-"tscCRThc"
source("RunTScluCORThc.R")
# df.mat is the input dataset
# 5 is the optimal number of clusters
# TMP_CluMethRes[[A]] is the list R object storing the memberships obtained using as input dataset the one associated to generic string A (raw file is loaded in R by args[1])
TMP_CluMethRes[[A]][[B]]<-RunTScluCORThc(A,B,df.mat,5)
save(TMP_CluMethRes,file=args[2]) #
##
##

B<-"loclu"
source("RunLONGCLUST.R")
# df.mat is the input dataset
# 5 is the optimal number of clusters
# TMP_CluMethRes[[A]] is the list R object storing the memberships obtained using as input dataset the one associated to generic string A (raw file is loaded in R by args[1])
TMP_CluMethRes[[A]][[B]]<-RunLONGLUST(A,B,df.mat,5)
save(TMP_CluMethRes,file=args[2]) #

##
library(corrplot)
library(mclust)

#finalRes=list()
clustSchemes1<-c("Mfuzz","kmInMfuzz","nbclust1","nbclust3","tscluCORThc","tscCRTpam","loclu")
#, "SSClust","nbclust1","nbclust3","tscluCORThc","tscCRTpam","loclu")
clustSchemes2<-c("Mfuzz","kmInMfuzz","nbclust1","nbclust3","tscluCORThc","tscCRTpam","loclu")
#, "SSClust","nbclust1","nbclust3","tscluCORThc","tscCRTpam","loclu")
#"Mfuzz","kmInMfuzz", "SSClust","nbclust1","nbclust3","tscluCORThc","tscCRTpam","loclu"
#"Mfuzz","kmInMfuzz", "SSClust","nbclust1","nbclust3","tscluCORThc","tscCRTpam" ,"loclu"

mAt <- matrix(0, nrow = length(clustSchemes1), ncol =length(clustSchemes2) , byrow = TRUE, dimnames = list(clustSchemes1,clustSchemes2))
temp<-mAt
for(i in 1:length(clustSchemes1)){
for(j in 1:length(clustSchemes2)){
cat(as.character(clustSchemes1[i]),as.character(clustSchemes1[j]),"\n")
temp[rownames(temp)==as.character(clustSchemes1[i]),colnames(temp)==as.character(clustSchemes1[j])] <-adjustedRandIndex(unlist(TMP_CluMethRes[[A]][i]),unlist(TMP_CluMethRes[[A]][j]))#
}
}
temp
#accumulatore<-accumulatore+temp

col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))
#corrplot.mixed(aveMatrixBothGenderInsulin,col=col4(150))
corrplot.mixed(temp,col=col4(150))


                Mfuzz  kmInMfuzz  nbclust1  nbclust3 tscluCORThc  tscCRTpam
Mfuzz       1.0000000 0.61360738 0.7480448 0.6466053  0.58281924 0.47231795
kmInMfuzz   0.6136074 1.00000000 0.8622004 0.7968010  0.69704746 0.65026963
nbclust1    0.7480448 0.86220038 1.0000000 0.8484699  0.76185719 0.65905948
nbclust3    0.6466053 0.79680104 0.8484699 1.0000000  0.75391847 0.67284049
tscluCORThc 0.5828192 0.69704746 0.7618572 0.7539185  1.00000000 0.70750488
tscCRTpam   0.4723179 0.65026963 0.6590595 0.6728405  0.70750488 1.00000000
loclu       0.1690126 0.08827841 0.1195729 0.1250040  0.08032604 0.05910503
                 loclu
Mfuzz       0.16901261
kmInMfuzz   0.08827841
nbclust1    0.11957285
nbclust3    0.12500401
tscluCORThc 0.08032604
tscCRTpam   0.05910503
loclu       1.00000000














