mutcount<-function(A){

con<-matrix(nrow=c(406), ncol=c(1))

for(j in 1:406)
{

if(A[j,4] > 1000){
con[j,1]<-c(1)
}

if(A[j,5] > 1000){
con[j,1]<-c(2)
}
if(A[j,6] > 1000){
con[j,1]<-c(3)
}
if(A[j,7] > 1000){
con[j,1]<-c(4)
}
}

mut<-matrix(c(0),nrow=c(406), ncol=c(12))

for(i in 1:406){

if(con[i,1]==1){
mut[i,1]<-A[i,7]
mut[i,2]<-A[i,6]
mut[i,3]<-A[i,5]
}
if(con[i,1]==2){
mut[i,10]<-A[i,4]
mut[i,11]<-A[i,7]
mut[i,12]<-A[i,6]
}
if(con[i,1]==3){
mut[i,7]<-A[i,4]
mut[i,8]<-A[i,7]
mut[i,9]<-A[i,5]
}
if(con[i,1]==4){
mut[i,4]<-A[i,4]
mut[i,5]<-A[i,6]
mut[i,6]<-A[i,5]
}
}

mutnam<-c("A2C","A2G","A2T","C2A","C2G","C2T","G2A","G2C","G2T","T2A","T2C","T2G")

mutations<-matrix(c(0), nrow=c(1), ncol=c(12))

colnames(mutations)<-mutnam

for(i in 1:12){
mutations[1,i]<-sum(mut[,i])
}
return(mutations)
}

