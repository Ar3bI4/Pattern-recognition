x = read.table("clast_points.txt",header = TRUE) 
X = matrix(data=unlist(x, recursive = TRUE, use.names = FALSE), nrow=3, byrow=TRUE, dimnames=list(c("x","y","Clast"))) 
N = matrix(nrow=1, ncol = length(unique(X[3,])))
for (i in 1:length(unique(X[3,])))
{
  N[1,i] = ncol(X[,which(X[3,]==i)])
}
M = matrix(nrow=2, ncol=length(N[1,]))
for (i in 1:length(N[1,]))
{
  M[1,i]=sum(X[1,which(X[3,]==i)]/N[1,i])
  M[2,i]=sum(X[2,which(X[3,]==i)]/N[1,i])
}
XY=matrix(nrow=2, ncol=length(X[1,]))  
XY[1,]=X[1,]
XY[2,]=X[2,]
S=matrix(ncol=2)
# print(((XY[,which(X[3,]==1)])))
# print(t(XY[,which(X[3,]==1)]))
# print(XY[,which(X[3,]==i)]%*%t(XY[,which(X[3,]==i)]))
for (i in 1:length(N[1,]))
{
  S=rbind(S,(XY[,which(X[3,]==i)]%*%t(XY[,which(X[3,]==i)]))/N[,i])
  #print (S)
}
S=S[-1,]
#print (S)
j=1
C=matrix(ncol=2)
#print(S[c(j,j+1),])
for (i in 1:length(N[1,]) )
{
  C=rbind(C,S[c(j,j+1),]-M[,i]%*%t(M[,i]))
  j=j+2
}
#print(C)
#print(length(N[1,]))
C=C[-1,]
j=1
z=c(-4,-4)
d=matrix(nrow=1, ncol=length(N[1,]))
for (i in 1:length(N[1,]))
{
  d[,i]=log(N[i]/length(X[1,]))-0.5*log(det(C[c(j,j+1),]))-0.5*t((z-M[,i]))%*%solve(C[c(j,j+1),])%*%(z-M[,i])
  j=j+2
}
#print(d)
kl=which(d==max(d))
plot(X[1,], X[2,], xlim = c(-6,2), ylim = c(-6,2))
j = 5 
klcol=c()
for (i in 1:length(N[1,]))
{
  points(X[1,which(X[3,]==i)],X[2,which(X[3,]==i)], col=j, pch = 16)
  klcol=c(klcol,j)
  j=j+1
}
#print(X)
points(z[1],z[2],col=klcol[kl], pch = 10)
#print(length(X[1,]))

a = -6.5
while((a >=-6.5) && (a < 2.5)){
  b = -6.5
  while((b >= -6.5) && (b < 2.5)){
    j=1
    z = c(a, b)
    d=matrix(nrow=1, ncol=length(N[1,]))
    for (i in 1:length(N[1,]))
    {
      d[1,i]=log(N[i]/length(X[1,]))-0.5*log(det(C[c(j,j+1),]))-0.5*t((z-M[,i]))%*%solve(C[c(j,j+1),])%*%(z-M[,i])
      j=j+2
    }
    if(abs(d[1,1] - d[1,2]) <= 0.8){
      if((d[1,3] < d[1,2]) && (d[1,3] < d[1,1])){
        points(z[1],z[2], col = "black", cex = 0.5, pch = 20)
      }
    }
    if(abs(d[1,1] - d[1,3]) <= 0.8){
      if((d[1,2] < d[1,1]) && (d[1,2] < d[1,3])){
        points(z[1],z[2], col = "black", cex = 0.5, pch = 20)
      }
    }
    if(abs(d[1,3] - d[1,2]) <= 0.8){
      if((d[1,1] < d[1,3]) && (d[1,1] < d[1,2])){
        points(z[1],z[2], col = "black", cex = 0.5, pch = 20)
      }
    }
    b = b + 0.04
  } 
  a = a + 0.04
}
grid(lwd = 1)