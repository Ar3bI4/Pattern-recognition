#setwd("D:/ProgrammingR/RO_LR_5_perc")
XYdata = matrix(data=unlist(read.table( file = "XY.txt",header=TRUE), recursive = TRUE, use.names = FALSE), nrow=3, 
                 byrow=TRUE,dimnames=list(c("X","Y","Clast")))
XYdata = rbind(XYdata, c(1:length(XYdata[1,]))) #добавление строки с номерерами точек (в варианте от 1 до 21)

N = 100  #Количество иттераций для остановки
Niterations = c(0,0,0)
Wfunction = function(Claster1, Claster2)
{
  flag <- TRUE;
  W = c(-1,2,2)                                                                                         
  Wnew = W 
  Wnew_num = 0; 
  
  #Первые координаты образов из кластеров 1 и 2
  X = c(XYdata[,XYdata[3,] == Claster1][1,],
            c(-1) %*% XYdata[,XYdata[3,] == Claster2][1,])   
  #Вторые координаты образов из кластеров 1 и 2
  y = c(XYdata[,XYdata[3,] == Claster1][2,],
        c(-1) %*% XYdata[,XYdata[3,] == Claster2][2,]) 
  
  #Индексы координат
  index_xy = c(XYdata[,XYdata[3,] == Claster1][4,],
                XYdata[,XYdata[3,] == Claster2][4,])
  X = rbind(X,y)
  X = rbind(X, index_xy)
 
  x_last = c(1)
  for(i in 1:length(X[1,]))
  {      
    if(XYdata[3,][X[3,][i]] == Claster2)
    { x_last[i] = -1 } 
    else 
    { x_last[i] = 1 } 
  }
  X = rbind(X[1,], X[2,], x_last) 
  k = 1
  while(flag) 
  {
    C = 1
    if(k %% length(X[1,]) != 0)
    { xk = X[,k%%length(X[1,])] } 
    else 
    { xk = X[,length(X[1,])] }
    if((W %*% xk) <= 0)
    {
      C = ceiling(as.numeric(-((W %*% xk)/(xk %*% xk)))) #корректирующее приращение
      if(C == 0 || C == as.numeric(-((W %*% xk)/(xk %*% xk)))) {C = С + 1} 
      W = W + (xk*C) 
    }
    
    if(isTRUE(all.equal(W,Wnew))) 
    { Wnew_num = Wnew_num + 1 } 
    else { Wnew = W; Wnew_num = 0 }
    if(Wnew_num >= N)
    { flag = FALSE }
    k = k+1
  }
  Niterations[Claster1] <<- k
  X = cbind(X,W)
  #print(X)
  #print(Niterations[Claster1])
  return(X)
}

W = c()
for(l in 1:length(unique(XYdata[3,]))) 
{    
  if(l == length(unique(XYdata[3,]))) #1 и 2; 2 и 3;
  { W = cbind(W,Wfunction(l,1)[,"W"])} 
  else # 3 и 1
  { W = cbind(W,Wfunction(l,l+1)[,"W"])}
  #print(W)
}

plot(XYdata[1,], XYdata[2,], type = "n", xlab = "x", ylab = "y", xlim = c(-6,2), ylim = c(-6,2))
grid()

points(XYdata[1,which(XYdata[3,]==1)],XYdata[2,which(XYdata[3,]==1)], 
       col = 4, type = "p", pch = 16, cex = 1)

points(XYdata[1,which(XYdata[3,]==2)],XYdata[2,which(XYdata[3,]==2)], 
       col = 7, type = "p", pch = 16, cex = 1)

points(XYdata[1,which(XYdata[3,]==3)],XYdata[2,which(XYdata[3,]==3)], 
       col = 2, type = "p", pch = 16, cex = 1)

#for(i in 1:length(unique(XYdata[3,])))
#{
#  x1 = 6.0
#  seg1 = c(99,99); seg2 = c(99,99) #координаты концов сегментов
#  if(i == 1)
#  { LineColor = "#CD3333" }
#  if(i == 2)
#  { LineColor = "#00F5FF" }
#  if(i == 3)
#  { LineColor = "#2E8B57" }
#  
#  while(x1 > -2)
#  {
#    y1 = -6.0
#    while(y1 <= 2)
#    {
#      z = c(x1, y1, 1)
#      
#      if(abs(z %*% W[,i]) <= 0.05)
#      {
#        if(seg1[1] == 99)
#        {
#          seg1[1] = x1
#          seg1[2] = y1
#        }
#        seg2[1] = x1
#        seg2[2] = y1
#      }
#      y1 = y1 + 0.01
#    }
#    x1 = x1 - 0.01
#  }
#  points(c(seg1[1],seg2[1]),c(seg1[2],seg2[2]), col = LineColor, type = "o")
#  #print(seg1); print(seg2)
#}

ClassGranica = function(x1,x2)
{
  z = c(x1,x2,1)
  zSearch = c(0,0,0)
  for(i in 1: length(unique(XYdata[3,])))
  {
    if(i != length(unique(XYdata[3,]))) #i != 3
    {
      if(z %*% W[,i] < 0) 
      { zSearch[i+1] = zSearch[i+1] + 1 } 
      else
      { zSearch[i] = zSearch[i] + 1 }
    } 
    else #i == 3
    {
      if(z %*% W[,i] < 0)      
      { zSearch[1] = zSearch[1] + 1 } 
      else                            
      { zSearch[i] = zSearch[i] + 1 }
    }
  }
  for(i in 1:length(unique(XYdata[3,])))
  {
    if(zSearch[i] >= 2)
    {
      if(i == 1)
      { points(x = z[1], y = z[2], col = 4, pch= 16, cex = 0.1) }
      if(i == 2)
      { points(x = z[1], y = z[2], col = 7, pch = 16, cex = 0.1) }
      if(i == 3)
      { points(x = z[1], y = z[2], col = 2, pch = 16, cex = 0.1) }
      break
    }
    if(i == length(unique(XYdata[3,])))
    {
      points(x = z[1], y = z[2], col = 1, pch = 16, cex = 0.1)
    }
  }
}

for(i in 1:length(unique(XYdata[3,])))
{
x1 = 3.0
  while(x1 > -7)
  {
    y1 = -7.0
    while(y1 <= 3)
    {#
      ClassGranica(x1,y1)
      y1 = y1 + 0.1
    }
    x1 = x1 - 0.1
  }
}
# e = 0.01
# ras = 0.4
# x = -7
# y = -7
# z = c(x,y,1)
# eps = 0.1
# while(x < 3){
#  while(y < 3){
#   if (z%*%W[,1] < 0+eps && z%*%W[,1] > 0-eps) #&& !(z%*%W[,2] < 0 && z%*%W[,3] > 0))
#   {points(x,y,col = 1,pch = 16, cex =ras)}
#   if (z%*%W[,2] < 0+eps && z%*%W[,2] > 0-eps) #&& !(z%*%W[,1] > 0 && z%*%W[,3] > 0))
#   {points(x,y,col = 1,pch = 16, cex =ras)}
#   if (z%*%W[,3] < 0+eps && z%*%W[,3] > 0-eps) #&& !(z%*%W[,1] < 0 && z%*%W[,2] > 0))
#   {points(x,y,col = 1,pch = 16, cex =ras)}
#   if (!(z%*%W[,1] > 0 && z%*%W[,3] > 0) && !(z%*%W[,1] < 0 && z%*%W[,2] > 0) && !(z%*%W[,2] < 0 && z%*%W[,3] < 0))
#   {points(x,y,col = 1,pch = 16, cex =ras)}
#   y = y + 3*e
#   }
#   x = x + 3*e
#   y = -7
# }
# e = 0.01
# x = -7
# y = -7
# eps = 0.1
# ras = 0.4
# while(x < 4){
#   while(y < 4){
#     if (W[1,]%*%c(x,y,1) < 0+eps && W[1,]%*%c(x,y,1) > 0-eps && !(W[2,]%*%c(x,y,1) < 0 && W[3,]%*%c(x,y,1) < 0))
#     {points(x,y,col = 1,pch =16, cex =ras)}
#     if (W[2,]%*%c(x,y,1) < 0+eps && W[2,]%*%c(x,y,1) > 0-eps && !(W[1,]%*%c(x,y,1) > 0 && W[3,]%*%c(x,y,1) > 0))
#     {points(x,y,col = 1,pch =16, cex =ras)}
#     if (W[3,]%*%c(x,y,1) < 0+eps && W[3,]%*%c(x,y,1) > 0-eps && !(W[1,]%*%c(x,y,1) < 0 && W[2,]%*%c(x,y,1) > 0))
#     {points(x,y,col = 1,pch =16, cex =ras)}
#     if (!(W[1,]%*%c(x,y,1) > 0 && W[3,]%*%c(x,y,1) > 0) &&
#         !(W[1,]%*%c(x,y,1) < 0 && W[2,]%*%c(x,y,1) > 0) &&
#         !(W[2,]%*%c(x,y,1) < 0 && W[3,]%*%c(x,y,1) < 0))
#     {points(x,y,col = 1,pch =16, cex =ras)}
#     y = y + 3*e
#   }
#   x = x + 3*e
#   y = -7
# }
#}
PointClassSearch = function(x1,x2)
{
  z = c(x1,x2,1)
  zSearch = c(0,0,0)
  for(i in 1: length(unique(XYdata[3,])))
  {
    if(i != length(unique(XYdata[3,]))) #i != 3
    {
      if(z %*% W[,i] < 0) #di(z) < 0
      { zSearch[i+1] = zSearch[i+1] + 1 } 
      else #di(z) >= 0
      { zSearch[i] = zSearch[i] + 1 }
    } 
    else #i == 3
    {
      if(z %*% W[,i] < 0)      #d3(z) < 0
      { zSearch[1] = zSearch[1] + 1 } 
      else                            #d3(z) >= 0
      { zSearch[i] = zSearch[i] + 1 }
    }
  }
  
  #print(W)
  for(i in 1:length(unique(XYdata[3,])))
  {
    if(zSearch[i] >= 2)
    {
      if(i == 1)
      { points(x = z[1], y = z[2], col = 4, pch = 16, cex = 1.2) }
      if(i == 2)
      { points(x = z[1], y = z[2], col = 7, pch = 16, cex = 1.2) }
      if(i == 3)
      { points(x = z[1], y = z[2], col = 2, pch = 16, cex = 1.2) }
      
      # print(sprintf("[%f;%f] принадлежит классу %i",z[1],z[2],i))
      break
    }
    if(i == length(unique(XYdata[3,]))){
      # print(sprintf("[%f;%f] принадлежит области непринятия решений",z[1],z[2]))
      points(x = z[1], y = z[2], col = "black", pch = 16, cex = 1.2)
    }
  }
}
#legend("bottomright", legend = c("1-2", "2-3", "3-1"),
#       lwd = 4, col = c("#CD3333", "#00F5FF", "#2E8B57"))

#legend("topright", legend = c("1", "2", "3"), lty = 3,
       #lwd = 4, col = c("#FF00FF", "#EEA9B8", "#EE7600"))
PointClassSearch(-2,-2)
print(Niterations)