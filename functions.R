plot2d <- function(X,Y){
  xl = range(X[,1],Y[,1])
  yl = range(X[,2],Y[,2])
  plot(X, xlim=xl, ylim=yl, pch="X", col="red", xlab="l=1", ylab="l=2")
  par(new=T)
  plot(Y, xlim=xl, ylim=yl, pch="Y", col="blue", xlab=" ", ylab=" ",
       xaxt="n", yaxt="n")
  legend("topright", legend=c("X","Y"), col=c("red","blue"), pch=c("X","Y"))
}

I.KDRE <- function(X,Y,Z,h=0.1){
  f_kde = as.vector(kde(x=X, eval.points=Z, h=h)$estimate)
  g_kde = as.vector(kde(x=Y, eval.points=Z, h=h)$estimate)
  kdre = f_kde/g_kde
  return(kdre)  
}

D.KDRE <- function(X,Y,Z,h=0.5,eps=0.5){

  nZ = nrow(Z)
  HZ1 = sapply(1:nZ, function(j) mean(Y[,1]<=Z[j,1]))
  HZ2 = sapply(1:nZ, function(j){
    w = exp(-((Z[j,1] - Y[,1])/eps)^2)/sqrt(pi)
    reg.w = w/sum(w)
    sum(reg.w * (Y[,2]<=Z[j,2]))
  })
  HZ = cbind(HZ1,HZ2)

  nX = nrow(X)  
  HX1 = sapply(1:nX, function(j) mean(Y[,1]<=X[j,1]))
  HX2 = sapply(1:nX, function(j){
    w = exp(-((X[j,1] - Y[,1])/eps)^2)/sqrt(pi)
    reg.w = w/sum(w)
    sum(reg.w * (Y[,2]<=X[j,2]))
  })
  HX = cbind(HX1,HX2)
  
  kdre = as.vector(kde(x=HX, eval.points=HZ, h=h)$estimate)
  return(kdre)
}
