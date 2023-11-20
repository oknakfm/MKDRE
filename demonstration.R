require("ks","mvtnorm","rgl") 
source("functions.R")

set.seed(123)

## =========================================
## Experimental settings
par = list(
  nX = 10**4, nY = 10**4,
  muX = c(0,-0.5), muY = c(0,0),
  VX = matrix(c(0.3,0.1,0.1,0.3),2,2),
  VY = matrix(c(0.5,0.1,0.1,0.5),2,2))
## =========================================

n.prec = 15

## true ratio
x = y = seq(-1.5,1.5,length.out=n.prec)
Z = expand.grid(x,y)
f = dmvnorm(Z, mean=par$muX, sigma=par$VX)
g = dmvnorm(Z, mean=par$muY, sigma=par$VY)
r = f/g

## data generation
X = rmvnorm(n=par$nX, mean=par$muX, sigma=par$VX)
Y = rmvnorm(n=par$nY, mean=par$muY, sigma=par$VY)
plot2d(X,Y)

## Indirect KDRE
I.r = I.KDRE(X=X,Y=Y,Z=Z,h=0.1)

## Direct KDRE
D.r = D.KDRE(X=X,Y=Y,Z=Z,h=0.1,eps=0.1)


## ================================================================
## Plot estimates and true densities
pdf(file=paste0(par$nX,"_",par$nY,".pdf"),
    width=4, height=4)
par(mar = c(4, 4, 2, 1), oma = c(0,0,0,0))
xl = range(r)
plot(1, type="n", xlim=xl, ylim=xl, log="xy",
     xlab="True", ylab="Estimates", main=paste0("n=",par$nX," / m=",par$nY))
par(new=T)
plot(r, I.r, pch="I", xlim=xl, ylim=xl, col="red", log="xy",
     xlab=" ",ylab=" ",xaxt="n",yaxt="n")
par(new=T)
plot(r, D.r, pch="D", xlim=xl, ylim=xl, col="blue", log="xy", 
     xlab=" ",ylab=" ",xaxt="n",yaxt="n")

ss = seq(xl[1],xl[2],length.out=10)
par(new=T)
plot(ss,ss, type="l",xlim=xl, ylim=xl,log="xy", 
     xlab=" ",ylab=" ",xaxt="n",yaxt="n")
legend("topleft",legend=c("Indirect","Direct"),
       col=c("red","blue"),pch=c("I","D"))
dev.off()


## ================================================================
## Contour plot
pdf(file=paste0("Contour_",par$nX,"_",par$nY,".pdf"),
    width=4, height=4)
par(mar = c(4, 4, 2, 1), oma = c(0,0,0,0))
contour(matrix(r, n.prec, n.prec), main="True")
dev.off()

pdf(file=paste0("Contour_I_",par$nX,"_",par$nY,".pdf"),
    width=4, height=4)
par(mar = c(4, 4, 2, 1), oma = c(0,0,0,0))
contour(matrix(I.r, n.prec, n.prec), main="Indirect")
dev.off()

pdf(file=paste0("Contour_D_",par$nX,"_",par$nY,".pdf"),
    width=4, height=4)
par(mar = c(4, 4, 2, 1), oma = c(0,0,0,0))
contour(matrix(D.r, n.prec, n.prec), main="Direct")
dev.off()


## ================================================================
## 3d plots
view <- matrix(c(-0.85,0.38,-0.38,0,
                 -0.53,-0.52,0.67,0,
                 0.06,0.77,0.64,0,
                 0,0,0,1),4,4)
par3d(userMatrix = view)
persp3d(x,y,r,col="gray",xlab="z1",ylab="z2",zlab="True")
persp3d(x,y,I.r,col="gray",xlab="z1",ylab="z2",zlab="Indirect")
persp3d(x,y,D.r,col="gray",xlab="z1",ylab="z2",zlab="Direct")

