dies <- c(1,2,4,5,8,10,11,14,16,20)
events <- c(255,1500,2105,5050,16300,45320,58570,375800,1525640,2577000)
ordis <- data.frame(dies,events)
plot (ordis$dies, ordis$events, pch=16,col="red",cex=1.5)

attach(coches)
mean(MPG,na.rm=TRUE)
attach(air)
mean(Wind,na.rm=TRUE)
model1=lm(Horsepower~ Displacement); 
plot(Displacement,Horsepower,pch=16,col="blue")
abline(model1,col="red")
summary(model1)
sqrt(0.8342)
cor(Horsepower,Displacement,use="na.or.complete")
f1 <- function(x) {model1$coef[1] + model1$coef[2]*(x)}
Displacement_pred = 151
Horsepower_pred= f1 (Displacement_pred);Horsepower_pred
points(Displacement_pred,Horsepower_pred, col = "black", pch= 20, cex = 4)
par(mfrow=c(1,2))
plot(Weight,MPG,xlab="Weight", ylab="MPG",col="red",pch=20,cex=1.5)
plot(Weight,log(MPG),col="red",pch=20,cex=1.5)
curve(exp(4.3)*exp(-0.0004033*x),add=T,col="blue",lwd=3)

