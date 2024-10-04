x1= 0:5
f1=c(1,2,2,3,4,5)/15 
plot(x1,f1, type ="h", col= "red", lwd=3, main = "Función de probabilidad", xlab="X", ylab="f(x)", xlim= c(-0.5, 5.5), ylim= c(0, 0.3))

points(x1,f1,col="red",lwd=10)
gra.fx.ej1 = recordPlot()

F1 = cumsum(f1)
plot(c(-1,x1,6),c(0,F1,1), type = "s", col="red", lwd=3, main= "función distribución", xlab = "X",ylab = "f(x)")

points(x1,F1,col="red", lwd=8)
gra.fx.ej1 = recordPlot()

f1
F1
library(MASS)
fractions(F1)

miu.X <- sum(x1*F1); miu.X
F1
F1 <= 0.5
Q2.X <- max(x1[F1<=0.5]); Q2.X

var.X <- sum((x1-miu.X)^2*f1); var.X

set.seed(12)
sim.venta <- ssample(x1,10000,replace=T, prob=f1)
sim.venta

f1 <- table(sim.venta)/length(sim.venta)
xb <- barplot(f1)
lines(xb,f1, type = "h", col="red", lwd=3)
points(xb,f1, col="red", lwd=10)
main.sim <- mean(sim.venta); mean.sim
var.sim <-var(sim.venta); var.sim

set.seed(123)
y1 <- 0.75*x1-1.5
sim.ben <- sample(y1, 10000, replace = T, prob = f1)
fi <- table(sim.ben)/length(sim.ben)
xb <- barplot(fi)
lines(xb, f1, col="red", lwd=10)
main.sim.ben <- mean(sim.ben); mean.sim.ben
var.sim.ben <-var(sim.ben); var.sim.ben

