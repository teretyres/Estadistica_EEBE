#Exercici 1
x <- 0:5
f <- c(1,2,2,3,4,3)/15
sum(f)
plot(x,f,type="h",col = "red",lwd = 5)#Histograma
points(x,f,col="black",lwd = 10)
F <- cumsum(f)
plot(x,F,type="s",col="red",lwd = 5)#Escala (només a acumulativa)!
points(x,F,col="black",lwd = 10)
E.X <- sum(x*f); E.X
V.X <- sum((x-E.X)^2*f);V.X

#30 casos
set.seed(12)
sim.x <-sample(x,30,replace = T,prob=f) # 30 casos de la semilla 12 amb repeticions i la probabilitat f
xb <-barplot(table(sim.x)/length(sim.x))
lines(xb,f,type="h",col = "red",lwd = 5)#Histograma
points(xb,f,col="black",lwd = 10)
mean(sim.x)
var(sim.x)

#10000 casos
set.seed(12)
sim.x <-sample(x,10000,replace = T,prob=f) # 10000 casos de la semilla 12 amb repeticions i la probabilitat f
xb <-barplot(table(sim.x)/length(sim.x))
lines(xb,f,type="h",col = "red",lwd = 5)#Histograma
points(xb,f,col="black",lwd = 10)
mean(sim.x)
var(sim.x)

Y <- 1.65*x -1.20*5+ 0.75*1.2*(5-x)
E.Y <- 0.75*E.X-1.5; E.Y
V.Y <- 0.75^2*V.X; V.Y

#10000 casos (Y)
set.seed(12)
sim.y <-sample(Y,10000,replace = T,prob=f) # 10000 casos de la semilla 12 amb repeticions i la probabilitat f
yb <-barplot(table(sim.y)/length(sim.y))
lines(yb,f,type="h",col = "red",lwd = 5)#Histograma
points(yb,f,col="black",lwd = 10)
mean(sim.y)
var(sim.y)


#Exercici 2
x <- seq(0,1,length=10000)
f <- 4/(pi*(1+x^2))
plot(x,f)
set.seed(123)
sim.x <-sample(x,30,replace = T,prob=f)
hist(sim.x,freq = F)
lines(x,f)
mean(sim.x)
var(sim.x)

set.seed(123)
sim.x <-sample(x,10000,replace = T,prob=f)
hist(sim.x,freq = F)
lines(x,f)
mean(sim.x)
var(sim.x)
