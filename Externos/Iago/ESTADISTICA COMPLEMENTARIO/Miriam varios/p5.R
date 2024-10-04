f <- function(x)(4/(pi*(1+x^2)))
curve(f,0.1, col="red", lwd=5, main= "Función de densidad", xlab ="X" , ylab = "f(x)")

F1 <- function(x)(4*atan(x)/pi)
curve(F1, 0.1, col ="red", lwd=5,main = "Función de distribución", xlab = "X", ylab = "f(x)")


f(0)
f(1)
F(0)
F(0.5)


miu.X <- integrate(function(x) x*4/(pi*(1+x^2)), lower = 0, upper=1)
miu.X$Value

#mediana
Q2.X <- tan(pi/8); Q2.X

#Varianza

var.X<- integrate(function(x)(x-miu.X$Value)^2*4/(pi*(1+x^2)), lower=0, upper=1)
var.X$Value

x1 = seq(0,1,0.001) #posibles resultados
f1 = 4/(pi*(1+x^2))#probabilidad
set.seed(123)
sim.X <- sample(x1,10000, replace = TRUE, prob=f1)



hist(sim.X, freq = FALSE)
curve(f,0.1, col= "red", lwd=5, main= "Función de densidad")

#values

mean.sim <- mean(sim.X); mean.sim
var.sim <- var(sim.X); var.sim
med.sim <- median(sim.X); med.sim

set.seed(123)
sim.X <- sample(x1,10000, replace = TRUE, prob=f1)
hist(sim.X, freq = FALSE)
curve(f,0.1, col= "red", lwd=5, main= "Función de densidad")



