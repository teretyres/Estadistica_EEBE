#EJERCICIO 1

consumo <- c(19.2,19.4,18.4,18.6,20.5,20.8)
sigma = 1 #varianza
alpha = 0.01 #1-alpha = nivel de confianza 99%
n = length(consumo)

#Error estándar

SE = sigma/sqrt(n); SE
E = qnorm(1-alpha/2)*SE; E
xbar = mean(consumo)
IC = xbar + c(-E,E); IC

library(TeachingDemos)
z.test(consumo, stdev = 1, conf.level=.99)


mu <- 19.5
sigma <- 1
N <- 10000
n <- 6
alpha <- 0.01

L <- function(i){
  consumo <- rnorm(n,mu,sigma)
  mean(consumo) - qnorm(1-alpha/2)*sigma/sqrt(n)
  
}

set.seed(123)
sim.L <- sapply(1:N,L)
hist(sim.L, freq = FALSE)
abline(v=mu)

sum(sim.L>mu)/N

U <- function(i){
  consumo <- rnorm(n,mu,sigma)
  mean(consumo) + qnorm(1-alpha/2)*sigma/sqrt(n)
}

set.seed(123)
sim.U <- sapply(1:N,U)
hist(sim.U,freq=FALSE)
abline(v=mu)

sum(sim.U < mu)/N

head(cbind(sim.L,sim.U))
sum(sim.L<mu & sim.U>mu)/N


#margen de error si no conocemos la varianza

SE = sd(consumo)/sqrt(n); SE
E = qt(1-alpha/2,n-1)*SE; E

xbar = mean(consumo)
IC = xbar + c(-E,E); IC

t.test(consumo,conf.level = .99)



#EJERCICIO 2
energia <- c(67, 67.3, 67.8, 66.4, 67.5, 67.5, 66.6, 67.1, 66.5, 66.9)
alpha = 0.05
n = length(energia)
S = sd(energia)

SE = S/sqrt(n); SE
E = qt(1-alpha/2,n-1)*SE; E

xbar = mean(energia)
IC = xbar + c(-E,E); IC

t.test(energia) #por defecto conf.level = 0.95



#EJERCICIO 3

peso <- c(7.96, 7.90, 7.98, 8.01, 7.97, 8.03, 8.02, 8.04, 8.02)
alpha = 0.05
n=length(peso)
var.sample = var(peso); var.sample

chi.alpha.lower = qchisq(alpha/2,df=n-1,lower.tail = TRUE); chi.alpha.lower #calcular el interior
chi.alpha.upper = qchisq(alpha/2,df=n-1,lower.tail = FALSE);chi.alpha.upper #parte superior

  #ICV

ICV = (n-1)*var.sample*c(1/chi.alpha.upper,1/chi.alpha.lower); ICV
ICVs = sqrt(ICV); ICVs


sigma.test(peso)

  
sigma.test(peso)$conf.int
sqrt(sigma.test(peso)$conf.int)

