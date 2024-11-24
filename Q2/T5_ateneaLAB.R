## EJERCICIO 1

#apartado a
# nº de bombas en un cuadrdado específico-> Poisson
lambda<- 535/576 #nº de eventos/espacio
dpois(2,lambda) #impacten 2 bombas
1-ppois(0,lambda)#P(x>=1) al menos 1 bomba

#apartado b
# nº de zonas con 2 impactos
p<- 0.1703929 #del apartado anterior
n<- 576
pbinom(99,n,p) #P(x<100)
E_b<-n*p #esperanza
E_b

#apartado c
# nº de zonas inspeccionadas para encontrar 10 bombardeadas
r<- 10
pn<-0.6049802
x<- 20-r
1-pnbinom(x-1,r,pn)#P(x>=20)
pnbinom_Zk(r,pn,20,'>=')
E_nb<- r/pn
E_nb


##EJERCICIO 2
mu<- 9000
sigma<- 2000

#apartado a
pnorm(10000,mu,sigma)#P(x<1000)
1-pnorm(12000,mu,sigma)#P(x>12000)
pnorm(10000,mu,sigma)-pnorm(7500,mu,sigma)#P(7500<x<10000)
dnorm(7000,mu,sigma)
qnorm(0.1, mu,sigma)#valor superior 0.9
qnorm(0.3,mu,sigma)#valor inferior 0.3

#apartado b
set.seed(123)
sim<- rnorm(10000,mu,sigma)
mean(sim)
median(sim)
sqrt(var(sim))


##EJERCICIO 3
#X<- distancia (en m) recorre un animal desde donde nace hasta el primer territorio que ocupa
# exponencial
lambda2<- 0.01005

pexp(100,lambda2)#P(x<100)
pexp(110,lambda2)-pexp(80,lambda2) #P(80<x<110)
qexp(0.5, lambda2)

set.seed(123)
sim<- rexp(10000,lambda2)
mean(sim)
median(sim)
sqrt(var(sim))
