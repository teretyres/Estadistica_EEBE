## EJERCICIO 1
#apartado 1
## HIPERGEOMÉTRICA
#Población finita de éxitos/fracasos, cuanso una unidad es seleccionada, esta disminuye en proporción

#x= nº éxitos buscados
#N= nº población total
#k= nº éxitos totales
#n= nº de repeticiones (sin reemplazo)
x<-10
N<-150
n<- 25
k<- 60
dhyper(x,k,N-k,n)

#apartado 2
n2<- 40
#P(x>=15)
1- phyper(14,k,N-k,n2)

#apartado 3
n3<- 30
N3<- 200
k3<- 80
Var_h<- n3*k3/N3*(1-k3/N3)*((N3-n3)/(N3-1)); Var_h
qhyper(0.25,k3,N3-k3,n3)

#apartado 4
N4<- 120
n4<- 35
k4<- 48
set.seed(789)
sim4<-rhyper(10000, k4, N4-k4,n4)
mean(sim4)

#apartado 5
## BINOMIAL NEGATIVA
#nº de fracasos hasta un nº fijo de éxitos

#x= nº de fracasos
#r= nº éxitos
x<- 12-5
p<- 0.4
r<- 5
dnbinom(x,r,p)
# ------------------------------------------------------------------------------

## EJERCICIO 2

#apartados 1 y 2
#distr. normal
mu<- 498 #hojas/paquete
#P(x>489)= 0.99865
#Paquete aceptable 492<x<501
sigma<- (489-498)/qnorm(1-0.99865); sigma
1-(pnorm(501,mu, sigma)-pnorm(492,mu,sigma))
dnorm(502, mu, sigma)

#apartado 3
mu2<- 499.5
#P(x<495)= 0.025
sigma2<- (495-mu2)/qnorm(0.025); sigma2

#apartado 4
mu3<- 502
sigma3<- 2
set.seed(321)
sim<- rnorm(10000, mu3, sigma3)
sum(sim>=497 & sim<=507)/10000

#apartado 5 (no estoy muy segura)
p5<- 0.02
x5<- 10
n5<- 500
dbinom(x5, n5, p5)*2 #quitar y poner asi q *2

# ------------------------------------------------------------------------------

## EJERCICIO 3 

mux<- 73.2
sigmax<- 6.6

#apartado a
n_a<- 121 #n_a> 30 -> distr. normal (TLC)
mu_s2<- sigmax^2; mu_s2

#apartado b
# n_b< 30 -> no se puede saber la distr.

#apartado c
# No se sabe la distr.

#apartado d
#n_d< 30 -> no se puede saber la distr.

#apartado e
n_e<- 100
#P(s^2<47)
pchisq((n_e-1)*47/sigmax^2, df=n_e-1)


