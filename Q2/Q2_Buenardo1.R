## EJERCICIO 1
p=0.48
#apartados 1 y 2
## BINOMIAL NEGATIVA
#nº de fracasos hasta un nº fijo de éxitos

#x= nº de fracasos
#r= nº éxitos
dnbinom(95-45,45,p)
pnbinom(87-45,45, p)- pnbinom(83-45,45,p)

#apartado 3
E_nb<- 49/p; E_nb
Var_nb<- 49*(1-p)/p^2; Var_nb
#-------------------------------------------------------------------------------

## EJERCICIO 2
mu<- 4
sigma<- 0.97
#apartado 1
curve(dnorm(x,mu,sigma),0,8, col='red')
#apartado 2, 3 y $
1-pnorm(4.1,mu,sigma)
pnorm(4,mu,sigma)- pnorm(3.1,mu,sigma)
qnorm(0.86,mu,sigma)
#apartado 5
pnorm(22.9, mu*7, sqrt(7)*mu)
#-------------------------------------------------------------------------------

## EJERCICIO 3
## POISSON
#nº de resultados en un intervalo específico de espacio
#lambda=nº de eventos/espacio
lambda<- 8 #llamadas/hora
## EXPONENCIAL: tiempo q transcurre antes de un evento
#apartado 1 
#como dexp es una variable contínua no puede da un valor exacto
#apartado 2
1-pexp(0.167,lambda)
#apartado 3
sim<-rexp(200000,lambda)
mean(sim)
#-------------------------------------------------------------------------------

#EJERCICIO 4
## POISSON
#nº de resultados en un intervalo específico de espacio
#lambda=nº de eventos/espacio
l4<- 10 #imperfecciones/centimetro
1-ppois(11,l4)
dpois(41,l4*4 )
sim4<-rpois(400000,l4)
var(sim4)
curve(dpois(x,l4), 0,25,col='red', lwd=2)
