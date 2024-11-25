### BUENARDO 2

## EJERCICIO 1
mu<- 3.3
sigma<- 0.535
#apartado 1
curve(dnorm(x,mu,sigma), 0,8,col='red', lwd=2)
#apartados 2,3 y 4
1-pnorm(2.2,mu,sigma)
pnorm(4.3,mu,sigma)-pnorm(2.2,mu,sigma)
qnorm(0.34,mu,sigma)
#apartado 4
pnorm(26.5, mu*9, sqrt(9)*sigma)
#-------------------------------------------------------------------------------

## EJERCICIO 2
p<- 0.52
## BINOMIAL NEGATIVA
#nº de ensayos hasta un nº fijo de éxitos
#x= nº de fracasos (ensayos-éxitos)
#r= nº éxitos
dnbinom(94-50,50,p)
pnbinom(51-25, 25,p)-pnbinom(47-25,25,p)
E_nb<- 33/p; E_nb 
Var_nb<- 33*(1-p)/p^2; Var_nb
#-------------------------------------------------------------------------------

## EJERCICIO 3
## POISSON
#nº de resultados en un intervalo específico de espacio
#lambda=nº de eventos/espacio
lambda<- 22 #llamadas/hora
## EXPONENCIAL: tiempo q transcurre antes de un evento
#apartado 1
0 # contínua -> área 0
#apartado 2
pexp(0.054,lambda)
#apartado 3
sim<-rexp(200000,lambda)
mean(sim)
#-------------------------------------------------------------------------------

#EJERCICIO 4
#apartados 1 y 2
p4<- 0.47
dbinom(14,26,p4)
pbinom(19,30,p4)
#apartado 3
E_b<-34*0.53; E_b
Var_b<- 34*0.53*(1-0.53); Var_b
qbinom(0.25, 34,0.53)
#apartado 4
sim4<-rbinom(500000,28,p4)
mean(sim4)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

### BUENARDO 3

## EJERCICIO 4
p3<-0.48 
## BINOMIAL
#pruebas repetidas con éxito/fracaso, ensayos indepedientes
#n= nº de ensayos
#p= prob. éxito
#x= nº de éxitos
dbinom(16,32,p3)
1-pbinom(20,32,p3)
sim3<-rbinom(300000,32,p3)
mean(sim3)
curve(dbinom(x,32,p3), 0,30, col='red', lwd=2)
