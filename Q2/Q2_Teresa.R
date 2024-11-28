## EJERCICIO 1
##BINOMIAL NEGATIVA
#nº de ensayos hasta un nº fijo de éxitos
#x= nº de fracasos (ensayos-éxitos)
#r= nº éxitos
p1<- 0.15
r1<- 3
x1<- 8-3

#apartado 1
dnbinom(x1,r1,p1)

#apartado 2
1-pnbinom(9-5,5,p1)

#apartado 3
p13<- 0.2
r13<- 4
Var_nb<- r13*(1-p13)/p13^2; Var_nb
qnbinom(0.5, r13,p13)

#apartado 4
p14<- 0.12
set.seed(789)
sim14<- rnbinom(10000,6,p14)
mean(sim14)

#apartado 5
n15<- 15
p15<- 0.10
dnbinom(n15-6,6,p15)
#------------------------------------------------------------

## EJERCICIO 2
mu2<- 330
#distr. normal
#P(x>320)= 0.995
#lata aceptable: 325<x<335

#apartado 1
sigma2<- (320-mu2)/qnorm(1-0.995);sigma2
pnorm(335,mu2,sigma2)-pnorm(325,mu2,sigma2)

#apartado 2
dnorm(336,mu2,sigma2)# 0

#apartado 3
mu23<- 331
#P(x<327)=0.05
sigma23<- (327-mu23)/qnorm(0.05); sigma23

#apartado 4
set.seed(456)
sim2<-rnorm(10000,329,2)
sum(sim2>=234 & sim2<=334)

#apartado 5
dbinom(5,330,0.03)
#-------------------------------------------------------------

## EJERCICIO 3
#distr. normal
mu3<- 988
sigma3<-56

#apartado 1
sigma3^2

#apartado 2
#NaN

#apartado 3
pnorm(977, mu3,sigma3)

#aparatdo 4
#P(xbar>1022)
1-pnorm(1022,mu3,sigma3/sqrt(4))

#apartado 5
#P(xbar>979)
1-pnorm(979,mu3,sigma3/sqrt(81))
