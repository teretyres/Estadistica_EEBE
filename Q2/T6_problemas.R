## EJERCICIO 4
mu<- 5
sigma<- 1
#distr. normal

#apartado a
n<- 20
#P(4.4<xbar<5.6)
pnorm(5.6,mu, sigma/sqrt(n))-pnorm(4.4,mu,sigma/sqrt(n))

#apartado b
#P(xbar>sigma)=0.1
sigmab<- qnorm(0.9)/sqrt(n)+mu;sigmab
#-------------------------------------------------------------------------------

## EJERCICIO 13
mu13<- 22.6
sigma13<- 2
n13<-65
#P(xbar>23)
1-pnorm(23,mu13,sigma13/sqrt(n13))
#-------------------------------------------------------------------------------

#EJERCICIO 16
mu16<- 29
sigma16<- 4
n16<- 37
#distr. normal
#P(xbar>30)
1-pnorm(30,mu16,sigma16/sqrt(n16))
#-------------------------------------------------------------------------------

## Pregunta 4
lambda<- 6/365
#distr. Poisson

sigma_e<- 1/lambda; sigma_e
sigma_e/sqrt(36)

pnorm(2465, sigma_e*36, sigma_e*sqrt(36))-pnorm(2143, sigma_e*36, sigma_e*sqrt(36))
#P(xbar>67)=0.119
(sigma_e*qnorm(1-0.119)/(67-sigma_e))^2
#-------------------------------------------------------------------------------

## EJERCICIO 24
dchisq((10^6-1)*0.01^2/(sqrt(10^6*0.5^2))^2, df=10^6-1)
sigma24<- sqrt(10^6*0.5^2); sigma24
qnorm(1-0.09)*sigma24+0.5
