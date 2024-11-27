## EJERCICIO 1
#distr. normal
mu<- 801
n<- 64
#P(xbar<830)= 0.881
#apartado a
mu
#apartado b
sigmax<- sqrt(n)*(830-mu)/qnorm(0.881); sigmax^2
#apartado c
pnorm(51635,mu*n,sqrt(n)*sigmax)
#apartado d
#P(xbar>819)=0.238
nd<- (sigmax*qnorm(1-0.238)/(819-mu))^2; nd
#apartado e
ne<- 25
#P(xbar<828)=0.15
mue<- 828-qnorm(0.15)*sqrt(ne)/sigmax; mue
mue-mu #27.02636
#-------------------------------------------------------------------------------

## EJERCICIO 2
mu2<- 5.1
sigma2<- 1.055
#apartado a
curve(dnorm(x,mu2,sigma2),0,9 ,col='red', lwd=3)
##este ejercicio y el 3 son iguales q los de buenardo