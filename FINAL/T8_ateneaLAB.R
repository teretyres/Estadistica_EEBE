## EJERCICIO 1
n<- 10
X<- c(23.3, 22.5, 21.9, 21.5, 19.9, 21.3, 21.7, 23.8, 22.6, 24.7)
sigmax<- sqrt(1.96)
mu0<- 23
alfa<- 0.05
qnorm(alfa)
xbar<- mean(X); xbar
sd(X)
Zobs<- (xbar-mu0)/(sigmax/sqrt(n)); Zobs
pvalor<- pnorm(Zobs); pvalor
#-------------------------------------------------------------------------------

## EJERCICIO 2
mu02<- 6.8
X2<- c(7.3, 7.1, 7.9, 7.0, 7.2 )
alfa2<- 0.01
n2<- length(X2)
tc<- qt(1-alfa2, df=n2-1); tc# cola sup.
xbar2<- mean(X2); xbar2
s2<- sd(X2);s2
tobs<- (xbar2-mu02)/(s2/sqrt(n2));tobs
pvalor2<- 1-pnorm(tobs);pvalor2
#-------------------------------------------------------------------------------

## EJERCICIO 3
n3<- 250
xbar3<- 3.15
s3<- 0.34
mu03<- 3.20
alfa3<- 0.05
Zc<- qnorm(1-alfa3/2); Zc
Zobs<- (xbar3-mu03)/(s3/sqrt(n3)); Zobs
pvalor3<- pvalor<- 2*pnorm(Zobs); pvalor3
