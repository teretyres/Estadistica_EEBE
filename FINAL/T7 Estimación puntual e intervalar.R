library(BSDA)

## ESTIMACIÓN INTERVALAR

# 1. Estimación Media
xbar<- mean(X)#vector de X
Z<- (xbar-mux)/(sigmax/sqrt(n)) #varianza conocida y distr. normal
Z<- qnorm(1-alfa/2)#doble cola
ME<- qnorm(1-alfa/2)*(sigmax/sqrt(n)); ME # Margen de error
E<- sigmax/sqrt(n); E #Error estándar
IC<- c(xbar-Z*sigmax/sqrt(n), xbar+Z*sigmax/sqrt(n))

#Si tenemos el vector X
z.test(x, sigma.x=sigmax, conf.level= 1-alfa)#alternative='greater' (cola sup.)/ 'less' (cola inf.)

s<- sd(X)
TS<-  (xbar-mux)/(s/sqrt(n))#varianza desconocida y distr. normal
TS<- qt(1-alfa/2, n-1)#doble cola
IC<- c(xbar-TS*s/sqrt(n), xbar+TS*s/sqrt(n) )

#Si tenemos el vector X
t.test(x, conf.level= 1-alfa)#alternative='greater' (cola sup.)/ 'less' (cola inf.)

IC<- c(xbar-Z*sigmax/sqrt(n), xbar+Z*sigmax/sqrt(n)) #n>> y sigmax conocida
IC<- c(xbar-Z*s/sqrt(n), xbar+Z*s/sqrt(n)) #n>> y sigmax desconocida

# 2. Estimación Varianza
chi1<- qchisq(alfa/2)#doble cola
chi2<- qchisq(1-alfa/2)#doble cola
IC<- c((n-1)*s^2/chi21, (n-1)*s^2/chi22)

# 3. Estimación Proporción
#Distr. Binomial:
n*p>=5
n*(1-p)>=5

pbar<- X/n #E(pbar)= p, Var(pbar)= p*(1-p)/n
Z<- (pbar-p)/sqrt(p*(1-p)/n)
Z<- qnorm(1-alfa/2)
IC<- c(pbar- Z*sqrt(p*(1-p)/n), pbar+ Z*sqrt(p*(1-p)/n)) # si n>>> en vez de p podemos usar pbar

## SIMULACIÓN 

L_inf<- function(i){
  variable1<- rnorm(n,mux,sigmax)
  mean(variable1)-qnorm(1-alfa/2)*sigmax/sqrt(n)
}
set.seed()
simL_inf<- sapply(1:N, L_inf)

L_sup<- function(i){
  variable2<- rnorm(6,mux,sigmax)
  mean(variable2)+qnorm(1-alfa/2)*sigmax/sqrt(n)
}
set.seed()
simL_sup<- sapply(1:N, L_sup)
