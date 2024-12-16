## EJERCICIO 1
#distr. normal
X<- c(19.2, 19.4, 18.4, 18.6, 20.5, 20.8)
n<- length(X)

#Apartado a (todo correcto)
sigmax<- 1
alfa<- 0.01
library(BSDA)
E<- sigmax/sqrt(n); E #Error estándar
ME<- qnorm(1-alfa/2)*(sigmax/sqrt(n)); ME # Margen de error
z.test(X, sigma.x=sigmax, conf.level= 1-alfa)



#Apartado b (valores incorrectos)
mux<- 19.5
set.seed(123)
sim<- rnorm(10000*6, mux, sigmax)
samples_x<-as.data.frame(matrix(sim, ncol=6))
mean_samples<- apply(samples_x,1,mean)
Liminf<- c(mean_samples-qnorm(1-alfa/2)*sigmax/sqrt(6))
sum(Liminf>19.5)/10000
Limsup<- c(mean_samples+qnorm(1-alfa/2)*sigmax/sqrt(6))
sum(Limsup<19.5)/10000
1-0.0045*2

#Apartado b (Versión Kevin) (valores correctos)
L_inf<- function(i){
  variable1<- rnorm(6,mux,sigmax)
  mean(variable1)-qnorm(1-alfa/2)*sigmax/sqrt(6)
}
set.seed(123)
simL_inf<- sapply(1:10000, L_inf)
sum(simL_inf>19.5)/10000

L_sup<- function(i){
  variable2<- rnorm(6,mux,sigmax)
  mean(variable2)+qnorm(1-alfa/2)*sigmax/sqrt(6)
}
set.seed(123)
simL_sup<- sapply(1:10000, L_sup)
sum(simL_sup<19.5)/10000

#Apartado c (todo correcto)
E<- sd(X)/sqrt(n); E #Error estándar
ME<- qt(1-alfa/2, df=n-1)*(sd(X)/sqrt(n)); ME # Margen de error
t.test(X, conf.level= 1-alfa)

#-------------------------------------------------------------------------------

## EJERCICIO 2 (todo correcto)
n2<-10
X2<- c(67, 67.3, 67.8, 66.4, 67.5, 67.5, 66.6, 67.1, 66.5 , 66.9)
alfa2<- 0.05
ME<- qt(1-alfa2/2, df=n2-1)*(sd(X2)/sqrt(n2)); ME # Margen de error
E<- sd(X2)/sqrt(n2); E #Error estándar
t.test(X2, conf.level= 1-alfa2)

#-------------------------------------------------------------------------------

## EJERCICIO 3 (todo correcto)
X3<- c(7.96, 7.90, 7.98, 8.01, 7.97, 8.03, 8.02, 8.04, 8.02)
n3<- 9
var(X3)
chi21<-qchisq(0.975, df=n3-1); chi21
chi22<-qchisq(0.025, df=n3-1); chi22
IC<- c((n3-1)*sd(X3)^2/chi21, (n3-1)*sd(X3)^2/chi22); IC
sqrt((n3-1)*sd(X3)^2/chi21)
sqrt((n3-1)*sd(X3)^2/chi22)
