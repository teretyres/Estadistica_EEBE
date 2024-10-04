rm(list=ls())
theta = 2.3 #Parámetro "theta" a estimar usando una muestra

#Función de distribución
frame()
curve(theta/(x^(theta+1)), from=1, to=6, lwd=3, col='red') #Esto cambia

#Simula muestra de n observaciones
n = 2000 #Tamaño muestra
xs = exp(rexp(n, theta)) #Valores de X
hist(xs[xs<6], prob=T, add=T, breaks=15)
th_est = mean(xs)/(mean(xs)-1); th_est #Estimación de theta, esto cambia

#Estimación de theta con m muestras de n observaciones
m = 1*10^4
X_est=rep(m,0)
for(i in 1:m){xs = exp(rexp(n, theta)); th_est[i] = mean(xs)/(mean(xs)-1)} #Esto cambia
mean(th_est)
var(th_est)
var_th_est = (theta*(theta-1)^2/(n*(theta-2))); var_th_est #Esto cambia
hist(th_est, prob=TRUE, breaks=15)
curve(dnorm(x,theta,sqrt(var_th_est)), lwd=3, col='red', add=T)
