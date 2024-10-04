#ESTIMACIÓ INTERVALAR

#Exercici 1
#Primer apartat
sigma = 1
consumo = c(19.2,19.4,18.4,18.6,20.5,20.8)
n = length(consumo)
x_bar = mean(consumo)
alfa = 0.01
z_alfamedios = qnorm(1-alfa/2); z_alfamedios
Errorestandar = sigma/sqrt(n); Errorestandar
MargenError = z_alfamedios*Errorestandar; MargenError
Intervaloconfianza = x_bar+c(-MargenError,MargenError); Intervaloconfianza
install.packages("TeachingDemos") #instalar TeachingDemos
library(TeachingDemos)
z.test(consumo,stdev=1,conf.level=0.99) #confirmar que lo hecho anteriormente está perfecto

#Segon apartat (Simulació)
mu = 19.5
sigma = 1
N = 10000
n = 6
alfa = 0.01
set.seed
#Inferior
Inferior = function(i){
  consumo = rnorm(n,mean=mu,sd=sigma)
  mean(consumo)-qnorm(1-alfa/2)*sigma/sqrt(n)
}
set.seed(123)
simulacionfuncionA = sapply(1:N,Inferior)
hist(simulacionfuncionA)
abline(v=mu)
sum(simulacionfuncionA > mu)/N

#Superior
Superior = function(i){
  consumo = rnorm(n,mean=mu,sd=sigma)
  mean(consumo)+qnorm(1-alfa/2)*sigma/sqrt(n)
}
set.seed(123)
simulacionfuncionB = sapply(1:N,Superior)
hist(simulacionfuncionB)
abline(v=mu)
sum(simulacionfuncionB < mu)/N

sum(simulacionfuncionA < mu & simulacionfuncionB > mu)/N

#Tercer apartat
consumo = c(19.2,19.4,18.4,18.6,20.5,20.8)
n = length(consumo)
S = sd(consumo)
x_bar = mean(consumo)
alfa = 0.01
t_alfamedios = qt(1-alfa/2,n-1)
Errorestandar= S/sqrt(n); Errorestandar
MargenError = t_alfamedios*Errorestandar; MargenError
Intervaloconfianza = x_bar+c(-MargenError,MargenError); Intervaloconfianza
t.test(consumo, conf.level = 0.99)

#Exercici 2
energia =c(67,67.3,67.8,66.4,67.5,67.5,66.6,67.1,66.5,66.9)
x_bar = mean(energia)           
S = sd(energia)
n = length(energia)
alfa = 0.05
t_alfamedios = qt(1-alfa/2,n-1)
Errorestandar= S/sqrt(n); Errorestandar
MargenError = t_alfamedios*Errorestandar; MargenError
Intervaloconfianza = x_bar+c(-MargenError,MargenError); Intervaloconfianza
t.test(energia)

#Exercici 3
peso =c(7.96,7.90,7.98,8.01,7.97,8.03,8.02,8.04,8.02)
n = length(peso)
alfa = 0.05
S = sd(peso)
var = S^2;var
chi_a = qchisq(alfa/2,n-1);chi_a
chi_b = qchisq(1-alfa/2,n-1);chi_b
Intervaloconfianza_var = c((n-1)*S^2/chi_b,(n-1)*S^2/chi_a); Intervaloconfianza_var
Intervaloconfianza_desv = sqrt(Intervaloconfianza_var); Intervaloconfianza_desv
install.packages("TeachingDemos") #instalar TeachingDemos
library(TeachingDemos)
sigma.test(peso)
