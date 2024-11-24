## EJERCICIO 1
mu<- 0.4412712
sigma<-sqrt(0.07851927)

f <- function(x) {
  4 / (pi * (1 + x^2))
}

#graficar
curve(f,0,1,col='red', lwd=3, mai= 'función de densidad', xlab='x', ylab='f(x)')

#simulación
N<- 50 #nº de muestras
n<- 4 #tamaño de muestras
size1<- N*n

x1<- seq(0,1,0.001)
f1<- f(x1)
set.seed(321)
samples<- sample(x1, size1, replace=T, prob=f1)
samples_x<-as.data.frame(matrix(samples, ncol=n))

#función de distr. de suma
sum_sample_x<- apply(samples_x,1,sum)
mean(sum_sample_x)
0.4412712*n #comprovación q sea la media sea n veces mayor
var(sum_sample_x)
0.07851927*n
#como n menor a 30 no es distribución normal

#función de distr. media
mean_sample_x<- apply(samples_x,1,mean)
mean(mean_sample_x)
var(mean_sample_x)
0.07851927/n

#función de distr. varianza
var_sample_x<- apply(samples_x,1,var)
mean(var_sample_x)

n2=50
#se puede hacer distr. normal
pnorm(24, n2*mu, sqrt(n2)*sigma)- pnorm(17, n2*mu, sqrt(n2)*sigma)
pnorm(0.41, mu, sigma/sqrt(n2))- pnorm(0.39, mu, sigma/sqrt(n2))
pchisq((n2-1)*0.15^2/sigma^2, df=n2-1)- pchisq((n2-1)*0.05^2/sigma^2, df=n2-1)


## EJERCICIO 2
mux<- 80
sigmax<- 15
#distr. normal
#Simular
n_2<- 9
N_2<- 100
size2<- n_2*N_2
set.seed(321)
muestra<- rnorm(size2, mux, sigmax)
samples_2<- as.data.frame(matrix(muestra, ncol=n_2))
#distr. suma
sum_samples2<- apply(samples_2, 1, sum)
hist(sum_samples2, prob=T)

#distr. media
mean_samples2<- apply(samples_2,1,mean)                               
hist(mean_samples2, prob=T)       
curve(dnorm(x, mux, sigmax/sqrt(n_2)), col='red', add=T)
#distr. varianza
var_samples2<- apply(samples_2, 1, var)
hist(var_samples2, prob=T)
curve(dchisq(x, df=n_2-1), col='red', add=T)

stat<- (n_2-1)*var_samples2/sigmax^2
hist(stat, prob=T)
curve(dchisq(x, n_2-1), col='red', add=T)

#P(700<T<800); n=9
n3<- 9
pnorm(800, mux*n3, sqrt(n3)*sigmax)- pnorm(700,mux*n3, sqrt(n3)*sigmax)
#P(78<xbar<80)
pnorm(80, mux,sigmax/sqrt(n3))-pnorm(78, mux,sigmax/sqrt(n3))
#P(200<s^2<250)
pchisq((n3-1)*250/sigmax^2, df=n3-1)-pchisq((n3-1)*200/sigmax^2, df=n3-1)
