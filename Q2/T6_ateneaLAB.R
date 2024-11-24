##EJERCICIO 1
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


#EJERCICIO 2

