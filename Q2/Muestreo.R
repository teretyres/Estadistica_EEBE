
## SUMA MUESTRAL (Y)
Y<-sum(xi)
mu_y<- n*mux
sigma_y^2<- n*sigmax^2

## MEDIA MUESTRAL/ PROMEDIO (xbar)
xbar=1/n*sum(xi)
mu_xbar<- mu
sigma_xbar^2<- sigmax^2/n

## VARIANZA MUESTRAL (S^2)
S^2= 1/(n-1)*sum((x-xbar)^2
mu_s2<- sigmax^2
#si n-> infinito sigma_s=0
#Estandarización de la varianza: 
W=(n-1)*s^2/sigmax^2 #(chi^2)
dchisq((n-1)*s^2/sigmax^2, df=n-1) #n= tamaño de la muestra

## TLC (Teorema del límite central)
#Si X tiene una distr. normal: la suma y la media siguen una distr. normal y la varianza la distr. de chi
#Si X tiene una distr. desconocida y n>30: la distribución de la media y la suma muestral es normal.


## FUNCIONES de DENSIDAD
f <- function(x) {
  ###función f(x)
}
curve(f,0,1,col='red', lwd=3, mai= 'función de densidad', xlab='x', ylab='f(x)')#graficar

#SIMULAR (para n< 30, si no distr. normal)
N<- 50 #nº de muestras
n<- 4 #tamaño de muestras
size1<- N*n

#Matriz de valores
x1<- seq(0,1,0.001)
f1<- f(x1)
set.seed(321)
samples<- sample(x1, size1, replace=T, prob=f1)
samples_x<-as.data.frame(matrix(samples, ncol=n))
#función de distr. de suma
sum_sample_x<- apply(samples_x,1,sum) #mean(sum_sample_x), var(sum_sample_x)

#función de distr. media
mean_sample_x<- apply(samples_x,1,mean) #mean(mean_sample_x),...

#función de distr. varianza
var_sample_x<- apply(samples_x,1,var) #mean(var_sample_x),...





