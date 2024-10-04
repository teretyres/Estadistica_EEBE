f = c(0.2,0.17,0.16,0.13,0.12,0.11,0.11) #Funcion de distribución
set.seed(852) #Semilla en 852
clientes.mes = 600 #Número de clientes en un mes
años = 5 #Numero de años a muestrear
samples = sample(seq(0,6),12*clientes.mes*años, replace=T, prob=f) #Toma el muestreo de las posibilidades clientes.mes veces
samples.X = matrix(samples, ncol=12*años)
mean(apply(samples.X,1,sum)) #Promedio de envios anual, hace sumatorio de todos (sum)
sd(apply(samples.X,1,sum)) #Desviación tipica, hace sumatorio de todos (sum)
mean(apply(samples.X,1,mean)) #Promedio de la media de envios anual, hace la media de todos (mean)
sd(apply(samples.X,1,mean)) #Desviacion tipica media de envios anual, hace la media de todos (mean)
hist(apply(samples.X,1,mean), prob=T) #Genero el histograma a partir del sample de todos los datos
media.curva=2.57
varianza.curva=0.066752
curve(dnorm(x,mean=media.curva,sd=sqrt(varianza.curva)),add=T, lwd=2, col="red")