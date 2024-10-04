#----------------------------------------------
#  T6 - Muestreo y teorema central del límite
#----------------------------------------------

#Ejemplo 2
N = 300 #Número de muestras
n.W = 25 #Tamaño de la muestra
mean.W = 4.5 #Parámetros de la población
sd.W = 1.4;set.seed(10) #Fijación de la semilla de la aleatoriedad
samples = rnorm(N*n.W, mean=mean.W, sd=sd.W)
#Simulación de N * nr muestras
samples.W = as.data.frame(matrix(samples, ncol=n.W))
#Organización en un data.frame

#Ejemplo 1
N = 300 #Número de muestras
n.R = 5 #Tamaño de la muestra
min.R = 313.5 #Parámetros de la población
max.R = 346.5; set.seed(10) #Fijación de la semilla de la aleatoriedad
samples = runif(N*n.R, min=min.R, max=max.R)
#Simulación de N * nr muestras
samples.R = as.data.frame(matrix(samples, ncol=n.R))
#Organización en un data.frame

sum.samples.R = apply(samples.R,1,sum)
mean(sum.samples.R)
var(sum.samples.R)
hist(sum.samples.R,prob=T)

sum.samples.W = apply(samples.W,1,sum)
mean(sum.samples.W)
var(sum.samples.W)
hist(sum.samples.W,prob=T)

hist(sum.samples.W,prob=T) #Histograma de la suma muestral
curve(dnorm(x,mean=n.W*mean.W,sd=sqrt(n.W)*sd.W),add=T, lwd=2, col='red')
text(125,0.049,expression(N(n*mu,n*sigma^2)),col='red',cex=1.3)

#-- Teorema del Límite Central
#----

N = 300 #Número de muestras
min.R = 313.5 #Parámetros de la población
max.R = 346.5
mean.R = (min.R + max.R)/2
sd.R = sqrt((max.R - min.R)^2/12)
n.R = round(10^(seq(0,3,length=6))) #Diferentes tamaños de muestra
M.mean.samples.R = rep(0,6) #Inicialización del vector con ceros
par(mfrow=c(3,2)) #Se divide la figura en 6
for (i in 1:6) #Comienzo de las repeticiones {
  samples = runif(N*n.R[i], min=min.R, max=max.R)
  samples.R = as.data.frame(matrix(samples, ncol=n.R[i]))
  mean.samples.R = apply(samples.R,1,mean)
  #Calcula la media de cada muestra (fila)
  #para el título
  hist(mean.samples.R, prob=T, breaks=12)
  #Histograma de la media muestral
  curve(dnorm(x,mean=mean.R,sd=sd.R/sqrt(n.R[i])), add=T, lwd=2, col='red')


#--- Cheat Sheet ---

matrix(data,nrow=,ncol=) #Convierte datos almacenados en matiz especificada
as.data.frame() #Verifica si objeto es data.frame, o lo convierte
apply() #Aplica una función a todos los elementos de 
        #un objeto en la dirección especificada, 1 (filas) o 2 (columnas)
dchisq() #calcula la función de densidad de densidad de la distribución 
         #chi-cuadrado, probabilitat varianza muestral pchisq(valor/desviació_estandar^2*n-1,n-1)

