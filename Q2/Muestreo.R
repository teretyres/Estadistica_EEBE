
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

## TLC
#Si n>30, la distribución de la media y la suma muestral es normal.