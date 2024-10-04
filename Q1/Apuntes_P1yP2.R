##P1
#Archivos .txt
read.table(file="name.txt", header=TRUE, dec=",", na.strings='NA')
#Basics
X<- c(x1, x2, x3,...,x2) #vector
sort(dades$seccio) #ordena - a +
cut(dades$seccio, breaks=x) #cortar en x intervalos
rm(list=ls()) #equivalente a clear all en matlab
sum(X) #sumatorio
#Tabla de frecuencias
ni<- table(dades$seccio) #cuantas veces sale cada cosa
fi<- ni/sum(ni) #Frecuencias relativas
Fi<- cumsum(fi) #suma acumulativa-> Frecuencia acumulada
Ni<- cumsum(ni) #n acumulada
cbind(ni, fi, NI, Fi)
#Pie chart
pie(fi)
#Mirar CENTRALIDAD de los datos
hist(dades$seccio, breaks = x) #histograma con x-1 columnas
mean(dades$seccio) #media/ mitjana
median(dades$seccio) #mediana
quantile(dades$seccio) #cuartil (25%)
quantile(dades$seccio, 0.18) #cortar en porcentaje deseado
#DISPERSION de los datos
#Dispersion alrededor de la mediana
ul_cuartil<-quantile(dades$seccio, 0.75) #último cuartil
pr_cuartil<-quantile(dades$seccio, 0.25) #primer cuartil
ul_cuartil-pr_cuartil #rango intercuartil
#Dispersion alrededor de la media
var(dades$seccio) #varianza muestral
sd(dades$seccio) #desviacion tipica

##P2
#Regresion lineal
xbar<- mean(x)
ybar<- mean(y)
m<- (sum((x-xbar)*(y-ybar)))/(sum((x-xbar)^2)) #derivada por minimos cuadrados
b<- ybar-m*xbar
ypredict<- m*x+b #manera clasica
mod<- lm(y~x) #modelo lineal
summary(mod)
ypredict<- predict(mod,data.frame(x=x)) #cada x y su valor
#plotear real y regresion
plot(x,y)
lines(x,ypredict)
Rsq <- (sum(((ypredicted-ybar)^2)))/(sum(((y-ybar)^2))) #coeficiente de determinacion
R <- sqrt(Rsq) #coeficiente de correlacion






