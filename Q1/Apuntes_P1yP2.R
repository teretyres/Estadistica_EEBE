##P1
#Archivos .txt
read.table(file="name.txt", header=TRUE, dec=",", na.strings='NA')
#Basics
X<- c(x1, x2, x3,...,x2) #vector
rep(num, cuantas veces)
sort(dades$seccio) #ordena - a +
cut(dades$seccio, breaks=x) #cortar en x intervalos
rm(list=ls()) #equivalente a clear all en matlab
sum(X) #sumatorio
seq(x1,x2,saltos, length.out=n) #secuencia de numeros; n= longitud de la secuencia (si usamos length.out no pueden haber saltos)
head(x, n) #n primeros valores de x
tail(x, n) #n ultmos valores de x

#Rangos
diff(range(misdades))

#Tabla de frecuencias
ni<- table(dades$seccio) #cuantas veces sale cada cosa
fi<- ni/sum(ni) #Frecuencias relativas
Fi<- cumsum(fi) #suma acumulativa-> Frecuencia acumulada
Ni<- cumsum(ni) #n acumulada
cbind(ni, fi, Ni, Fi)

pie(fi)#Pie chart
pie((tables(misdades)))
boxplot(dades)
dotchart(misdades)
barplot((table(misdades)))
scatterplot(yaxis xaxis, ((N-1)/N)*var(misdades))

#Mirar CENTRALIDAD de los datos
hist(dades$seccio, breaks = x) #histograma con x-1 columnas
mean(dades$seccio) #media/ mitjana
median(dades$seccio) #mediana
#moda
freq<-table(misdades$Air.Flow)
freq_ord<-sort(freq, decreasing = TRUE)
moda<- names(freq_ord[1])

quantile(dades$seccio) #cuartil (25%)
quantile(dades$seccio, 0.18) #cortar en porcentaje deseado
muestras_200_250 <- sum(misdades$Displacement >= 200 & misdades$Displacement <= 250, na.rm = TRUE)
#DISPERSION de los datos
#Dispersion alrededor de la mediana
ul_cuartil<-quantile(dades$seccio, 0.75) #último cuartil
pr_cuartil<-quantile(dades$seccio, 0.25) #primer cuartil
ul_cuartil-pr_cuartil #rango intercuartil
IQR() #rango intercuartil entre el primero y ultimo cuartil
#Dispersion alrededor de la media
N = length(datos_1) 
var(dades$seccio) #varianza muestral
((N-1)/N)*var(misdades)#varianza no corregida
sd(dades$seccio) #desviacion tipica corregida
sqrt((N-1)/N)*sd(misdades)#desviacion típica no corregida


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
#dibujar linea de regresioón
plot(x,y,pch=16,col="red",cex=1.5)
mod<- lm(x~y)
abline(mod,col="blue",lwd=5)
points(8.5, ypred, col = "black", pch= 20, cex=4) #1 punto predecido

R<- sqrt(multiple R squared)

#cuando hay NaN
,na.rm=TRUE) #cuando se hace la media, mediana, etc

usa_cars <- misdades[misdades$Origin=="USA", ]#subgrupos




