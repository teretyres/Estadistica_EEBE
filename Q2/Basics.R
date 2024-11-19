## BASICS
X<- c(x1, x2, x3,...,x2) #vector
rep(num, cuantas veces)
sort(dades$seccio) #ordena - a +
cut(dades$seccio, breaks=x) #cortar en x intervalos
rm(list=ls()) #equivalente a clear all en matlab
sum(X) #sumatorio
seq(x1,x2,saltos, length.out=n) #secuencia de numeros; n= longitud de la secuencia (si usamos length.out no pueden haber saltos)
head(x, n) #n primeros valores de x
tail(x, n) #n ultmos valores de x

## TABLA de FRECUENCIAS
#Tabla de frecuencias
ni<- table(dades$seccio) #cuantas veces sale cada cosa
fi<- ni/sum(ni) #Frecuencias relativas
Fi<- cumsum(fi) #suma acumulativa-> Frecuencia acumulada
Ni<- cumsum(ni) #n acumulada
cbind(ni, fi, Ni, Fi)

## GRÁFICOS
pie(fi)#Pie chart
pie((tables(misdades)))
boxplot(dades)
dotchart(misdades)
barplot((table(misdades)))
scatterplot(yaxis, xaxis, ((N-1)/N)*var(misdades))