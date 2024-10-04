# Questionario 1
sqrt(0.8877)
0.049541*9.38+29.360
17.9195*9.38-1.9998
#Exer 2
table(notas$S1)
length(notas$S1)
cumsum(table(notas$S1))
table(notas$S1)/37
cumsum(table(notas$S1)/37)

#Exer 3
table(notas$S3)
barplot(notas$S3)

rm(list=ls())
datos <- read.delim("C:/Users/alumne/Desktop/Estadistica con R/notas.txt", stringsAsFactors=TRUE)
h=hist(datos$S3)

#Exer 4
X<-c(12,13,17,21,24,35,38,45,49,54,54,66,71,73,78)
Y<-c(302,312,343,368,385,428,438,460,468,480,481,503,513,516,524)
model <- lm(Y~X)
plot(X,Y)
abline(model, col="red")
model
coef(model)[2]*100+coef(model)[1]

#Exercicio 5
X<-seq(0,pi,by=pi/24)
X
Y<-X[c(-1,-11,-2)]
Y
sum(X)
sum(sin(X))-sum(cos(Y))
iris
petalo <- lm(iris$Sepal.Width~iris$Petal.Length)
plot(iris$Petal.Length,iris$Sepal.Width)
abline(petalo, col="red")
petalo
coef(petalo)[2]*4.9+coef(petalo)[1]

#Exer 7 Medidas de posición y tendencia
rock
mean() #Media, promedio
median() #Mediana, valor central al ordenar la muestra de menor a mayor
sort(table(rock), decreasing=TRUE) #Ver la Moda
summary(rock) #Calcula Media, Mediana, Mínimo, Máximo y Quantiles
quantile(rock$perm, 0.95) #Percentiles
IQR(rock$perm) #Quantil 75% - Quantil 25%
range(rock$perm) #Valor mínimo y máximo de datos
N = length(rock$shape)
var(rock$shape) #Varianza corregida
((N-1)/N)*var(rock$shape) #Varianza NO corregida
sd(rock$shape) #Desviación típica corregida
sqrt((N-1)/N)*sd(rock$shape) # Desviación típica NO corregida
