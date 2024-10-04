#Ex 1
rm(list=ls())
data = iris
data
data[,1]
dotchart(data$Sepal.Length)

#Ex 2

mis_datos = mtcars

mean(mis_datos$qsec)
median(mis_datos$drat)
summary(mis_datos$disp)
quantile(mis_datos$disp)
quantile(mis_datos$mpg,0.18)
IQR(mis_datos$drat)
quantile(mis_datos$drat,0.75)-quantile(mis_datos$drat,0.25)
sd(mis_datos$drat)
std = sd(mis_datos$qsec); std
var = std^2; var

#Ex 3

X = c(2,4,6,-8,-7,0,0,-9,5,6,-2,-7,3,8,3,0,-8,9,8,-5,-2,7,9,1,0,8,-5,9,-6,-6)
sum(X)
Y = X[-25][-22][-11]; Y
Xe = exp(X); Xe
Ye = exp(Y); Ye
sum(Xe)-sum(Ye)
Y[12] >= Y[21]

#Ex 4

datos = read.delim("C:/Users/joser/OneDrive/Desktop/ª/Uni/Q3/ES/R/notas.txt",header=TRUE,sep="\t",na.strings="NA")
datos[,3]
ni = table(datos$S1)
fi = table(datos$S1)/length(datos$S1)
Ni = cumsum(ni)
Fi = cumsum(fi)
Tabla_Frec = cbind(ni,fi,Ni,Fi); Tabla_Frec

#Ex 5

X = c(2,4,7,30,59,62,63,63,64,68,75,75,77,78,79)
Y = c(60,69,98,186,373,318,373,322,370,378,432,383,430,473,474)
model = lm(Y~X)
plot(X,Y)
abline(model,col='red')
model
f = 4.978*100+48.462; f
coef(model)[1]+coef(model)[2]*100
f1 <- function(x) {model$coef[1] + model$coef[2]*(x)} #f1 = b + mx
Prediction = 100
Value = f1(Prediction); Value

#Ex 6

mis_datos = mtcars
x = mis_datos$disp
y = mis_datos$wt
plot(x,y)
model = lm(y~x); model
coef(model)[2]
coef(model)[1]
summary(model)

#Ex 7

sqrt(0.3882)
194.31*0.13650+6.28148


