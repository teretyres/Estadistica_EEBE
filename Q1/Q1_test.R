#Ejercicio 1
X<- c(pi/30*seq(0,30))
sum(X)
Y<- X[-c(9,25,5)]
sum(sin(X))-sum(cos(Y))
data.frame(x=Y)

#Ejercicio 2
mean(notas$S1)
median(notas$S1)
quantile(notas$S1)
IQR(notas$S1)
sd(notas$S1) #desviacion tipica corregida
var(notas$S1)

#Ejercicio 3
datos<- mtcars
datos
ni<- table(datos$am) #cuantas veces sale cada cosa
fi<- ni/sum(ni) #Frecuencias relativas
Fi<- cumsum(fi) #suma acumulativa-> Frecuencia acumulada
Ni<- cumsum(ni) #n acumulada
cbind(ni, fi, Ni, Fi)

#Ejercicio 4
data<-trees
hist(data$Volume)

#Ejercicio 5
dades<- rock
x<- dades$area
y<- dades$peri
xbar<- mean(x)
ybar<- mean(y)
m<- (sum((x-xbar)*(y-ybar)))/(sum((x-xbar)^2)) #derivada por minimos cuadrados
b<- ybar-m*xbar
m
b
ypredict<- m*x+b #manera clasica
mod<- lm(y~x) #modelo lineal
summary(mod)

#Ejercicio 6
X<-c(1,2,4,5,8,10,11,14,16,20)
Y<- c(981,1306,3708,4943,21146,44609,78398,287780,631983,4192426)
mod1<- lm(X~Y)
summary(mod1)
t<- 7.324*24+3.373*10^(-6)
t

#Ejercicio 7
sqrt(0.633)
13.497*1+3.322
13.497*0.53+3.322
