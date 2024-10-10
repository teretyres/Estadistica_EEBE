#Ejercicio 1
X1<- c(rep(3,40),rep(6,35), rep(18,45))
sum(X1)
del<- c(13, 101, 35, 48, 87, 113, 72, 96)
X2<- X1[-del]
sum(X2)
X3<- X2[1:50]
num6<- sum(X3==6)
num6
V<- sqrt((1/50)*sum(X3^2))
V

#Ejercicio 2
X<- c(pi/16*seq(0,16))
sum(X)
max(sin(X))
pos<- sin(X)==1
pos
Y<- X[-c(4,9,14)]
sum(sin(X)-sum(cos(Y)))

#Ejercicio 3
fact<- c(23, 33, 25, 45, 10, 28, 39, 27, 15, 38, 34, 29)
mes<- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')
tabla<- data.frame(mes, fact)
tabla
sum(fact)
mean(tabla$fact)

#Ejercicio 4
pH<- c( 3.08, 3.26, 3.67, 3.79, 3.89, 3.91, 4.11, 4.52, 4.55, 4.59, 4.66, 4.79, 5.02, 5.48, 5.60, 6.00, 6.15, 6.37, 16.38, 6.63, 6.89, 7.05, 7.18, 7.22, 7.94)
t<- seq(0, 24*3, by=3)
medidas<- data.frame(t, pH)
medidas$pH[19]<- NA
medidas  
mean(medidas$pH, na.rm=TRUE)    
acid<- medidas$pH[c(1,2,3,4,5,6,7,8,9,10,11,12)]
normal<-medidas$pH[c(seq(13, 18),20, 21)] 
basic<- medidas$pH[c(seq(22,25))]
mean(acid)
mean(normal)
mean(basic)


plot(medidas$t,medidas$pH); abline(h=c(5,7))
