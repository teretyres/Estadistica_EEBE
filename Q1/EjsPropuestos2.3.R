#Ejercicio 1
altura<- c(178, 181, 168, 183, 164, 181, 174, 176, 174, 176, 181, 168, 164, 174, 171)
peso<- c(82, 89, 68, 91, 65, 80, 79, 81, 80, 79, 82, 69, 67, 80, 78)

plot(altura, peso)
x<- altura
y<- peso
xbar<- mean(x)
ybar<- mean(y)
m<- (sum((x-xbar)*(y-ybar)))/(sum((x-xbar)^2)) #derivada por minimos cuadrados
b<- ybar-m*xbar
m
b
mod<- lm(y~x) #modelo lineal
abline(mod, col='blue')
summary(mod)
0.8576^2
1.1533*173-122.8993
R1<- sqrt(0.8678)
R1
R1_sq<- cor(x,y, use='na.or.complete')
R1_sq
m*173+b
(72-b)/m

#Ejercicio 2
x1bar<- mean(anscombe$x1)
y1bar<- mean(anscombe$y1)
m1<- (sum((anscombe$x1-x1bar)*(anscombe$y1-y1bar)))/(sum((anscombe$x1-x1bar)^2)) #derivada por minimos cuadrados
b1<- y1bar-m1*x1bar
m1
b1
y1predict<- m1*anscombe$x1+b1
R1sq <- (sum(((y1predict-y1bar)^2)))/(sum(((anscombe$y1-y1bar)^2))) #coeficiente de determinacion
R1<- sqrt(R1sq) #coeficiente de correlacion
R1sq
R1
y1pred<- m1*8.5+b1
plot(anscombe$x1,anscombe$y1,pch=16,col="red",cex=1.5)
mod1<- lm(anscombe$x1~anscombe$y1)
abline(mod1,col="blue",lwd=5)
points(8.5, y1pred, col = "black", pch= 20, cex
       = 4)

x2bar<- mean(anscombe$x2)
y2bar<- mean(anscombe$y2)
m2<- (sum((anscombe$x2-x2bar)*(anscombe$y2-y2bar)))/(sum((anscombe$x2-x2bar)^2)) #derivada por minimos cuadrados
b2<- y2bar-m2*x2bar
m2
b2
y2predict<- m2*anscombe$x2+b2
R2sq <- (sum(((y2predict-y2bar)^2)))/(sum(((anscombe$y2-y2bar)^2))) #coeficiente de determinacion
R2<- sqrt(R2sq) #coeficiente de correlacion
R2sq
R2
y2pred<-m2*8.5+b2
plot(anscombe$x2,anscombe$y2,pch=16,col="red",cex=1.5)
mod2<- lm(anscombe$x2~anscombe$y2)
abline(mod2,col="blue",lwd=5)
points(8.5, y2pred, col = "black", pch= 20, cex=4)

x3bar<- mean(anscombe$x3)
y3bar<- mean(anscombe$y3)
m3<- (sum((anscombe$x3-x3bar)*(anscombe$y3-y3bar)))/(sum((anscombe$x3-x3bar)^2)) #derivada por minimos cuadrados
b3<- y3bar-m3*x3bar
m3
b3
y3predict<- m3*anscombe$x3+b3
R3sq <- (sum(((y3predict-y3bar)^2)))/(sum(((anscombe$y3-y3bar)^2))) #coeficiente de determinacion
R3<- sqrt(R3sq) #coeficiente de correlacion
R3sq
R3
y3pred<-m3*8.5+b3
plot(anscombe$x3,anscombe$y3,pch=16,col="red",cex=1.5)
mod3<- lm(anscombe$x3~anscombe$y3)
abline(mod3,col="blue",lwd=5)
points(8.5, y3pred, col = "black", pch= 20, cex=4)

x4bar<- mean(anscombe$x4)
y4bar<- mean(anscombe$y4)
m4<- (sum((anscombe$x4-x4bar)*(anscombe$y4-y4bar)))/(sum((anscombe$x4-x4bar)^2)) #derivada por minimos cuadrados
b4<- y4bar-m4*x4bar
m4
b4
y4predict<- m4*anscombe$x4+b4
R4sq <- (sum(((y4predict-y4bar)^2)))/(sum(((anscombe$y4-y4bar)^2))) #coeficiente de determinacion
R4<- sqrt(R4sq) #coeficiente de correlacion
R4sq
R4
y4pred<-m4*8.5+b4
plot(anscombe$x4,anscombe$y4,pch=16,col="red",cex=1.5)
mod4<- lm(anscombe$x4~anscombe$y4)
abline(mod4,col="blue",lwd=5)
points(8.5, y4pred, col = "black", pch= 20, cex=4)


#Ejercicio 3
x5<- c(97,27, 93, 175, 38, 192, 28, 182, 61, 77)
y5<- c(521, 863, 712, 163, 138, 811, 534, 442, 963, 313)
plot(x5,y5,pch=16,col="red",cex=1.5)

#Ejercicio 4
plot(ester$t,ester$conc,pch=16,col="red",cex=1.5)
