#--------------------------------------
#Ej1
#--------------------------------------

h = c(178, 181, 168, 183, 164, 181, 174, 176, 174, 176, 181, 168, 164, 174, 171) #x
p = c(82, 89, 68, 91, 65, 80, 79, 81, 80, 79, 82, 69, 67, 80, 78) #y

model = lm(p~h);model

plot(h,p)
plot(h,p,pch=16,col='red',cex=1.5)
abline(model,col='blue',lwd=5)
summary(model)
sqrt(0.8678)
b = -122.8993; m = 1.1533
Pred = m*159 + b; Pred
Altura_nueva = (72-b)/m; Altura_nueva

#--------------------------------------
#Ej2
#--------------------------------------

datos = read.delim("C:/Users/joser/OneDrive/Desktop/ª/Uni/Q3/ES/R/anscombe.txt",header=TRUE,sep=";",na.strings="NA",dec=",")
mean(datos$y1, na.rm=TRUE)
x1 = datos[,1];x1
y1 = datos[,2];y2
x2 = datos[,3]
y2 = datos[,4]
x3 = datos[,5]
y3 = datos[,6]
x4 = datos[,7]
y4 = datos[,8]

model1 = lm(y1~x1); model1
model2 = lm(y2~x2); model2
model3 = lm(y3~x3); model3
model4 = lm(y4~x4); model4
model2$coef[1]; model2$coef[2]
summary(model4)
sqrt(0.6667)
f1 <- function(x) {model4$coef[1] + model4$coef[2]*(x)} #f1 = b + mx
Displacement_predict = 8.5
Horsepower_predict = f1(Displacement_predict); Horsepower_predict
par(mfrow=c(2,2))
plot(x1,y1,xlab="Weight", ylab="MPG",col="red",pch=20,cex=1.5)
plot(x2,y2,xlab="Weight", ylab="MPG",col="red",pch=20,cex=1.5)
plot(x3,y3,xlab="Weight", ylab="MPG",col="red",pch=20,cex=1.5)
plot(x4,y4,xlab="Weight", ylab="MPG",col="red",pch=20,cex=1.5)

#--------------------------------------
#Ej3
#--------------------------------------

L = c(97,27,93,175,38,192,28,182,61,77)
I = c(521,863,712,163,138,811,534,442,963,313)
sum(L)/length(L)
plot(L,I)
h=hist(L,I)
