#Ex1#
Dies=c(1,2,4,5,8,10,11,14,16,20);Dies
Ordinadors=c(340,571,1274,2058,7480,17853,28841,113206,265346,1512930);Ordinadors
plot(Ordinadors~Dies)
model2=lm(Ordinadors~Dies);model2
summary(model2)

#S'ha de fer l'aroximació per exponencials
model3=lm(log(Ordinadors)~Dies);model3
summary(model3)
Dies_pred=27
Log_Ordinadors_pred=predict(model3,data.frame(Dies=Dies_pred));
Log_Ordinadors_pred
Ordinadors_pred=exp(Log_Ordinadors_pred);Ordinadors_pred

#Ex2#
attach(notas)
data=(c(age));data
mean(data)
sum(data)/length(data)
median(data)
quantile(data,0.25)
IQR(data)
sd(data)
var(data)

#Ex3#
X=c(-3,4,7,-1,-1,8,4,-7,2,-4,-9,-8,-5,8,-2,-2,3,-1,6,-5,-2,5,8,9,-2,0,-9,-6,-2,9);X
sum(X)
Y=X[c(-16,-23,-15)];Y
sum(exp(X))-sum(exp(Y))
Y[23];Y[18]

#Ex4#
mis_dades<-iris
attach(mis_dades)
datax=c(Sepal.Width);datax
datay=c(Petal.Width);datay
plot(datay~datax)
model1=lm(datay~datax); model1
summary(model1)
f1 <- function(x) {model1$coef[1] + model1$coef[2]*(x)}
datax_pred = 3
datay_pred = f1(datax_pred);datay_pred
datay_pred = predict(model1,data.frame(datax=datax_pred));
datay_pred

#Ex5#
attach(notas)
data2=(c(S6));data2
ni = table(data2);
fi = table(data2)/length(data2);
Ni = cumsum(ni);
Fi = cumsum(fi);
Tabla_Frec = cbind(ni,fi,Ni,Fi);
Tabla_Frec

#Ex6#
mis_dades2.0=mtcars
attach(mis_dades2.0)
data56=c(hp);data56
dotchart(data56)

rm(list=ls())
data3=mtcars
dotchart(data3$hp)

#Ex7#
#R=0.3772
sqrt(0.3772)
