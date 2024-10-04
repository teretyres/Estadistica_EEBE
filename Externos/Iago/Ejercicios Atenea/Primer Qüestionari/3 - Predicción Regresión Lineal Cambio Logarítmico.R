

#3 - PREDICCION REGRESION LINEAL CON CAMBIO LOGARITMICO

x<-c( 1 , 2 , 4 , 5 , 8 , 10 , 11 , 14 , 16 , 20  );

y<-c(  );#ORDENADA y
valor<- ;#ABCISA x A PREDECIR

model=lm(log(y)~x)
f=function(x){exp(model$coef[1]+model$coef[2]*(x))}
f(valor)