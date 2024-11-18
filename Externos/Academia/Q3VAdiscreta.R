#PROBLEMA 3

    a <- inicio;
    b <- fin ;
    h <- paso;
    fx <- c( probabilidades  );


x=seq(a,b,by=h);
sumaProb <- sum(fx); 
media <- sum(x*fx); media
esp2 <- sum(x^2*fx);
varianza <- esp2-media*media; 


promedio1<-12*media;
desviacion1<-sqrt(varianza)*sqrt(12);
promedio2<-media;
desviacion2<-sqrt(varianza)/sqrt(12);
fraseA <- "Teoricamente NO tienen que ser similares, sin embargo en este caso SI lo son.";
promedio3<-60*media;
desviacion3<-sqrt(varianza)*sqrt(60);
promedio4<-media;
desviacion4<-sqrt(varianza)/sqrt(60);
fraseB <- "Teoricamente deberian ser similares y los resultados lo confirman.";

promedio1
desviacion1
promedio2
desviacion2
fraseA
promedio3
desviacion3
promedio4
desviacion4
fraseB
