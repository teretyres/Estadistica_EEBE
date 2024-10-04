

#ANALISIS ESTADISTICO mtcars


datos<-mtcars
mpg<-datos[,1]
cyl<-datos[,2]
disp<-datos[,3]
hp<-datos[,4]
drat<-datos[,5]
wt<-datos[,6]
qsec<-datos[,7]
vs<-datos[,8]
am<-datos[,9]
gear<-datos[,10]
carb<-datos[,11]

val1<- ;#MEDIA
val2<- ;#MEDIANA
val3<- ;#PRIMER CUARTIL
val4<- ;#PERCENTIL 18%
val5<- ;#RANGO INTERCUARTÍLICO
val6<- ;#DESVIACIÓN TÍPICA (CORREGIDA)
val7<- ;#VARIANZA (CORREGIDA)

p1<-mean(val1); 
p2<-median(val2); 
p3<-quantile(val3,0.25); 
p4<-quantile(val4,prob=0.18); 
p5<-IQR(val5); 
p6<-sd(val6); 
p7<-sd(val7)^2; 

p1
p2
p3
p4
p5
p6
p7