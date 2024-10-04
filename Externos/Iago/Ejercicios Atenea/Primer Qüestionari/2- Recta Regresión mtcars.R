
#2 - RECTA DE REGRESIÓN de mtcars

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


y<- ;#ORDENADA y
x<- ;#ABCISA x

model=lm(y~x); 
summary(model)