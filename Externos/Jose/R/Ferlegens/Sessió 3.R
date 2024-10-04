#-------------------------------EJERCICI 1------------------------------------
x<-0:5
f<-c(1, 2, 2, 3, 4, 3)/15
sum(f)

#fem les grafiques
plot(x,f,type="h",ylim=c(0,max(f)), lwd=3, col="red")
points(x,f,lwd=3,col="red")
F<-cumsum(f)
plot(c(-1,x,6),c(0,F,1),type="s",lwd=3,col="red")
points(x,F,lwd=3,col="red")

#valor esperat
x*f
sum(x*f)
#mediana (al no ser exacte es fa directament amb la grafica i deduint)
abline(h=.5)
#varianza
sum(x^2*f)-sum(x*f)^2

#fer diferents simulacions
help(sample)
mostra<-sample(0:5,30, TRUE,f)
set.seed(12)
mostra
xb<- barplot(table(mostra)/30)
points(xb,f,lwd=3,col="red")
mean(mostra)
var(mostra)
mostra2<-sample(0:5,10000, TRUE,f)
xa<- barplot(table(mostra2)/10000)
points(xa,f,lwd=3,col="red")
mean(mostra2)
var(mostra2)

0.75*sum(x*f)-1.5
mostra3<-sample(0.75*mostra-1.5,10000, TRUE,f)
mean(mostra3)

#-----------------------EXERCICI 2------------------
f<-function(x) {4/(pi*(1+x^2))*(x>=0)*(x<=1)}
f(0)
f(-1)
f(0.5)

F<-function(x) {4*atan(x)/pi*(x>0)*(x<1)+(x>=1)}
F(0.5)
F(2)
#probabilidades
integrate(f,0,0.5)
#valor esperado
fx<-function(x) {(4/(pi*(1+x^2)))*x*(x>=0)*(x<=1)}
espx<-integrate(fx,0,1)
integrate(function(x) {f(x)*x},0,1)
#mediana
tan((0.5*pi)/4)
#varianza
espx2<-integrate(function(x) {f(x)*x^2},0,1)
espx2$value-espx$value^2

x<-seq(0,1,.001)
n<-30
set.seed(123)
mostraf<-sample(x,n,TRUE,prob=f(x))
mean(mostraf)
median(mostraf)
var(mostraf)
pnorm