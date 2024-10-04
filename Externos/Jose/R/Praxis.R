#Ej ferlegens
p=0.47
r = 35
mu=r/p

-pnbinom(61,35,mu=mu)+pnbinom(64,35,mu=mu)
m=factorial(83)/(factorial(49)*factorial(34))
m*0.47^35*(1-0.47)^(84-35)
r = 35
f1 <- function(x) (factorial(x-1)/(factorial((x-1)-(35-1))*factorial(35-1)))*0.47^35*(1-0.47)^(x-35)
f1(64)-f1(61)

#-----Ej propuestos sesion 4------
#EJ1
x=c(0,1,2,3,4,5)
f=c(1/15,2/15,2/15,3/15,4/15,3/15)
EM=46/15#sum(x*f)
median(x)
var=(1-EM)^2*1/15+(1-EM)^2*2/15+(2-EM)^2*2/15+(3-EM)^2*3/15+(4-EM)^2*4/15+(5-EM)^2*5/15;var
plot(x,f,type = 'h',col='red',lwd=3)
points(x,f,col='red',lwd=8); 
gra.fx = recordPlot()
model1=lm(f~x);model1
abline(model1)
fun <- function(x) coef(model1)[1]+coef(model1)[2]*(x)
fun(6)
sum(f)
median(x)
var(x)

N = 10000
muestra = 5
set.seed(12)
s = sample(x,N,replace=T,prob=f); s
h1 = hist(s,breaks=0:5,freq=FALSE, add=T)
gra.fx; hist(s,breaks=0:5,freq=FALSE, add=T)
mean(s)
var(s)
-1.2*5+1.65*1
x = c(-6,-4.35,-2.7,-1.05,0.6,2.25)
plot(x,f,type = 'h',col='red',lwd=3)
points(x,f,col='red',lwd=8)
set.seed(12)
s = sample(x,N,replace=T,prob=f); s
h1 = hist(s,breaks=0:5,freq=FALSE, add=T)
mean(s)
var(s)


#EJ2
x = seq(0,1,0.01)
fx <- function(x) 4/(pi*(1+x^2))
FX <- function(x) 4*atan(x)/pi
y = fx(x)
y = FX(x)
FX(0.5)
plot(x,y)
(1.018592-fx(1))^2


#-----Ej propuestos sesion 5------

#EJ1
x=0:576
y= dbinom(x,576,prob=0.1703929)
plot(x,y, type="h", col='red', lwd=3,xlim=c(70,130))
points(x,y, col='red', lwd=10)
dbinom(x,576,prob=0.1703929)
pbinom(100,576,prob=0.1703929)

#EJ2

dnorm(7000,mean=9000,sd=2000)
pnorm(10000,mean=9000,sd=2000,lower.tail = FALSE)-pnorm(7500,mean=9000,sd=2000)
1-0.08191019
qnorm(0.1,mean=9000,sd=2000)

