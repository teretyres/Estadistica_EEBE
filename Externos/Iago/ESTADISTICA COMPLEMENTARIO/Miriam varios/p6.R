#exercici 1


x1 <- seq(0,1,0.001); #posibles resultados
f <- function(x)(4/(pi*(1+x^2)));
curve(f,0.1,col="red",lwd=5, main= "Función densidad");

mean.X <- 0.4412712
var.X <-  0.07851927

#Simulación
N <- 50
n <- 4
set.seed(321)
f1 <- f(x1)
samples <- sample(x1, n*N,replace=TRUE, prob=f1);

samples.X = as.data.frame(matrix(samples,ncol=n));
samples.X


sum.samples.X = apply(samples.X,1,sum);
sum.samples.X

mean(sum.samples.X)


var(sum.samples.X)

med.samples.X = apply(samples.X,1,mean)
mean(med.samples.X)

var(med.samples.X)

var.samples.X = apply(samples.X,1,var)
mean(var.samples.X)

n2 <- 50;
pnorm(24,mean=n2*mean.X, sd = sqrt(n2*var.X))-pnorm(17,mean=n2*mean.X,sd=sqrt(n2*var.X))

pnorm(0.41,mean=mean.X, sd = sqrt(var.X/n2))-pnorm(0.39,mean=mean.X,sd=sqrt(var.X/n2))

#exercici 2

mean.Y <- 80
var.Y <- 15^2
n <- 9
pnorm(800, mean = n*mean.Y, sd=sqrt(n*var.Y)) - pnorm(700,mean=n*mean.Y,sd=sqrt(n*var.Y))
pnorm(80, mean = mean.Y, sd=sqrt(var.Y/n)) - pnorm(78,mean=mean.Y,sd=sqrt(var.Y/n))
pchisq(250*(n-1)/var.Y,df=n-1)-pchisq(200*(n-1)/var.Y,df=n-1)

n<-100
n<-9
set.seed(321)
samples <- rnorm(N*n,mean=mean.Y, sd=sqrt(var.Y))
samples.Y = as.data.frame(matrix(samples, ncol = n))
samples.Y

sum.samples.Y = apply(samples.Y,1,sum)
hist(sum.samples.Y,prob=T)
curve(dnorm(x,mean=n*mean.Y, sd=sqrt(n*var.Y)),add=T,lwd=2,col="red")

med.samples.Y = apply(samples.Y,1,mean)
hist(med.samples.Y,prob=T)
curve(dnorm(x,mean=mean.Y, sd=sqrt(var.Y/n)),add=T,lwd=2,col="red")

var.samples.Y = apply(samples.Y,1,var)
hist(var.samples.Y,prob=T)
curve(dchisq(x,df=n-1),add=T, lwd=2, col="red")

hist(var.samples.Y*(n-1)/(var.Y),prob=T)
curve(dchisq(x,df=n-1), add=T,lwd=2,col="red")

