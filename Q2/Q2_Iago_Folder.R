## EJERCICIO 1
#distr. normal
mu<- 838
n<- 49
#P(xbar<864)= 0.975 
sigma<- sqrt(n)*(864-mu)/qnorm(0.975);sigma
#apartado a
n*mu
#apartado b
n*sigma^2
#apartado c
1-pnorm(41317, mu*n, sigma*sqrt(n))
#apartado d
#P(xbar<836)<5%
nd<- (qnorm(0.05)/((863-mu)/sigma))^2;nd
#apartado e
ne<- 16 #n3<30 no se sabe la distr. 
#-------------------------------------------------------------------------------

## EJERCICIO 2
mu2<- 5100
sigma2<- 1020
n2<- 22
#apartado a
mu_s2<- sigma2^2; mu_s2
#apartado b
NaN #No se sabe (=0 si n=infinity)
#apartado c
pchisq((n2-1)*1352520/sigma2^2, df=n2-1)-pchisq((n2-1)*520200/sigma2^2, df=n2-1)
#-------------------------------------------------------------------------------

## EJERCICIO 3
mu3<- 70.5
sigma3<- 8.3
#apartado a
NaN #n<<<
#apartado b
NaN #n<<<
#apartado c
NaN #??
#apartado d
NaN #n<<<
#apartado e
n3<- 121
pnorm(8640, mu3*n3, sigma3*sqrt(n3))
#-------------------------------------------------------------------------------

## EJERCICIO 4
x1<- c(0,1,2,3,4,5,6)
f1<- c(0.19,0.18,0.17,0.16,0.13,0.12,0.05)
n4<- 12
#apartado a
set.seed(591)
samples<- sample(x1, 12, replace=T, prob=f1)
sum(samples)
mean(samples)
var(samples)
#apartado b
set.seed(591)
samples2<- sample(x1, 12*800, replace=T, prob=f1)
samples2_x<- as.data.frame(matrix(samples2, ncol=12))
#1
sum_samples2_x<- apply(samples2_x,1,sum)
mean(sum_samples2_x) #opción e
#2
sqrt(var(sum_samples2_x))#opción c
#3
mean_sample2_x<- apply(samples2_x,1,mean) 
mean(mean_sample2_x) #opción e
#4
var_sample2_x<- apply(samples2_x,1,var)
mean(var_sample2_x) #opción a 
#5
hist(mean_sample2_x, prob=T)
curve(dnorm(x,2.42,0.27697), add=TRUE, col='red', lwd=2)
#opción a
#apartado c
samples3<- sample(x1, 12*5*800, replace=T, prob=f1)
samples3_x<-  as.data.frame(matrix(samples3, ncol=12*5))
#1
sum_sample3_x<- apply(samples3_x,1, sum)
mean(sum_sample3_x)#opción b
#2
sqrt(var(sum_sample3_x))#opción b
#3
mean_sample3_x<-apply(samples3_x, 1, mean)
mean(mean_sample3_x)#opción e
#4
sqrt(var(mean_sample3_x)) #opción b
#5
hist(mean_sample3_x, prob=T)
curve(dnorm(x, 2.42, 0.055393), add=T, col='red', lwd=3)
#opción a