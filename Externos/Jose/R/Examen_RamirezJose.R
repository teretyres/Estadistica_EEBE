
#Exercici 1
med=87.2
desv=5.7
#1
muestra = 16
EX=muestra*media

N = 800000
n.W = 64
mean.W = med 
sd.W = desv
samples = rnorm(N*n.W, mean=mean.W, sd=sd.W)

samples.W = as.data.frame(matrix(samples, ncol=n.W))
sum.samples.W = apply(samples.W,1,sum)

mean(sum.samples.W) 

var(sum.samples.W)
sd(sum.samples.W)
