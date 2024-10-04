#### Exercici 1 ###
dpois(2,535/576)

x<-0:5
f<- dpois(x,535/576)
plot(x,f,type="h",ylim=c(0,max(f)),lwd=3,col="red")
points(x,f,lwd=3,col="red")
ppois(0,535/576,lower.tail=FALSE)


p<-dpois(2,535/576)
x<-75:125
f<- dbinom(x,576,p)
plot(x,f,type="h",ylim=c(0,max(f)),lwd=3,col="red")
points(x,f,lwd=3,col="red")
pbinom(99.5,576,p)
576*p


prob_min_una<-ppois(0,535/576,lower.tail=FALSE)
pnbinom(9.5,10,prob_min_una,lower.tail=FALSE) 
10/prob_min_una


###Exercici 2###
fn<-dnorm(x,9000,2000) #funcion normal con media y desviacion estandar
curve(dnorm(x,9000,2000),3000,15000,lwd=3,col="red") #grafica de la funció normal
curve(pnorm(x,9000,2000),3000,15000,lwd=3,col="red") #integral de la funció normal
pnorm(10000,9000,2000,lower.tail = TRUE) #probabilitat de normal de ser inferior a 1000
pnorm(12000,9000,2000,lower.tail=FALSE) #probabilitat de normal de ser superior a 12000
pnorm(10000,9000,2000,lower.tail = TRUE)-pnorm(7500,9000,2000,lower.tail = TRUE) #Prob entre uno y otro

qnorm(.9,9000,2000,lower.tail = FALSE)
qnorm(.3,9000,2000,lower.tail = TRUE)

n<-10000
set.seed(123)
mostra<-rnorm(n,9000,2000) #fa una mostra
mean(mostra) #mitjana de la mostra
median(mostra) #mediana
sd(mostra)



###Exercici 3###
curve(dexp(x,0.01005),0,500,lwd=3,col="green") #curva de funcion exponencial
curve(pexp(x,0.01005),0,500,lwd=3,col="purple")
pexp(100,0.01005,lower.tail=TRUE)
pexp(110,0.01005,lower.tail=TRUE)-pexp(80,0.01005,lower.tail=TRUE)
qexp(.5,0.01005) #distància mediana

n<-10000
set.seed(123)
mostra2<-rexp(n,0.01005)
mean(mostra2)
median(mostra2)
sd(mostra2)
