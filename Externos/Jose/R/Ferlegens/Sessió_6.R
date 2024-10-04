#####################PREGUNTA 1######################
###Exercici 1
f<-function(x) {4/pi/(1+x^2)}

curve(f(x),0,1,lwd=3,col="blue")

x<-seq(0,1,.001)
set.seed(321)
m_200<-sample(x,200,replace=TRUE,prob=f(x))   #agafes un mostreig
#per fer 50 mostres de 4, podem fer una mostra de 4 repetida 20 cops o una de 200 entre 4 mostres
mostra<-matrix(m_200,ncol=4)

#per trobar la suma mostral utilitzem apply
suma<-apply(mostra,1,sum)
mean(suma) #la mitjana de la suma mostral
mean(m_200)*4 #comprovem
sd(suma)^2  #variança de la suma mostral
sd(m_200)^2*4

###Exercici 2
mitjanes<-apply(mostra,1,mean)
mean(mitjanes)
sd(mitjanes)^2

###Exercici 3
variances<-apply(mostra,1,var)
mean(variances)

###Exercici 4
pnorm(24,50*0.4412712,sqrt(50*0.07851927),lower.tail = TRUE)-pnorm(17,50*0.4412712,sqrt(50*0.07851927),lower.tail = TRUE)
pnorm(0.41,0.4412712,sqrt(0.07851927/50),lower.tail = TRUE)-pnorm(0.39,0.4412712,sqrt(0.07851927/50),lower.tail = TRUE)
#la variança no es pot calcular

#############PREGUNTA 2########
#Exercici 1
set.seed(123)
n<-9
N<-100
rnorm(N*n,80,15)
mostres<-matrix(rnorm(N*n,80,15),ncol=n)

sumes<-apply(mostres,1,sum)
hist(sumes,freq = FALSE)
curve(dnorm(x,n*80,sqrt(n)*15),lwd=3,col="red",add=TRUE)

mitjanes<-apply(mostres,1,mean)
hist(mitjanes,freq = FALSE)
curve(dnorm(x,80,15/sqrt(n)),lwd=3,col="red",add=TRUE)

variances<-apply(mostres,1,var)
hist(variances,freq=FALSE)
curve(dchisq(x,n-1),lwd=3,col="red",add=TRUE,n=1000)

estadistic<-variances/15^2*(n-1)
hist(estadistic,freq = FALSE)
curve(dchisq(x,n-1),lwd=3,col="red",add=TRUE,n=1000)

###Pregunta 2
pnorm(800,80*9,15*sqrt(9),lower.tail = TRUE)-pnorm(700,80*9,15*sqrt(9),lower.tail = TRUE)
pnorm(80,80,15/sqrt(9),lower.tail = TRUE)-pnorm(78,80,15/sqrt(9),lower.tail = TRUE)
pchisq(250/15^2*(9-1),9-1)-pchisq(200/15^2*(9-1),9-1)
