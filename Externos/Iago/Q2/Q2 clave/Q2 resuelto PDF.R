#Exercici 1
#a)
mu=80;
sd =sqrt(396.4)
x <- seq(0, 150, length=1000)
y <- dnorm(x, mean=80, sd) #nunca type h!!
plot(x, y,)
#b)
1-pnorm(90.2,mu,sd)
#c)
pnorm(102,mu,sd)-pnorm(56.2,mu,sd)
#d)
qnorm(0.22,mu,sd)
#e)
1-pnorm(64.62579+2,mu,sd)

#Exercici 2
#a)Cuál es la probabilidad de que haya una distancia de 0.089 centímetros entre dos imperfecciones del alambre? 
0 #La probabilidad de que una variable continua tome un valor exacto es siempre 0
#b)
1-pexp(0.2,7)
#c)
set.seed(23); mean(rexp(400000,7))
#Exercici 3
#a)
dnbinom(23,30,0.6)
#b)
pnbinom(43-25,25,0.6)-pnbinom(39-25,25,0.6)
#c)
E = 32/0.6; E
#d)
V = 32*(1-0.6)/0.6^2; V

#Exercici 4
#a)
dbinom(32-16,32,0.48)
#b)
1-pbinom(20,32,0.48)
#c)
set.seed(23);mean(rbinom(300000,32,0.48))
#d)
plot(0:32,dbinom(0:32,size=32,prob=0.48),type='h'); #Siempre type h

#Exercici 5
#a)
lambda=16
1-ppois(14,lambda)
#b)
dpois(65,3*16)
#c)
set.seed(23);mean(rpois(100000,16))
#d)
plot(0:39,dpois(0:39,16),type='h') #siempre type h!!!
