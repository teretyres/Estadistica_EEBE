#Exercici 1
#a)
mu=79;
sd =sqrt(388.5)
x <- seq(0, 180, length=1000)
y <- dnorm(x, mean=80, sd) #nunca type h!!
plot(x, y,)
#b)
1-pnorm(91.6,mu,sd)
#c)
pnorm(95,mu,sd)-pnorm(64.6,mu,sd)
#d)
qnorm(0.17,mu,sd)
#e)
1-pnorm(60.19302+4,mu,sd)

#Exercici 2
#a)
dbinom(45-25,45,0.6)
#b)
1-pbinom(30,45,0.6)
#c)
set.seed(23);mean(rbinom(300000,45,0.6))
#d)
plot(0:45,dbinom(0:45,size=45,prob=0.6),type='h'); #Siempre type h

#Exercici 3
#a)
dnbinom(183-47,47,0.3)
#b)
pnbinom(110-36,36,0.3)-pnbinom(106-36,36,0.3)
#c)
E = 31/0.3; E
#d)
V = 31*(1-0.3)/0.3^2; V

#Exercici 4
#a)
lambda=22
1-ppois(26,lambda)
#b)
dpois(49,2*lambda)
#c)
set.seed(23);mean(rpois(300000,lambda))
#d)
plot(0:42,dpois(0:42,22),type='h') #siempre type h!!!

#Exercici 5
#a) 
0 #La probabilidad de que una variable continua tome un valor exacto es siempre 0
#b)
1-pexp(0.131,10)
#c)
set.seed(23); mean(rexp(100000,10))
