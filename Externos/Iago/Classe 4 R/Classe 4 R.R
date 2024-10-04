#uni(a,b) -> Distribució uniforme discreta
#binom(n,p) -> Distribució binomial
#geom(P) -> Distribució geomètrica
#nbinom(r,p) -> Distribució binomial negativa
#hyper(N,D,n) -> Distribució hipergeomètrica
#pois(??) -> Distribució de Poisson

#d-... -> f(x) = P(X=x)
#p-... -> F(x) = P(X<=x)
#q-... -> x tal que P(X<=x) = q
#r-... -> mostra aleatòria

#Exercici 1
#Si queremos analizar el número de bombas que impactan en un cuadrado en particular. 
#Nº experiments = 535; probabilitat èxit = 535/576 = Valor esperat = ??
Pois(535/576)
x1 <- 0:535
f1 <- dpois(x1,535/576); f1
plot(x1,f1,type="h", col="red",lwd = 5)
points(x1,f1,col="blue",lwd=3)
p2 <-dpois(2,535/576) #Probabilitat de que caiguin dues bombes en un àrea concreta
#P(x>0) = P(X>=1) = 1-P(X<1) = 1-P(X=0) = 1-P(X<=0)
1-dpois(0,535/576)
p1 <- 1-ppois(0,535/576)
ppois(0,535/576,lower.tail = FALSE) #P(X>0)
#Si queremos analizar el número de zonas que reciben exactamente 2 impactos. 
#576 regions(experiments); probabilitat èxit = P(X=2) = dpois(2,535/576) 
B(576,p2)
x2 <- 0:576
f2 <- dbinom(x2,576,p2)
plot(x2,f2,type="h", col="red",lwd = 5)
points(x2,f2,col="blue",lwd=3)
#P(Y<100) = P(Y<=99)
pbinom(99,576,p2)
#Valor esperat = n*p
V_esp <- 576*p2
#Si queremos analizar el número de zonas que deben ser inspeccionadas para encontrar 10 que hayan sido bombardeadas. 
#Z = Nº de zones a inspeccionar fins a trobar 10 impactades
BN(10,0.6049802)
x3 <- 0:30
f3 <- dnbinom(x3-10,size = 10,prob= 0.6049802)
plot(x3,f3,type="h", col="red",lwd = 5)
points(x3,f3,col="blue",lwd=3)
#P(Z>=20)= P(Z=19)
pnbinom(19-10,10,p1,lower.tail = FALSE)
10/p1

#Exercici 2
