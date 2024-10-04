#Exercici 1
X1 <- c(rep(3,40),rep(6,35),rep(18,45)) 
sum(X1)
X2 <- X1[c(-13,-101,-35,-48,-87,-113,-72,-96)]
length(X2)
sum(X2)
X3 <- X2[seq(1,50,1)]
length(X3)
sort(X3)
((sum(X3^2))/length(X3))^0.5
#Exercici 2
X <-c(seq(0,((16*pi)/16),((pi)/16)))
length(X)
sum(X)
X1 = sin(X)
range(X1)
max(which(X1 >= 1))#Trobem posició x!
Y <- X[-c(4,9,14)]
sum(sin(X))-sum(cos(Y))
#Exercici 3
X1 <- c("Gener","Febrer","Març","Abril","Maig","Juny","Juliol","Agost","Setembre","Octubre","Novembre","Desembre")
X2 <- c(23, 33, 25, 45, 10, 28, 39, 27, 15, 38, 34, 29)
data.frame(X1,X2)
sum(X2)
min(X2)
min(which(X2<=10))#Trobem posicio minima per trobar el mes més rapid
X1[5]
max(X2)
max(which(X2>=45))
X1[4]
mean(X2)
which(X2>=28.83333)
X1[c(2,4,7,10,11,12)]
#Exercici 4
Mesures <- c(3.08, 3.26, 3.67, 3.79, 3.89, 3.91, 4.11, 4.52, 4.55, 4.59, 4.66, 4.79, 5.02, 5.48, 5.60, 6.00, 6.15, 6.37, 16.38, 6.63, 6.89, 7.05, 7.18, 7.22 ,7.94)
length(Mesures)
Temps <- c(seq(0,72,3))
length(Temps)
Medidas <-data.frame(Mesures,Temps)
which(Mesures >=14)
mean(Mesures[-19])
Mesures[19] = "NA"
Acid <- length(which(Mesures<=5))
Acid
Normal <- length(which(Mesures <=7))-12
Normal
Alcalino <- length(which(Mesures >7))-1
Alcalino
which(Mesures<=5)
mean(Mesures[1:12])
which(Mesures <=7)
mean(Mesures[c(13, 14, 15, 16, 17,18, 20, 21)])
which(Mesures >7)
mean(Mesures[22:25])
plot(Medidas$Temps,Medidas$Mesures);abline(h=c(5,7))
