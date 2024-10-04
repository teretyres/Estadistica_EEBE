#EXERCICI 1

altura <- c(178, 181, 168, 183, 164, 181, 174, 176, 174, 176, 181, 168, 164, 174, 171);
peso <- c(82, 89, 68, 91, 65, 80, 79, 81, 80, 79, 82, 69, 67, 80, 78);
plot(altura,peso)
plot(peso~altura, col="red",pch=16,cex=1.5)
modelo1 <- lm(peso~altura) #menys decimals
modelo1$coefficients #més decimals
abline(modelo1,col="blue",lwd=2)
cor(peso,altura)
cor(peso,altura)^2
summary(modelo1)
1.153*173-122.899
modelo1$coefficients[2]*173+modelo1$coefficients[1]#MILLOR!!
modelo1$coefficients[1]
modelo1$coefficients[2]
(72-modelo1$coefficients[1])/modelo1$coefficients[2]

#EXERCICI 2

#1
modelo2 = lm(anscombe$y1~anscombe$x1)
summary(modelo2)
cor(anscombe$y1,anscombe$x1)
cor(anscombe$y1,anscombe$x1)^2
modelo2$coefficients[2]*8.5+modelo2$coefficients[1]
plot(anscombe$x1,anscombe$y1,col="red",pch=16,cex=1.5)
abline(modelo2,col="blue",lwd=2)
points(8.5,modelo2$coefficients[2]*8.5+modelo2$coefficients[1], col="black", pch=16, cex = 3)
#2
modelo3 = lm(anscombe$y2~anscombe$x2)
summary(modelo3)
cor(anscombe$y2,anscombe$x2)
cor(anscombe$y2,anscombe$x2)^2
modelo3$coefficients[2]*8.5+modelo3$coefficients[1]
plot(anscombe$x2,anscombe$y2,col="red",pch=16,cex=1.5)
abline(modelo3,col="blue",lwd=2)
points(8.5,modelo3$coefficients[2]*8.5+modelo3$coefficients[1], col="black", pch=16, cex = 3)
#3
modelo4 = lm(anscombe$y3~anscombe$x3)
summary(modelo4)
cor(anscombe$y3,anscombe$x3)
cor(anscombe$y3,anscombe$x3)^2
modelo4$coefficients[2]*8.5+modelo4$coefficients[1]
plot(anscombe$x3,anscombe$y3,col="red",pch=16,cex=1.5)
abline(modelo4,col="blue",lwd=2)
points(8.5,modelo4$coefficients[2]*8.5+modelo4$coefficients[1], col="black", pch=16, cex = 3)
#4
modelo5 = lm(anscombe$y4~anscombe$x4)
summary(modelo5)
cor(anscombe$y4,anscombe$x4)
cor(anscombe$y4,anscombe$x4)^2
modelo5$coefficients[2]*8.5+modelo5$coefficients[1]
plot(anscombe$x4,anscombe$y4,col="red",pch=16,cex=1.5)
abline(modelo5,col="blue",lwd=2)
points(8.5,modelo5$coefficients[2]*8.5+modelo5$coefficients[1], col="black", pch=16, cex = 3)

#EXERCICI 3

lluvia = c(97,27,93,175,38,192,28,182,61,77)
incendios = c(521,863,712,163,138,811,534,442,963,313)
plot(lluvia,incendios, col="red",pch=16,cex=1.5)
modelo6 = lm(incendios~lluvia)
modelo6$coefficients[2]*120+modelo6$coefficients[1]

#EXERCICI 4

modelo7 = lm(log(ester$conc)~ester$t)
plot(log(ester$conc)~ester$t, col="red",pch=16,cex=1.5)
summary(modelo7)
exp(modelo7$coefficients[2]*70+modelo7$coefficients[1])

