#Exercici 1
mis_datos <- iris;
head(mis_datos)
mean(mis_datos$Sepal.Length)
median(mis_datos$Petal.Width)
summary(mis_datos$Sepal.Length)
IQR(mis_datos$Petal.Length)
sd(mis_datos$Sepal.Length)
var(mis_datos$Petal.Length)

#Exercici 2
data <- trees;
head(data)
pie(table(data$Height))

#Exercici 3
mis_datos <- mtcars;
head(mis_datos)
plot(mis_datos$qsec~mis_datos$mpg, col = "red", pch =16, cex = 1.5)
m1 = lm(mis_datos$qsec~mis_datos$mpg)
abline(m1,col="blue",lwd=2)
m1$coefficients[1]
m1$coefficients[2]
summary(m1)

#Exercici 4
mis_datos <- rock;
head(mis_datos)
breaks = seq(1000,13000,by=2000);
dades2 = cut(mis_datos$area, breaks, right=FALSE);
head(dades2, n=6);
ni <- table(dades2); ni
fi <- ni/length(mis_datos$area); fi
Ni <- cumsum(ni); Ni
Fi <- cumsum(fi); Fi

#Exercici 5
sqrt(0.9107)
-0.4437+10.9181*1
-0.4437+10.9181*2.91

#Exercici 6
x <-c(6,9,12,13,35,38,38,42,46,48,49,50,62,62,79) #Nuemero de dias
y <-c(218,260,294,303,412,422,422,433,443,447,450,453,476,476,503) #Pc infectados
length(x)
length(y)

plot(y~x,col="red",pch=16, cex=0.5) #color(col="x"), pch(formas(triangulo, cuadrado...)), cex (tamano)


modelo1 <- lm(y~x) #Hace la regresion lineal

modelo1$coefficients[1]  #Este se suma

modelo1$coefficients[2]  #Este se multiplica por x

modelo1$coefficients[2] * ("VALOR X") + modelo1$coefficients[1] # =TIEMPO x

modelo1$coefficients[2]*96+modelo1$coefficients[1] #=tiempo x

abline(modelo1, col="blue", lwd=2) # Dibuja la recta de regresion

