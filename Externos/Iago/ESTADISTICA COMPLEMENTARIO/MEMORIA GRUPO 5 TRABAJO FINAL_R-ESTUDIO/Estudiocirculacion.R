install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)
#Vamos a realizar 3 regresiones lineales, la cantidad de vehiculos en funcion de los a�os, para poder realizar 
#una estimaci�n.


#Empezamos con el Total de Vehiculos.
Datos <- read.table(file="ap7-2000-2016.csv",sep=";",dec=",",
                    header = TRUE)

cor(Datos)

pairs((Datos))

regresion <- lm(Total ~ A�os, data = Datos)
summary(regresion)

plot(Datos$A�os, Datos$Total, xlab = "A�os", ylab = "Total",main="Regresi�n lineal IMD total en funci�n de los a�os")
abline(regresion)

#El R^2 es de 0.3458, por lo que es un resultado no muy fiable. Observamos que la regresion lineal da una tendencia
#decreciente debido a la crisis espa�ola. Por lo que tomaremos los datos del 2012 al 2016.



plot(Datos$A�os, Datos$Pesados, xlab = "A�os", ylab = "Pesados",main="Regresi�n lineal IMD Pesados en funci�n de los a�os")
qplot(A�os, Pesados, data=Datos, geom=c("point", "smooth"), method="lm", formula= y ~ poly(x, 3),level=0.99)
#Vemos que una regresion de polinimica de grado 3 se ajusta bastante bien.

#Realizamos una regresion polinomica de tercer grado
LinearModel.1 <- lm(Pesados ~ A�os +I(A�os^2)+I(A�os^3), data=Datos)
summary(LinearModel.1)
#no conseguimos realizar la polinomica de tercer grado el resultado es una de segundo grado.
LinearModel.2 <- lm(Pesados ~ A�os +I(A�os^2), data=Datos)
summary(LinearModel.2)

qplot(A�os, Pesados, data=Datos, geom=c("point", "smooth"), method="lm", formula= y ~ poly(x, 2),level=0.99)

#este modelo no nos interesa para realizar estimaciones futuras debido a la pendiente que sale negativa.


Datos <- read.table(file="ap7-2000-2016.csv",sep=";",dec=",",
                      header = TRUE)
names(Datos)

regresion2 <- lm(Pesados ~ A�os, data = Datos)
summary(regresion2)



plot(Datos$A�os, Datos$Pesados, xlab = "A�os", ylab = "Pesados",main="Regresi�n lineal IMD Pesados en funci�n de los a�os")
abline(regresion2)
#El R^2 mejora bastante pero la observaci�n es la misma. Vamos a tener que tomar los ultimos a�os para poder tener
#una estimaci�n razonable.


#Y ahora con los ligeros.
Datos <- read.table(file="ap7-2000-2016.csv",sep=";",dec=",",
                      header = TRUE)

regresion3 <- lm(Ligeros ~ A�os, data = Datos)
summary(regresion3)

plot(Datos$A�os, Datos$Ligeros, xlab = "A�os", ylab = "Ligeros",main="Regresi�n lineal IMD ligeros en funci�n de los a�os")
abline(regresion3)
#El R^2 es de 0.1799 y la pendiente negativa.


#Eliminamos los datos pre y durante la crisis

#Empezamos con el Total de Vehiculos.
Datosap7 <- read.table(file="ap7-4a�os.csv",sep=";",dec=",",
                    header = TRUE)
names(Datosap7)

cor(Datosap7)

RTotal <- lm(Total ~ A�os, data = Datosap7)
summary(RTotal)

plot(Datosap7$A�os, Datosap7$Total, xlab = "A�os", ylab = "Total",main="Regresi�n lineal IMD total en funci�n de los a�os")
abline(RTotal)


nuevos.a�os <- data.frame(A�os = seq(2017,2021))
predict(RTotal, nuevos.a�os)

confint(RTotal)

confint(RTotal, level = 0.9)

nuevos.a�os <- data.frame(A�os = seq(2012, 2021))
# Grafico de dispersion y recta
plot(Datosap7$A�os, Datosap7$Total, xlab = "A�os", ylab = "Total", xlim=c(2012, 2021),
     ylim=c(30000, 50000),main="Regresi�n lineal y estimaci�n total vehiculos (AP7 tarragona)")
abline(RTotal)

# Intervalos de confianza.
ic <- predict(RTotal, nuevos.a�os, interval = "confidence")
lines(nuevos.a�os$A�os, ic[, 2], lty = 5)
lines(nuevos.a�os$A�os, ic[, 3], lty = 5)

# Intervalos de prediccion.
ic <- predict(RTotal, nuevos.a�os, interval = "prediction")
lines(nuevos.a�os$A�os, ic[, 2], lty = 5, col = "red")
lines(nuevos.a�os$A�os, ic[, 3], lty = 5, col = "red")


#ahora con los vehiculos pesados
Datosap7 <- read.table(file="ap7-4a�os.csv",sep=";",dec=",",
                     header = TRUE)
names(Datosap7)

cor(Datosap7)


Linearmodel.2 <- lm(Pesados~ A�os +I(A�os^2), data=Datosap7)
summary(Linearmodel.2)

qplot(A�os, Pesados, data=Datosap7, geom=c("point", "smooth"), method="lm", formula= y ~ poly(x, 2))

par(mfrow=c(2,2))


plot(Linearmodel.2)
#vemos que este modelo no se ajusta bien. por lo que seguimos con la regresion lineal simple.

par(mfrow=c(1,1))
RPesados <- lm(Pesados ~ A�os, data = Datosap7)
summary(RPesados)

plot(Datosap7$A�os, Datosap7$Pesados, xlab = "A�os", ylab = "Pesados",main="Regresi�n lineal IMD Pesados en funci�n de los a�os")
abline(RPesados)

par(mfrow=c(2,2))
plot(RPesados)



#realizamos las predicciones
nuevos.a�os <- data.frame(A�os = seq(2012, 2021))
predict(Linearmodel.2, nuevos.a�os)
predict(RPesados, nuevos.a�os)

#podemos observar que cambian bastante los resultados segun el modelo utilizado

par(mfrow=c(1,1))



nuevos.a�os <- data.frame(A�os = seq(2012, 2021))
# Grafico de dispersion y recta
plot(Datosap7$A�os, Datosap7$Pesados, xlab = "A�os", ylab = "Pesados", xlim=c(2012, 2021),
     ylim=c(3000, 8000),main="Regresi�n lineal y estimaci�n vehiculos pesados (AP7 tarragona)")
abline(RPesados)

# Intervalos de confianza.
ic <- predict(RPesados, nuevos.a�os, interval = "confidence")
lines(nuevos.a�os$A�os, ic[, 2], lty = 5)
lines(nuevos.a�os$A�os, ic[, 3], lty = 5)

# Intervalos de prediccion
ic <- predict(RPesados, nuevos.a�os, interval = "prediction")
lines(nuevos.a�os$A�os, ic[, 2], lty = 5, col = "red")
lines(nuevos.a�os$A�os, ic[, 3], lty = 5, col = "red")

#Podemos entonces obtener una estimaci�n de la cantidad de vehiculos que pasaran en los a�os a venir.


nuevos.a�os <- data.frame(A�os = seq(2017,2021))
predict(RPesados, nuevos.a�os)




#Vamos a realizar ahora lo mismo con otra Autopista. La AP-9 En una estac�on de aforo de santiago de compostela.
#Directamente utilizaremos los datos de los ultimos 4 a�os.

DatosAP9 <- read.table(file="ap9-4a�os.csv",sep=";",dec=",",
                    header = TRUE)
names(DatosAP9)

cor(DatosAP9)


par(mfrow=c(1,1))

RTotal2 <- lm(Pesado ~ A�os, data = DatosAP9)
summary(RTotal2)

plot(DatosAP9$A�os, DatosAP9$Pesado, xlab = "A�os", ylab = "Pesados",main="Regresi�n lineal IMD ligeros en funci�n de los a�os")
abline(RTotal2)


nuevos.a�os <- data.frame(A�os = seq(2017,2021))
predict(RTotal2, nuevos.a�os)

confint(RTotal2)

confint(RTotal2, level = 0.9)

nuevos.a�os <- data.frame(A�os = seq(2012, 2021))
# Grafico de dispersion y recta
plot(DatosAP9$A�os, DatosAP9$Pesado, xlab = "A�os", ylab = "Pesados", xlim=c(2012, 2021),
     ylim=c(4000, 8000),main="Regresi�n lineal y estimaci�n total vehiculos (AP9 SC)")
abline(RTotal2)

# Intervalos de confianza.
ic <- predict(RTotal2, nuevos.a�os, interval = "confidence")
lines(nuevos.a�os$A�os, ic[, 2], lty = 5)
lines(nuevos.a�os$A�os, ic[, 3], lty = 5)

# Intervalos de prediccion.
ic <- predict(RTotal2, nuevos.a�os, interval = "prediction")
lines(nuevos.a�os$A�os, ic[, 2], lty = 5, col = "red")
lines(nuevos.a�os$A�os, ic[, 3], lty = 5, col = "red")




par(mfrow=c(2,1)) 
#deflexion AP7
dEFMAX <- read.table(file="ap7-2000-2021.csv",sep=";",dec=",",
                     header = TRUE)

plot(dEFMAX$N, dEFMAX$Er, xlab = "N", ylab = "Er",main="Regresi�n lineal y estimaci�n vehiculos pesados (AP7 tarragona)")

plot(dEFMAX$A�os, dEFMAX$Er, xlab = "A�os", ylab = "Er", 
     main="Plot Er en funci�n de los a�os.")
plot(dEFMAX$A�os, dEFMAX$Pesados, xlab = "A�os", ylab = "Pesados",main="Plot IMD pesados/a�os (AP7 tarragona)")


#deflexion AP9
dEFMAX2 <- read.table(file="ap9-2000-2021.csv",sep=";",dec=",",
                      header = TRUE)

plot(dEFMAX2$N, dEFMAX2$Er, xlab = "N", ylab = "Er",main="Regresi�n lineal y estimaci�n vehiculos pesados (AP9 SC)")
plot(dEFMAX2$A�os, dEFMAX2$Er, xlab = "A�os", ylab = "Er", 
     main="Plot Er en funci�n de los a�os AP9.")
plot(dEFMAX2$A�os, dEFMAX2$Pesados, xlab = "A�os", ylab = "Pesados",main="Plot IMD pesados/a�os (AP9 SC)")

cluster <- hclust(dist(dEFMAX))
plot(cluster)
rect.hclust(cluster,k=4, border="red")



cluster2 <- hclust(dist(dEFMAX2))
plot(cluster2)
rect.hclust(cluster,k=5, border="red")

