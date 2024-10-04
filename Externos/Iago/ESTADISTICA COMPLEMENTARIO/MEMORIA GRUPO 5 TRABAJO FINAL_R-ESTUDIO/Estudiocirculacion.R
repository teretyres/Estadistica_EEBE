install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)
#Vamos a realizar 3 regresiones lineales, la cantidad de vehiculos en funcion de los años, para poder realizar 
#una estimación.


#Empezamos con el Total de Vehiculos.
Datos <- read.table(file="ap7-2000-2016.csv",sep=";",dec=",",
                    header = TRUE)

cor(Datos)

pairs((Datos))

regresion <- lm(Total ~ Años, data = Datos)
summary(regresion)

plot(Datos$Años, Datos$Total, xlab = "Años", ylab = "Total",main="Regresión lineal IMD total en función de los años")
abline(regresion)

#El R^2 es de 0.3458, por lo que es un resultado no muy fiable. Observamos que la regresion lineal da una tendencia
#decreciente debido a la crisis española. Por lo que tomaremos los datos del 2012 al 2016.



plot(Datos$Años, Datos$Pesados, xlab = "Años", ylab = "Pesados",main="Regresión lineal IMD Pesados en función de los años")
qplot(Años, Pesados, data=Datos, geom=c("point", "smooth"), method="lm", formula= y ~ poly(x, 3),level=0.99)
#Vemos que una regresion de polinimica de grado 3 se ajusta bastante bien.

#Realizamos una regresion polinomica de tercer grado
LinearModel.1 <- lm(Pesados ~ Años +I(Años^2)+I(Años^3), data=Datos)
summary(LinearModel.1)
#no conseguimos realizar la polinomica de tercer grado el resultado es una de segundo grado.
LinearModel.2 <- lm(Pesados ~ Años +I(Años^2), data=Datos)
summary(LinearModel.2)

qplot(Años, Pesados, data=Datos, geom=c("point", "smooth"), method="lm", formula= y ~ poly(x, 2),level=0.99)

#este modelo no nos interesa para realizar estimaciones futuras debido a la pendiente que sale negativa.


Datos <- read.table(file="ap7-2000-2016.csv",sep=";",dec=",",
                      header = TRUE)
names(Datos)

regresion2 <- lm(Pesados ~ Años, data = Datos)
summary(regresion2)



plot(Datos$Años, Datos$Pesados, xlab = "Años", ylab = "Pesados",main="Regresión lineal IMD Pesados en función de los años")
abline(regresion2)
#El R^2 mejora bastante pero la observación es la misma. Vamos a tener que tomar los ultimos años para poder tener
#una estimación razonable.


#Y ahora con los ligeros.
Datos <- read.table(file="ap7-2000-2016.csv",sep=";",dec=",",
                      header = TRUE)

regresion3 <- lm(Ligeros ~ Años, data = Datos)
summary(regresion3)

plot(Datos$Años, Datos$Ligeros, xlab = "Años", ylab = "Ligeros",main="Regresión lineal IMD ligeros en función de los años")
abline(regresion3)
#El R^2 es de 0.1799 y la pendiente negativa.


#Eliminamos los datos pre y durante la crisis

#Empezamos con el Total de Vehiculos.
Datosap7 <- read.table(file="ap7-4años.csv",sep=";",dec=",",
                    header = TRUE)
names(Datosap7)

cor(Datosap7)

RTotal <- lm(Total ~ Años, data = Datosap7)
summary(RTotal)

plot(Datosap7$Años, Datosap7$Total, xlab = "Años", ylab = "Total",main="Regresión lineal IMD total en función de los años")
abline(RTotal)


nuevos.años <- data.frame(Años = seq(2017,2021))
predict(RTotal, nuevos.años)

confint(RTotal)

confint(RTotal, level = 0.9)

nuevos.años <- data.frame(Años = seq(2012, 2021))
# Grafico de dispersion y recta
plot(Datosap7$Años, Datosap7$Total, xlab = "Años", ylab = "Total", xlim=c(2012, 2021),
     ylim=c(30000, 50000),main="Regresión lineal y estimación total vehiculos (AP7 tarragona)")
abline(RTotal)

# Intervalos de confianza.
ic <- predict(RTotal, nuevos.años, interval = "confidence")
lines(nuevos.años$Años, ic[, 2], lty = 5)
lines(nuevos.años$Años, ic[, 3], lty = 5)

# Intervalos de prediccion.
ic <- predict(RTotal, nuevos.años, interval = "prediction")
lines(nuevos.años$Años, ic[, 2], lty = 5, col = "red")
lines(nuevos.años$Años, ic[, 3], lty = 5, col = "red")


#ahora con los vehiculos pesados
Datosap7 <- read.table(file="ap7-4años.csv",sep=";",dec=",",
                     header = TRUE)
names(Datosap7)

cor(Datosap7)


Linearmodel.2 <- lm(Pesados~ Años +I(Años^2), data=Datosap7)
summary(Linearmodel.2)

qplot(Años, Pesados, data=Datosap7, geom=c("point", "smooth"), method="lm", formula= y ~ poly(x, 2))

par(mfrow=c(2,2))


plot(Linearmodel.2)
#vemos que este modelo no se ajusta bien. por lo que seguimos con la regresion lineal simple.

par(mfrow=c(1,1))
RPesados <- lm(Pesados ~ Años, data = Datosap7)
summary(RPesados)

plot(Datosap7$Años, Datosap7$Pesados, xlab = "Años", ylab = "Pesados",main="Regresión lineal IMD Pesados en función de los años")
abline(RPesados)

par(mfrow=c(2,2))
plot(RPesados)



#realizamos las predicciones
nuevos.años <- data.frame(Años = seq(2012, 2021))
predict(Linearmodel.2, nuevos.años)
predict(RPesados, nuevos.años)

#podemos observar que cambian bastante los resultados segun el modelo utilizado

par(mfrow=c(1,1))



nuevos.años <- data.frame(Años = seq(2012, 2021))
# Grafico de dispersion y recta
plot(Datosap7$Años, Datosap7$Pesados, xlab = "Años", ylab = "Pesados", xlim=c(2012, 2021),
     ylim=c(3000, 8000),main="Regresión lineal y estimación vehiculos pesados (AP7 tarragona)")
abline(RPesados)

# Intervalos de confianza.
ic <- predict(RPesados, nuevos.años, interval = "confidence")
lines(nuevos.años$Años, ic[, 2], lty = 5)
lines(nuevos.años$Años, ic[, 3], lty = 5)

# Intervalos de prediccion
ic <- predict(RPesados, nuevos.años, interval = "prediction")
lines(nuevos.años$Años, ic[, 2], lty = 5, col = "red")
lines(nuevos.años$Años, ic[, 3], lty = 5, col = "red")

#Podemos entonces obtener una estimación de la cantidad de vehiculos que pasaran en los años a venir.


nuevos.años <- data.frame(Años = seq(2017,2021))
predict(RPesados, nuevos.años)




#Vamos a realizar ahora lo mismo con otra Autopista. La AP-9 En una estacíon de aforo de santiago de compostela.
#Directamente utilizaremos los datos de los ultimos 4 años.

DatosAP9 <- read.table(file="ap9-4años.csv",sep=";",dec=",",
                    header = TRUE)
names(DatosAP9)

cor(DatosAP9)


par(mfrow=c(1,1))

RTotal2 <- lm(Pesado ~ Años, data = DatosAP9)
summary(RTotal2)

plot(DatosAP9$Años, DatosAP9$Pesado, xlab = "Años", ylab = "Pesados",main="Regresión lineal IMD ligeros en función de los años")
abline(RTotal2)


nuevos.años <- data.frame(Años = seq(2017,2021))
predict(RTotal2, nuevos.años)

confint(RTotal2)

confint(RTotal2, level = 0.9)

nuevos.años <- data.frame(Años = seq(2012, 2021))
# Grafico de dispersion y recta
plot(DatosAP9$Años, DatosAP9$Pesado, xlab = "Años", ylab = "Pesados", xlim=c(2012, 2021),
     ylim=c(4000, 8000),main="Regresión lineal y estimación total vehiculos (AP9 SC)")
abline(RTotal2)

# Intervalos de confianza.
ic <- predict(RTotal2, nuevos.años, interval = "confidence")
lines(nuevos.años$Años, ic[, 2], lty = 5)
lines(nuevos.años$Años, ic[, 3], lty = 5)

# Intervalos de prediccion.
ic <- predict(RTotal2, nuevos.años, interval = "prediction")
lines(nuevos.años$Años, ic[, 2], lty = 5, col = "red")
lines(nuevos.años$Años, ic[, 3], lty = 5, col = "red")




par(mfrow=c(2,1)) 
#deflexion AP7
dEFMAX <- read.table(file="ap7-2000-2021.csv",sep=";",dec=",",
                     header = TRUE)

plot(dEFMAX$N, dEFMAX$Er, xlab = "N", ylab = "Er",main="Regresión lineal y estimación vehiculos pesados (AP7 tarragona)")

plot(dEFMAX$Años, dEFMAX$Er, xlab = "Años", ylab = "Er", 
     main="Plot Er en función de los años.")
plot(dEFMAX$Años, dEFMAX$Pesados, xlab = "Años", ylab = "Pesados",main="Plot IMD pesados/años (AP7 tarragona)")


#deflexion AP9
dEFMAX2 <- read.table(file="ap9-2000-2021.csv",sep=";",dec=",",
                      header = TRUE)

plot(dEFMAX2$N, dEFMAX2$Er, xlab = "N", ylab = "Er",main="Regresión lineal y estimación vehiculos pesados (AP9 SC)")
plot(dEFMAX2$Años, dEFMAX2$Er, xlab = "Años", ylab = "Er", 
     main="Plot Er en función de los años AP9.")
plot(dEFMAX2$Años, dEFMAX2$Pesados, xlab = "Años", ylab = "Pesados",main="Plot IMD pesados/años (AP9 SC)")

cluster <- hclust(dist(dEFMAX))
plot(cluster)
rect.hclust(cluster,k=4, border="red")



cluster2 <- hclust(dist(dEFMAX2))
plot(cluster2)
rect.hclust(cluster,k=5, border="red")

