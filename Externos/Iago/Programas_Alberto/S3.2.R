altura <- c(178, 181, 168, 183, 164, 181, 174, 176, 174, 176, 181, 168, 164, 174, 171)
altura

peso <- c(82, 89, 68, 91, 65, 80, 79, 81, 80, 79, 82, 69, 67, 80, 78)
peso

plot(peso ~ altura, pch = 16, col = 'red', cex = 1.5) # "pch" le indica al programa el tipo de punto que queremos (16 es un círculo relleno)

modelo1 = lm(peso ~ altura) # Hacemos la regresión. Con esto inicializado, podremos pedirle a R que nos de datos de la regresión
modelo1

modelo1$coefficients # Nos devuelve la pendiente y el corte con el eje y
abline(modelo1, col = "blue", lwd = 5) # Añade la recta de regresión al gráfico

summary(modelo1) # Pedimos más datos de la regresión. En penúltima línea -> R^2
cor(altura, peso) # Calculamos con mayor precisión el coef R
cor(altura, peso)^2 # Elevamos a 2 el coef R para conseguir R^2 con mayor precisión

# Opción 1:
predict(modelo1,data.frame(altura = 173)) # Sustituimos la x por 173 en la regresión a.k.a. predecimos cual debería ser su peso según la modelización

# Opción 2:
prediccion = modelo1$coefficients[2]*173+modelo1$coefficients[1] #  Substituimos a y = m*x+b
prediccion

points(173,prediccion, pch = 20, cex = 4, col = "purple") # Añadimos el punto que hemos predicho

# ¿Y si en vez de una altura nos dieran un peso?
prediccion2 = (72-modelo1$coefficients[1])/modelo1$coefficients[2]
prediccion2

################################################################################
dim(anscombe)

x1 <- c(anscombe$x1)
y1 <- c(anscombe$y1)
x2 <- c(anscombe$x2)
y2 <- c(anscombe$y2)
x3 <- c(anscombe$x3)
y3 <- c(anscombe$y3)
x4 <- c(anscombe$x4)
y4 <- c(anscombe$y4)

modelox1 = lm(y1 ~ x1)
modelox1$coefficients
cor(x1, y1)
cor(x1, y1)^2
predict(modelox1,data.frame(x1 = 8.5))

modelox2 = lm(y2 ~ x2)
modelox2$coefficients
cor(x2, y2)
cor(x2, y2)^2
predict(modelox2,data.frame(x2 = 8.5))

modelox3 = lm(y3 ~ x3)
modelox3$coefficients
cor(x3, y3)
cor(x3, y3)^2
predict(modelox3,data.frame(x3 = 8.5))

modelox4 = lm(y4 ~ x4)
modelox4$coefficients
cor(x4, y4)
cor(x4, y4)^2
predict(modelox4,data.frame(x4 = 8.5))

plot(y1 ~ x1, pch = 16, col = 'red', cex = 1.5)
plot(y2 ~ x2, pch = 16, col = 'green', cex = 1.5)
plot(y3 ~ x3, pch = 16, col = 'blue', cex = 1.5)
plot(y4 ~ x4, pch = 16, col = 'orange', cex = 1.5)

################################################################################
lluvia <- c(97,27,93,175,38,192,28,182,61,77)
incendios <- c(521,863,712,163,138,811,534,442,963,313)
plot(incendios ~ lluvia, pch = 16, col = 'red', cex = 1.5)
modelometeo = lm(incendios ~ lluvia)
predict(modelometeo,data.frame(lluvia = 120))

################################################################################
tiempo <- c(ester$t)

concentracion <- c(ester$conc)

modelotconc = lm(concentracion ~ tiempo)
predict(modelotconc,data.frame(tiempo = 70))

plot(ester$conc~ester$t,pch=16,col="red",cex=1.5) # Observamos que no se trata de una regresión lineal, más bien tiene forma log tendremos que transformarla
plot(log(ester$conc)~ester$t,pch=16,col="red",cex=1.5)
# A partir del cambio, calcularemos el valor de la concentración a los 70 segundos, pero no podremos darlo como válido porque ese no sería el resultado para nuestro gráfico original
model_log <- lm(log(ester$conc)~ester$t)
prediccionw = model_log$coefficients[2]*70+model_log$coefficients[1]
prediccionw
exp(prediccionw) # Para contrarrestar el logaritmo que hemos usado antes para linealizar nuestros datos
