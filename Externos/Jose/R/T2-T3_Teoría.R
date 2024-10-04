Datos = read.table("C:/Users/joser/OneDrive/Desktop/ª/Uni/Q3/ES/R/Q1/coches.txt" #el \t es tab.
                   ,header=TRUE,sep="\t",na.strings="NA",dec=".") #Depende de cómo estén en el .txt
mean(Datos$MPG, na.rm=TRUE)
plot(Datos$Horsepower,Datos$Displacement)
plot(Datos$Horsepower,Datos$Displacement,pch=16,col='red',cex=1.5)
# col == color, pch == tipo de punto, cex == tamaño de pto, lwd == grosor líneas.
pairs(Datos[3:9], upper.panel=NULL,pch=16,col='green') #Diagramas de dispersión de TODAS las variables.

#--------------------------------------
#Modelo lineal de los mínimos cuadrados
#--------------------------------------

#lm(y~x) para encontrar y = mx + b
model1 = lm(Datos$Horsepower~Datos$Displacement); model1 #model1$coef[1] y model1$coef[2]
model1$coef[1]
coef(model1)[1]
#--------------------------------------
#Añadir la recta de regresión
#--------------------------------------

plot(Datos$Displacement,Datos$Horsepower,pch=16,col='red',cex=1.5)
abline(model1,col='blue',lwd=5)

#------------------------------------------------
#Coef de determinación (R) y de correlación (R2)
#------------------------------------------------

summary(model1)
#Call:
#  lm(formula = Horsepower ~ Displacement)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-49.543 -11.425  -0.704   9.604  57.255 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
34.88703 -> b#(Intercept)  34.88703    3.96235   8.805 5.09e-14 ***
0.37062 -> m#  Displacement  0.37062    0.01677  22.094  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 18.63 on 97 degrees of freedom
#(1 observation deleted due to missingness)
0.8342 -> R^2 #Multiple R-squared:  0.8342,	Adjusted R-squared:  0.8325
#F-statistic: 488.2 on 1 and 97 DF,  p-value: < 2.2e-16
sqrt(0.8342) == 0.9133455 == R

cor(Datos$Horsepower,Datos$Displacement,use='na.or.complete') == R

#--------------------------------------
#Estimación de valores indeterminados
#--------------------------------------

f1 <- function(x) {model1$coef[1] + model1$coef[2]*(x)} #f1 = b + mx
Displacement_predict = 151
Horsepower_predict = f1(Displacement_predict); Horsepower_predict
Horsepower_pred=predict(model1,data.frame(Displacement=Displacement_predict)); #Otra manera, no sale
Horsepower_pred

#Graficar el punto estimado
plot(Displacement,Horsepower,pch=16,col='red',cex=1.5)
abline(model1,col="blue",lwd=5)
points(Displacement_predict,Horsepower_pred,col="black", pch=20,cex=4)

#--------------------------------------
#Regresión exponencial
#--------------------------------------

par(mfrow=c(1,2))
plot(Datos$Weight,Datos$MPG,xlab="Weight", ylab="MPG",col="red",pch=20,cex=1.5)
plot(Datos$Weight,log(Datos$MPG),xlab="Weight", ylab="log(MPG)",col="red",pch=20,cex=1.5)

#Obtenemos: ln(MPG)=4.3-0.0004033*Weight o que MPG=exp(4.3)*exp(0.0004033*Weight)
model2=lm(log(Datos$MPG)~Datos$Weight); summary(model2)

#Predecir los valores para los que no hay MPG (NaN)
obs = which(Datos$MPG=="NaN"); obs
Weight_pred = Datos$Weight[obs]; Weight_pred
Log_MPG_pred = predict(model2,data.frame(Weight=Weight_pred)); Log_MPG_pred
MPG_pred = exp(Log_MPG_pred); MPG_pred

#Resultado final
par(mfrow=c(1,2))
plot(Datos$Weight,log(Datos$MPG),xlab="Weight", ylab="log(MPG)",col="red",pch=20,cex=1.5)
abline(model2,col="blue",lwd=3)
points(Weight_pred,Log_MPG_pred, col = "black", pch= 20, cex = 2)
plot(Weight,MPG,xlab="Weight", ylab="MPG",col="red",pch=20,cex=1.5)
curve(exp(4.3)*exp(-0.0004033*x),add=T,col="blue",lwd=3)
points(Weight_pred,MPG_pred, col = "black", pch= 20, cex = 2)

#Tipos de gráficos

rm(list=ls()) #Borrar objetos
stem() #Diagrama de tallso y hojas
dotchart() #Gráfico de puntos
barplot() #Gráfico de barras
hist() #Histograma
pie() #Gráfico de sectores
boxplot(Datos$Horsepower) #Gráfico de caja guapo

