#Modelo Regresión exponencial
d = c(1,2,4,5,8,10,11,14,16,20)
o = c(255,1500,2105,5050,1630,45320,58570,375800,1525640,2577000)
o = log(o)
plot(d,exp(o)) #Gráfica sin log (exponencial)
#plot(d,o) #Gráfica con log (una recta)
#model1 = lm(o~d) #Gráfica con log (una recta)
#abline(model1) #Gráfica con log (una recta)
xmed = sum(d)/length(d) #xmed es la media de x (d)
ymed = sum(o)/length(o)
x = d - xmed # x,y son lo que hay dentro del paréntesis
y = o - ymed
Up = x*y #Lo del numerando
Down = x^2 #Lo del dividendo
m = sum(Up)/sum(Down) #La pendiente (m o Beta)
b = ymed - m*xmed #Coeficiente b = (log(Alpha)) = ymed - m*xmed
#[log == ln]
Alpha = exp(b)
lm(o~d) #Nos da b, m -> lm(y~x)

#log(f) = Beta*g + b
#f(g) = Alpha*exp(Beta*g)

g = seq(0,20,1)
fg = Alpha*exp(m*g) #Curva en gráfica sin log
lines(g,fg)


#Cálculo de R^2 (coef de determinación) y R (coef de Pearson)
Abajo = y^2
fmed = sum(fg)/length(fg)
f = fg - fmed
Arriba = f^2
R2 = sum(Arriba)/sum(Abajo)
R = sqrt(R2)
#No me sale

abline() #Para añadir recta de regresión a un gráfico de dispersión
