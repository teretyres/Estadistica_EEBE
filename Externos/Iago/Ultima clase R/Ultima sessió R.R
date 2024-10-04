#Exercici 1 hipotesi
install.packages("TeachingDemos")
library(TeachingDemos)
x = c(23.3, 22.5, 21.9, 21.5, 19.9, 21.3, 21.7, 23.8, 22.6, 24.7)
varianza = 1.96
alfa = 0.05
limite = 23
#Ho:mu >= 23 o mu = 23
#H1:mu <23
#3. Criterio de decisión basado en el valor crítico o región de rechazo: 
#Rechazamos hipotesis si z_obs < z_critico
z_obs = (mean(x)-23)/(sqrt(varianza)/sqrt(10)); z_obs
z_critico = qnorm(alfa); z_critico
#4. Criterio de decisión basado en el p-valor: 
#Rechazamos hipotesis si p_valor < alfa
#5. Realización del contraste: 
#5.a 
mean(x)
sd(x)
#5.b
z_obs = (mean(x)-23)/(sqrt(varianza)/sqrt(10)); z_obs
p_valor = pnorm(z_obs); p_valor
#6. El resultado del contraste es: 
#segun z_obs, no rechazo Ho ya que z_obs >z_critico
#segun p_valor, no rechazo Ho ya que p_valor > alfa
#Conclusion, no rechazar Ho
#7. Conclusión:
#En este caso, no hay suficiente evidencia para rechazar la hipotesis nula.

#z.test insatantaneo, encontrar z_obs(z) y p_valor(p-value)
z.test(x,mu=23,stdev=sqrt(varianza),alternative = "less",sd = sqrt(1.96),10,conf.level = 0.95)



#Exercici 2
x = c(7.3, 7.1, 7.9, 7.0, 7.2)
n= length(x)
mu = 6.8 
alfa = 0.01
#1. Hipótesis del contraste: 
#Ho: mu = 6.8
#H1: mu > 6.8
#3. Criterio de decisión basado en el valor crítico o región de rechazo: 
#Rechazamos hipotesis si t_obs > t_critico
t_critico = qt(1-alfa,4); t_critico
t_obs = (mean(x)-6.8)/(sd(x)/sqrt(n)); t_obs
#4. Criterio de decisión basado en el p-valor: 
#Rechazamos hipotesis si p_valor < alfa
#5. Realización del contraste: 
#5.a
mean(x)
sd(x)
#5.b
t_obs = (mean(x)-6.8)/(sd(x)/sqrt(n)); t_obs
p_valor = 1-pt(t_obs,4); p_valor
#6. El resultado del contraste es: 
#6. El resultado del contraste es: 
#segun t_obs, no rechazo Ho ya que t_obs >t_critico
#segun p_valor, no rechazo Ho ya que p_valor > alfa
#Conclusion, no rechazar Ho
#7. Conclusión:
#En este caso, no hay suficiente evidencia para rechazar la hipotesis nula.

#t.test insatantaneo, encontrar z_obs(x) y p_valor(p-value)
t.test(x,mu=6.8,stdev=sd(x),alternative = "greater",sd = sd(x),5,conf.level = 0.99))
