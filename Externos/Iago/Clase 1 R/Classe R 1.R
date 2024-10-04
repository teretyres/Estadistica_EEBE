#Exercicis sessi? 1
#Q?estionari sessi? 2
data("stackloss")
head(stackloss)
stackloss$Air.Flow
stackloss$Air.Flow[5]
stackloss$Air.Flow[c(5,9,20)]
dim(stackloss)
ni <- table(stackloss$Air.Flow)
fi <- ni/length(stackloss$Air.Flow)
Ni <- cumsum(ni); Ni
Fi <- cumsum(fi); Fi
stem(stackloss$Air.Flow)
stackloss$Water.Temp
table(stackloss$Water.Temp)
dotchart(stackloss$Water.Temp[stackloss$Air.Flow==58])
table(stackloss$Acid.Conc.)
barplot(table(stackloss$Acid.Conc.))
pie(table(stackloss$Air.Flow))        
boxplot(stackloss$Water.Temp)
boxplot(stackloss$Air.Flow)
boxplot(stackloss$Acid.Conc.)
boxplot(stackloss$stack.loss)
mean(stackloss$Air.Flow) #mitjana
median(stackloss$Water.Temp) #mediana
quantile(stackloss$Acid.Conc.) #Trobar quantils
quantile(stackloss$stack.loss, 0.18)
IQR(stackloss$Water.Temp)
sd(stackloss$Acid.Conc.)
var(stackloss$Air.Flow)
#Exercici 2
#Taula Origen
table(DatosCoches$Origin)
ni <- table(DatosCoches$Origin); ni
fi <- ni/length(DatosCoches$Origin); fi
#Taula Acceleració
dades = c(12.0, 11.5, 11.0, 12.0, 10.5, 10.0,  9.0,  8.5, 10.0,  8.5, 17.5, 11.5, 11.0, 10.5, 11.0, 10.0,  8.0,  8.0,  9.5, 10.0, 15.0, 15.5, 15.5,
          16.0, 14.5, 20.5, 17.5, 14.5, 17.5, 12.5, 15.0, 14.0, 15.0, 13.5, 18.5, 15.5, 16.9, 14.9, 17.7, 15.3, 13.0, 13.0, 13.9, 12.8, 15.4, 14.5,
          17.6, 17.6, 22.2, 22.1, 14.2, 17.4, 17.7, 21.0, 16.2, 17.8, 12.2, 17.0, 16.4, 13.6, 15.7, 13.2, 21.9, 15.5, 16.7, 12.1, 12.0, 15.0, 14.0,
          19.6, 18.6, 18.0, 16.2, 16.0, 18.0, 16.4, 20.5, 15.3, 18.2, 17.6, 14.7, 17.3, 14.5, 14.5, 16.9, 15.0, 15.7, 16.2, 16.4, 17.0, 14.5, 14.7,
          13.9, 13.0, 17.3, 15.6, 24.6, 11.6, 18.6, 19.4);
breaks = seq(8,26,by=3);
dades2 = cut(dades, breaks, right=FALSE);
head(dades2, n=6);
ni <- table(dades2); ni
fi <- ni/length(dades); fi
Ni <- cumsum(ni); Ni
Fi <- cumsum(fi); Fi
#Taula MPG
dades = c(18.0, 15.0, 18.0, 16.0, 17.0, 15.0, 14.0, 14.0, 14.0, 15.0, 15.0, 14.0, 15.0, 14.0, 24.0, 22.0, 18.0,
          21.0, 27.0, 26.0, 25.0, 24.0, 25.0, 26.0, 21.0, 10.0, 10.0, 11.0,  9.0, 28.0, 25.0, 25.0, 26.0, 27.0, 17.5, 16.0, 15.5, 14.5, 22.0, 22.0,
          24.0, 22.5, 29.0, 24.5, 29.0, 33.0, 20.0, 18.0, 18.5, 17.5, 29.5, 32.0, 28.0, 26.5, 20.0, 13.0, 19.0, 19.0, 16.5, 16.5, 13.0, 13.0, 13.0,
          28.0, 27.0, 34.0, 31.0, 29.0, 27.0, 24.0, 23.0, 36.0, 37.0, 31.0, 38.0, 36.0, 36.0, 36.0, 34.0, 38.0, 32.0, 38.0, 25.0, 38.0, 26.0, 22.0,
          32.0, 36.0, 27.0, 27.0, 44.0, 32.0, 28.0, 31.0);
breaks = seq(5,45,by=10);
dades2 = cut(dades, breaks, left=FALSE);
head(dades2, n=4);
ni <- table(dades2); ni
fi <- ni/length(dades); fi
Ni <- cumsum(ni); Ni
Fi <- cumsum(fi); Fi
hist(dades) #Histograma
#Gràfic de caixes
boxplot(DatosCoches$Acceleration);
boxplot(DatosCoches$Cylinders);
boxplot(DatosCoches$Displacement);
boxplot(DatosCoches$Horsepower);
boxplot(DatosCoches$MPG);
boxplot(DatosCoches$Mfg);
boxplot(DatosCoches$Weigth)
#¿Cuál es la media de las millas por galón (MPG) de los coches fabricados en Estados Unidos?

#¿Cuál es la mediana del peso de los coches que tienen 8 cilindros? 


#¿Cuál es el primer cuartil del desplazamiento?
quantile(DatosCoches$Displacement,0.25)
#¿Cuál es la desviación típica sin corregir de la potencia?
dades = c(130, 165, 150, 150, 140, 198, 220, 215, 225, 190, 115, 165, 153, 175, 175, 170, 160, 140, 150, 225,  95,  95,  97,  85,  88,  46,  87,  90,  95,
113,  90, 215, 200, 210, 193,  86,  81,  92,  79,  83, 140, 150, 120, 152, 100, 105,  81,  90,  52,  60,  70,  53, 100,  78, 110,  95,  71,  70,
75,  72, 102, 150,  88, 108, 120, 180, 145, 130, 150,  88,  88,  88,  85,  84,  90,  92, 74,  68, 68,  63,  70,  88,  75,  70,  67,  67,
67, 110,  85,  92, 112,  96,  84,  90,  86,  52,  84,  79,  82);
sqrt((99-1)/99)*sd(dades)
#Realiza el gráfico de sectores del número de cilindros.¿Cuál es el número que menos se repite?
pie(table(DatosCoches$Cylinders))