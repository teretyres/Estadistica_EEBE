# Sesion 2:
2+2
# Para inicializar objetos o bien "x = 3" o "x <- 3"
x = 2
y <- 3
x + y
# Vectores: v <- c(3,4,5)
v <- c(3,4,5)
# data.frame permite combinar distintas tipologías de datos

data("stackloss")
head(stackloss)
# Si se añade $ y [] nos mostrará solo la variable o espacio de la tabla
# que nos interese:
stackloss$Air.Flow[5]

# Número de observaciones, número de variables
dim(stackloss)

# Tabla frecuencias:
ni <- table(stackloss$Air.Flow); ni
fi <- ni/length(stackloss$Air.Flow); fi
Ni <- cumsum(ni); Ni
Fi <- cumsum(fi); Fi

# Diagrama de tallos y hojas:
stem(stackloss$Air.Flow)

# Diagrama de puntos:
dotchart(stackloss$Water.Temp[stackloss$Air.Flow==58]) # Traza el diagrama de 
# aquellos puntos que cumplen la condición

# Diagrama de barras:
barplot(table(stackloss$Acid.Conc.)) # NECESITA table()

# Diagrama "Quesitos"
pie(table(stackloss$Air.Flow))

#Box-Plot:
boxplot(stackloss$Air.Flow) # Para el Box-Plot de una variable específica
boxplot(stackloss) # Traza los box-plots de todas las variables

# media aritmetica, mediana, cuartiles
mean(stackloss$Air.Flow)
median(stackloss$Water.Temp)
quantile(stackloss$Acid.Conc.,0.25)
quantile(stackloss$stack.loss,0.18)

# Rango Intercuartílico:
IQR(stackloss$Water.Temp)

# Desviación típica:
sd(stackloss$Acid.Conc.)

# Varianza:
var(stackloss$Air.Flow)


head(DatosCoches)
ni <- table(DatosCoches$Origin)
ni
fi <- ni/length(DatosCoches$Origin)
fi
table(DatosCoches$Acceleration)

DatosCoches$Model[4]

dim(DatosCoches)

boxplot(DatosCoches$Acceleration) # No se puede hacer Box-Plot con carácteres
boxplot(DatosCoches$Displacement)
boxplot(DatosCoches$MPG)
boxplot(DatosCoches$Mfg)
boxplot(DatosCoches$Weigth)

quantile(DatosCoches$Displacement,0.25)

sd(DatosCoches$Horsepower)

# Tabla de frecuencias con datos agrupados:

aceleracion = DatosCoches$Acceleration
aceleracion
breaks = seq(8,26,by=3)
breaks

aceleracion_a = cut(aceleracion, breaks, right = FALSE)

ni = table(aceleracion_a); ni
fi = table(aceleracion_a)/length(aceleracion_a); fi
Ni = cumsum(ni); Ni
Fi = cumsum(fi); Fi

# Otro ej de este tipo de tablas:

Mpg = DatosCoches$MPG
Mpg
breaks = seq(5,45,by=10)
breaks

Mpg_a = cut(Mpg, breaks, right = TRUE, Left = FALSE)

ni = table(Mpg_a); ni
fi = table(Mpg_a)/(length(Mpg_a)-6); fi
Ni = cumsum(ni); Ni
Fi = cumsum(fi); Fi

Tabla_Frec = cbind(ni,fi,Ni,Fi); Tabla_Frec

###############################################

pie(table(DatosCoches$Cylinders))

N = length(DatosCoches$Horsepower)
sqrt(((N-1)-1/(N-1)*sd(DatosCoches$Horsepower, na.rm = TRUE))) # Como hay un
# dato de valor NaN, N ya no es 100 sino 99

h=hist(DatosCoches$Displacement); h
h$breaks
h$counts

x <- DatosCoches$Origin
which(x %in% c("USA"))

# Media de MPG para coches de USA ignorando los valores NaN:
mean(DatosCoches$MPG[which(x %in% c("USA"))], na.rm = TRUE)

y <- DatosCoches$Cylinders
median(DatosCoches$Weigth[which(y %in% c(8))], na.rm = TRUE)
