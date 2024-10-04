# INICIALIZAR VARIABLES:

x = 3
x <- 3

################################################################################
# INICIALIZAR VECTORES:

vector <- c(1,2,3)

################################################################################
# BASES DE DATOS:

# A veces las bases de datos forman parte de R y solo tenemos que llamarlas:

data("") # Entre las comillas escribimos el nombre de las base de datos.

# Si importamos una base de datos con sus respectivas propiedades, si escribimos
# el nombre de la base de datos $ el nombre de la propiedad, estamos pidiendo a
# R que nos devuelva un vector compuesto por todos los valores de esa propiedad.

# Si añadimos al "nombre base de datos"$"nombre propiedad" unos [] con un número
# estamos pidiendo que nos devuelva un valor concreto de la propiedad.

# Si queremos conocer el número de observaciones y de variables de la base de
# datos: dim("nombre base de datos"). Nos devolverá 2 números: el primero es el
# número de observaciones y el segundo el número de variables.

dim()

################################################################################
# TABLA DE FRECUENCIAS:

ni <- table(); ni # Número de veces que aparece cada dato.
fi <- ni/length(); fi # Frecuencias.
# En los paréntesis anteriores pondremos el listado de datos de los que estamos
# trazando su tabla de frecuencias.

Ni <- cumsum(ni); Ni # Número acumulado de veces que aparece cada dato
Fi <- cumsum(fi); Fi # Frecuencias acumuladas.

# También se pueden hacer tablas de frecuencia por intervalos:

interdata = # Ponemos el conjunto de datos que queramos agrupar en intervalos.
breaks = seq(,,) # seq("de dónde", "hasta dónde", "tamaño intervalos")
datos_intervalos = cut(interdata, breaks, right = FALSE)

interni = table(datos_intervalos); interni
interfi = table(datos_intervalos)/length(datos_intervalos); interfi
interNi = cumsum(interni); interNi
interFi = cumsum(interfi); interFi

################################################################################
# DIAGRAMAS:

# Tallos y hojas:
stem() # En los paréntesis pondremos el conjunto de datos del que queremos hacer
# el diagrama.

# De puntos:
dotchart([]) # dotchart("conjunto de datos"["propiedad que se debe cumplir"])

# De barras:
barplot(table()) # En los paréntesis de "table" ponemos el conjunto de datos que
# nos interese.

# De "quesitos":
pie(table()) # Igual que antes.

# Box-plot:
boxplot() # Podemos poner el nombre de una variable concreta y nos dará su Box-
# plot, pero si ponemos el nombre de la base de datos nos devolverá todos los
# Box-plots de todas las variables.

# Importante destacar también que no se puede hacer un Box-plot con variables
# no numéricas.

# Histograma:
h = hist() 
h$breaks # R nos devuelve los intervalos de cada barra.
h$counts # R nos devuelve la altura de cada barra.

################################################################################
# DATOS ESTADÍSTICOS DE INTERÉS:

# Media aritmética:
mean()

# Mediana:
median()

# Cuartiles:
quantile(,) # quantile("nombre variable","porcentaje que nos interese"). Ej: Q1 
# de un conjunto de datos "datos" -> quantile(datos,0.25)

# Rango intercuartílico:
IQR()

# Desviación típica:
sd()

# Varianza:
var()

################################################################################
# REGRESIÓN LINEAL:

variable_independiente <- c()
variable_dependiente <- c()

plot(variable_dependiente ~ variable_independiente, pch = 16, col = 'red', cex = 1.5)
# Representa de color rojo los datos. "pch" le indica al programa el tipo de 
# punto que queremos (16 es un círculo relleno).

modelo1 = lm(variable_dependiente ~ variable_independiente) # Modelizamos. Con 
# ellos, podremos pedir a R que nos diga el valor de los elementos de la
# progresión, o que incluso nos haga predicciones.

modelo1$coefficients # Nos devuelve el corte con el eje "y" y la pendiente 
# respectivamente.

abline(modelo1, col = "blue", lwd = 5) # Añade la recta de regresión al gráfico.

summary(modelo1) # Pedimos más datos de la regresión. En penúltima línea -> R^2.
# Nos dan resultados correctos pero con pocos decimales.

cor(altura, peso) # Calculamos con mayor precisión el coef R
cor(altura, peso)^2 # Elevamos a 2 el coef R para conseguir R^2 con mayor 
# precisión.

# Predicciones en base a las regresiones lineales:
  
  # CUANDO CONOCEMOS "X" Y QUEREMOS SABER "Y":

  # Opción 1:
predict(modelo1,data.frame(variable_independiente = )) # Ponemos después del "="
# el valor de "x" del cual queremos conseguir "y", según nuestra modelización.

  # Opción 2:
prediccion1 = modelo1$coefficients[2]*+modelo1$coefficients[1] #  Substituimos
# directamente en la fórmula: "y = m*x+b". Pondremos nuestro valor conocido 
# entre "*" y "+".

  # CUANDO CONOCEMOS "Y" Y QUEREMOS SABER "X":
prediccion2 = (-modelo1$coefficients[1])/modelo1$coefficients[2] # Pondremos
# nuestro valor conocido entre "(" y "-".

# ¿Cómo añadimos las predicciones a nuestro gráfico?
points(,prediccion1, pch = 20, cex = 4, col = "purple") # Antes de la coma 
# ponemos el valor de "x" a partir del cual se ha hecho la predicción.

################################################################################
# OTRAS OBSERVACIONES A TENER EN CUENTA:
###
# Si la regresión no es del todo lineal, la podemos linealizar. Por ejemplo:
model_log <- lm(log(variable_dependiente)~variable_independiente) # Planteamos
# una regresión logarítmica. Para ello hacemos el logaritmo de la variable
# depediente.
prediccionw = model_log$coefficients[2]*+model_log$coefficients[1] # Haríamos
# una predicción cualquiera de esta forma, pero faltará eliminar el cambio que
# ha provocado el logaritmo.
exp(prediccionw)

###
# Cuando la base de datos tiene datos "NaN" debemos indicarlo cuando escribamos
# la función. Para ello pondremos como último parámetro del paréntesis: 
na.rm = TRUE
# Además, como estaremos ignorando las variables que valgan NaN, deberemos tener
# en cuenta de que el número de datos a analizar se reducirá.

###
# Cuando queramos ver qué variables tienen un valor concreto:
a <- 
which(a %in% c()) # Pondremos en "c()" el valor que estemos buscando. Si se
# trata de un carácter, lo entrecomillaremos.