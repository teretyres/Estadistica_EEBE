
# Tabla de Frecuencias-----
TablaFrecuencia <- function(datos)
{
  datosTabla= datos
  ni=table(datosTabla)
  Ni = cumsum(ni)
  fi = table(datosTabla)/length(datosTabla)
  Fi = cumsum(fi)
  TablaFrec=cbind(ni,fi,Ni,Fi)
  return(TablaFrec)
}

# Tabla de Frecuencias Con Intervalos ----
TablaFrecuencia.Intervalos <- function(datos,inicial,final,saltos,DerechaIncluido)
{
  datosCut = datos
  breaks = seq(from=inicial,to=final,by=saltos)
  datosTabla= cut(datosCut,breaks,right = DerechaIncluido)
  ni=table(datosTabla)
  Ni = cumsum(ni)
  fi = table(datosTabla)/length(datosTabla)
  Fi = cumsum(fi)
  TablaFrec=cbind(ni,fi,Ni,Fi)
  return(TablaFrec)
}

# Cual cumple la condicion (de un data.frame o tabla) ----
CumpleCondicion <- function(datos,condicion)
{
  datos[which(condicion),]
}

# Desviacion estandar NO CORREGIDA-----
sd_NoCorregida <- function(datos)
{
  N= length(datos)
  calculo = sqrt((N-1)/N)*sd(datos)
  return (calculo)
}

# Varianza NO CORREGIDA-----
var.NoCorregida <- function(datos)
{
  N=length(datos)
  calculo = ((N-1)/N)*var(datos)
  return (calculo)
}

# moda-----
moda <- function(datos)
{
  tabla =table(datos)
  freq_ord=sort(tabla,decreasing = TRUE)
  resultado = names(freq_ord[1])
  return(resultado)
}

#predicciones minimos cuadrados-----
prediccion.mc.2 <- function(model,x)
{
  resultado=model$coef[1] + model$coef[2]*x
  return(resultado)
}

#predicciones minimos cuadrados log----
prediccion.mc.log <- function(model,x)
{
  resultado=model$coef[1] + model$coef[2]*x
return(exp(resultado))
}



