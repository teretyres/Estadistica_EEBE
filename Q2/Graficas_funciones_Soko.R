#Graficas Discretas----
Graficas_Densidad_Disc<- function(ddistribucion,x)
{
  #Acuerdate de definir x como un intervalo antes i.e.(60:140)
  plot(x,ddistribucion,type="h",ylab="f(x)",xlab="X",col="red")
  points(x,ddistribucion)
  title(main = "Función de Densidad")
  Grafica <- recordPlot()
  return(Grafica)
}
Graficas_Distribucion_Disc<- function(pdistribucion,x,min=-1,max=600)
{
  #Acuerdate de definir x como un intervalo antes i.e.(60:140)
  lim_x = c(min,x,max)
  lim_y = c(0,pdistribucion,1)
  plot(lim_x,lim_y,type="s",ylab="f(x)",xlab="X",col="red")
  points(x,pdistribucion)
  title(main = "Función de Distribución")
  Grafica <- recordPlot()
  return(Grafica)
}

#Graficas Continuas----
#Preferiblemente utiliza el curve explicado en Metodos contínuos.R
Graficas_Densidad_Cont <- function(ddistribucion,x)
{
  #Acuerdate de definir x como un intervalo antes i.e.(60:140)
  plot(x,ddistribucion,type="l",ylab="f(x)",xlab="X",col="red")
  #points(x,ddistribucion)
  title(main = "Función de Densidad")
  Grafica <- recordPlot()
  return(Grafica)
  #curve(ddistribucion, xlim=c(min, max), col='red')
}



