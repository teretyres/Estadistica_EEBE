  #4 - COMANDO PARA HACER UNA REPRESENTACION GRAFICA
  
  #SECTORES
  pie(table(data$nombrevariable))
  
  #TALLO Y HOJA
  stem(data$nombrevariable)
  
  #HISTOGRAMA
  hist(data$nombrevariable)
  
  #PUNTOS
  dotchart(data$nombrevariable)
  
  #BARRAS
  barplot(table(data$nombrevariable))