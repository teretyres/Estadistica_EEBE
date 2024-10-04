datos = c(1,5,3,1,2,3,4,5,1,4,2,4,4,5,1,4,2,4,2,2)
ni = table(datos)
fi = table(datos)/length(datos)
Ni = cumsum(ni)
Fi = cumsum(fi)
Tabla_Frecuencia = cbind(ni,fi,Ni,Fi)
Tabla_Frecuencia
barplot(table(datos))