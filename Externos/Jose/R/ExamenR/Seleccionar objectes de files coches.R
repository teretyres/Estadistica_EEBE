attach(DatosCoches)
coches_USA = DatosCoches[DatosCoches$Origin=="USA",]
coches_USA
mean(coches_USA$MPG, na.rm=TRUE)

coches_8cilindros = DatosCoches[DatosCoches$Cylinders==8,]
coches_8cilindros
median(coches_USA$Weight)