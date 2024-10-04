attach(notas)
ni = table(notas$S3)
fi = table(notas$S3)/length(notas$S3)
Ni = cumsum(ni)
Fi = cumsum(fi)

Tabla = cbind(ni,Ni,fi,Fi)
Tabla
