attach(DatosCoches)
ni = table(Origin)
fi = table(Origin)/length(Origin)
Tabla_Origin = cbind(ni,fi)
Tabla_Origin

breaks = seq(8,26,by=3)

Intervals = cut(Acceleration, breaks, right=FALSE)

ni = table(Intervals)
fi = table(Intervals)/length(Intervals)
Ni = cumsum(ni)
Fi = cumsum(fi)
Tabla_Acc = cbind(ni,fi, Ni, Fi)
Tabla_Acc

breaks = seq(5,45,by=10)

Intervals = cut(MPG, breaks, right=FALSE)

ni = table(Intervals)
fi = table(Intervals)/length(Intervals)
Ni = cumsum(ni)
Fi = cumsum(fi)
Tabla_MPG = cbind(ni,fi, Ni, Fi)
Tabla_MPG

breaks = seq()