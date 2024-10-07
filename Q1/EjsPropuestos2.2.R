misdades<- read.table(file="DatosCoches.txt", header=TRUE, dec=",", na.strings='NA')
read.table(file="DatosCoches.txt", header=TRUE, dec=",", na.strings='NA')
#Tabla de frecuencias
ni<- table(misdades$Origin) #cuantas veces sale cada cosa
fi<- ni/sum(ni) #Frecuencias relativas
Fi<- cumsum(fi) #suma acumulativa-> Frecuencia acumulada
Ni<- cumsum(ni) #n acumulada
cbind(ni, fi, Ni, Fi)

#Tabla de frecuencias
misdades$Acceleration<- as.numeric(as.character(misdades$Acceleration))
cortado<- cut(misdades$Acceleration, breaks=c(8, 11, 14, 17, 20, 23, 26), right= FALSE)#[)
ni2<- table(cortado) #cuantas veces sale cada cosa
fi2<- ni2/sum(ni2) #Frecuencias relativas
Fi2<- cumsum(fi2) #suma acumulativa-> Frecuencia acumulada
Ni2<- cumsum(ni2) #n acumulada
cbind(ni2, fi2, Ni2, Fi2)

#Tabla de frecuencias
misdades$MPG<- as.numeric(as.character(misdades$MPG))

cortado3<- cut(misdades$MPG, breaks=c(5, 15, 25, 35, 45), right= TRUE)# (]
cortado3
ni3<- table(cortado3) #cuantas veces sale cada cosa
fi3<- ni3/sum(ni3) #Frecuencias relativas
Fi3<- cumsum(fi3) #suma acumulativa-> Frecuencia acumulada
Ni3<- cumsum(ni3) #n acumulada
cbind(ni3, fi3, Ni3, Fi3)

hist(misdades$Displacement, breaks=50)
muestras_200_250 <- sum(misdades$Displacement >= 200 & misdades$Displacement <= 250, na.rm = TRUE)
muestras_200_250

usa_cars <- misdades[misdades$Origin=="USA", ]
mean_mpg_usa<- mean(usa_cars$MPG, na.rm= TRUE)
mean_mpg_usa
misdades$Cylinders<-as.numeric(as.character(misdades$Cylinders))
cil8<- misdades[misdades$Cylinders==8,]
median(cil8$Weigth, na.rm=TRUE)

