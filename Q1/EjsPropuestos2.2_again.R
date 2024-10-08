
#Tabla frecuencias Origin
ni<- table(coches$Origin) #cuantas veces sale cada cosa
fi<- ni/sum(ni) #Frecuencias relativas
Fi<- cumsum(fi) #suma acumulativa-> Frecuencia acumulada
Ni<- cumsum(ni) #n acumulada
cbind(ni, fi, Ni, Fi)

#Tabla frequencias Acceleration

cortado2<- cut(na.omit(coches$Acceleration), breaks= c(8, 11, 14, 17, 20, 23, 26), right=FALSE)
ni2<- table(cortado2) #cuantas veces sale cada cosa
fi2<- ni2/sum(ni2) #Frecuencias relativas
Fi2<- cumsum(fi2) #suma acumulativa-> Frecuencia acumulada
Ni2<- cumsum(ni2) #n acumulada
cbind(ni2, fi2, Ni2, Fi2)

#Tabla frecuencias MPG
new_breaks= seq(5, 45, by=10)
cortado3<- cut(na.omit(coches$MPG), breaks=new_breaks, right=TRUE)
ni3<- table(cortado3) #cuantas veces sale cada cosa
fi3<- ni3/sum(ni3) #Frecuencias relativas
Fi3<- cumsum(fi3) #suma acumulativa-> Frecuencia acumulada
Ni3<- cumsum(ni3) #n acumulada
cbind(ni3, fi3, Ni3, Fi3)

sort(coches$Displacement)
seq2<- seq(85, 455, by=50)
hist(na.omit(coches$Displacement))

boxplot(na.omit(coches$Acceleration))
boxplot(na.omit(coches$Displacement))
boxplot(na.omit(coches$MPG))        
boxplot(na.omit(coches$Mfg))
boxplot(na.omit(coches$Weight))

mean(na.omit(coches$MPG[coches$Origin == "USA    "]))
median(na.omit(coches$Weight[coches$Cylinders==8]))
quantile(na.omit(coches$Displacement))
N<-na.omit(length(coches$Horsepower))
sqrt((N-1)/N)*sd(coches$Horsepower, na.rm=TRUE)

sd(coches$Horsepower, na.rm=TRUE)
pie(table(coches$Cylinders))
