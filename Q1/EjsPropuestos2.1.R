#Codigo Ejercicios propuestos S2.1
misdades<- stackloss
stackloss

#Tabla de frecuencias
ni<- table(misdades$Air.Flow) #cuantas veces sale cada cosa
fi<- ni/sum(ni) #Frecuencias relativas
Fi<- cumsum(fi) #suma acumulativa-> Frecuencia acumulada
Ni<- cumsum(ni) #n acumulada
cbind(ni, fi, Ni, Fi)

stem(misdades$Air.Flow)
barplot(table(misdades$Acid.Conc.), horiz = TRUE)

#moda
freq<-table(misdades$Air.Flow)
freq_ord<-sort(freq, decreasing = TRUE)
moda<- names(freq_ord[1])
moda

boxplot(misdades)
mean(misdades$Air.Flow)
median(misdades$Water.Temp)
quantile(misdades$Acid.Conc.)
quantile(misdades$stack.loss, 0.18)


IQR(misdades$Water.Temp)
sd(misdades$Acid.Conc.)
var(misdades$Air.Flow)
