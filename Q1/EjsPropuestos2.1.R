#Codigo Ejercicios propuestos S2.1
misdades<- stackloss
stackloss
21*4
#Tabla de frecuencias
ni<- table(misdades$Air.Flow) #cuantas veces sale cada cosa
fi<- ni/sum(ni) #Frecuencias relativas
Fi<- cumsum(fi) #suma acumulativa-> Frecuencia acumulada
Ni<- cumsum(ni) #n acumulada
cbind(ni, fi, Ni, Fi)

stem(misdades$Air.Flow)
barplot(misdades$Acid.Conc., horiz = TRUE)
mean(misdades$Air.Flow)
median(misdades$Air.Flow)

freq<-table(misdades$Air.Flow)
max_freq<- max(freq)
modas<- as.numeric(names(freq)[freq==max_freq])
modas
boxplot(misdades)
mean(misdades$Air.Flow)
median(misdades$Water.Temp)
quantile(misdades$Acid.Conc.)
q1<-quantile(misdades$stack.loss, 0.18)
q2<- quantile(misdades$stack.loss, 0.82)
q2-q1

IQR(misdades$Water.Temp)
sd(misdades$Acid.Conc.)
var(misdades$Air.Flow)
