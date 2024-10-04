#Examen Mireia
mis_dades=mtcars;mis_dades
mitjana_qsecz<-mean(mis_dades$qsec);mitjana_qsecz
mediana_disp<-median(mis_dades$disp);mediana_disp
quartil<-summary(mis_dades$drat);quartil
quantil<-quantile(mis_dades$wt,0.18);quantil
rang<-IQR(mis_dades$qsec);rang
desviacio<-sd(mis_dades$wt);desviacio
varian<-var(mis_dades$hp);varian

#Exercici2
x<-c(seq(from=0,to=21,by=1)*pi/21);x
sumax<-sum(x);sumax
y<-x[-c(4,6,3)];y
calc<-sum(sin(x))-sum(cos(y));calc
pos9<-y[9];pos9
pos13<-y[13];pos13

#Exercici3
r<-sqrt(0.8993);r
predicció3<- 16.1689*1.88+3.5594;predicció3
abline(a=16.1689,b=3.5594,col='blue',lwd=3)


#Exercici4
Dies<-c (3,6,9,11,14,20,30,31,36,37,43,50,62,74,76)
Ordinadors<-c(114,181,221,240,264,299,338,341,356,358,372,388,408,426,429)
plot(Ordinadors~Dies)
model2=lm(Ordinadors~Dies);model2
summary(model2)

#S'ha de fer l'aroximació per exponencials
model3=lm(log(Ordinadors)~Dies);model3
summary(model3)
Dies_pred=103
Log_Ordinadors_pred=predict(model3,data.frame(Dies=Dies_pred));
Log_Ordinadors_pred
Ordinadors_pred=exp(Log_Ordinadors_pred);Ordinadors_pred

#Exercici5
dades<-mtcars
head(dades) #mirar les primeres observacions
nrow(dades) #mirar el numero de files
length(dades)#mirar el numero de columnes
ni = table(dades$vs) #Frecuencia absoluta
fi = table(dades$vs)/length(dades$vs) #Frecuencia relativa
Ni = cumsum(ni) #Frecuencia absoluta acumulada
Fi = cumsum(fi) #Frecuencia relativa acumulada
Tabla_Frec = cbind(ni,fi,Ni,Fi);Tabla_Frec 

#Exerici6
rm(list=ls())
data<-mtcars
barplot(table(data$am))

#Exercici7
mis_dades1<-iris;
mode10=lm(mis_dades1$Petal.Length~mis_dades1$Sepal.Length);mode10 # lm=liniarmodel
summary(mode10)
b<-mode10$coef[1];b #dona la b
m<-mode10$coef[2];m #dona la m
prediccio1<-1.858433*5+-7.101443 ;prediccio1