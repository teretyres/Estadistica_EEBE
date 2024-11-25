#Ejercicios propuestos 6
#Ej. 1
f<- function(x) {4/(pi*(1+x^2))}
curve(f,
      0,1,
      col="red",
      lwd=5, 
      main="Función de densidad",
      xlab="X",
      ylab="f(x)")

#simulacion
mu_x<- 0.4412712
sigma2_x<- 0.07851927

N<- 50
n<- 4
size_n<- n*N

x1<- seq(0,1,0.001)
f1<- f(x1)
set.seed(321)
samples<- sample(x1,size_n,replace = TRUE,prob = f1); samples

samples_x<- as.data.frame(matrix(samples, ncol=n))

sum_samples_x<- apply(samples_x,1,sum); sum_samples_x
media_sum_mues<- mean(sum_samples_x);media_sum_mues
var_sum_mues<- var(sum_samples_x); var_sum_mues

med_samples_x<- apply(samples_x,1,mean); med_samples_x
media_med_mues<- mean(med_samples_x); media_med_mues
var_med_mues<- var(med_samples_x); var_med_mues
hist(med_samples_x,prob=T)
curve(dnorm(x,mean=mu_x,sd=sqrt(sigma2_x/n)),
      add=T, lwd=2, col="red")

var_samples_x<- apply(samples_x,1,var);var_samples_x
media_var_mues<- mean(var_samples_x);media_var_mues

n2<- 50
pnorm(24, mean=50*mu_x,sd=sqrt(50*sigma2_x))-pnorm(17, mean=50*mu_x,sd=sqrt(50*sigma2_x))
pnorm(0.41, mean=mu_x,sd=sqrt(sigma2_x/50))-pnorm(0.39, mean=mu_x,sd=sqrt(sigma2_x/50))


#---------------------------------------------------------------




mu_y <- 80 # media
sigma2_y <- 15^2 #varianza
n <- 9


# Simula 100 muestras de tamaño n igual a 9 (utiliza la semila 321)
N <- 100 # Número de muestras
n <- 9 # tamaño de la muestra
n_size <- n*N # Simulación de N * n eventos

set.seed(321)
samples <- rnorm(n_size, mean=mu_y, sd=sqrt(sigma2_y)) # Simulación de N * n eventos


# Haz el histograma de la suma muestral
#¿La distribución de la suma muestral se puede aproximar a una N(nμ,nσ2)? Compare 
# el histograma y la distribución aproximada.
# SI
samples_Y <- as.data.frame(matrix(samples, ncol=n)) # Organización en un data.frame
sum_samples_Y <- apply(samples_Y,1,sum)
hist(sum_samples_Y,prob=T) #,ylim=c(0,0.01)
curve(dnorm(x,mean=n*mu_y,sd=sqrt(n*sigma2_y)),
      add=T, lwd=2, col="red")
# hist(sum_samples_Y,prob=T)
# suma_muestral <- recordPlot()
# dist_aprox <- function(x){dnorm(x,mean=n*mu_y,sd=sqrt(n*sigma2_y))}
# suma_muestral
# curve(dist_aprox, add=T, lwd=2, col="red")

# Haz el histograma de la media muestral
# ¿La distribución de la media muestral se puede aproximar a una N(μ,σ2/n)? Compare el histograma 
# y la distribución aproximada.
# SI
med_samples_Y <- apply(samples_Y,1,mean)
hist(med_samples_Y,prob=T) 
curve(dnorm(x,mean=mu_y,sd=sqrt(sigma2_y/n)),
      add=T, lwd=2, col="red")



# Haz el histograma de la varianza muestral
# ¿La distribución de la varianza muestral se puede aproximar a una 
# χ2n−1? Compare el histograma y la distribución aproximada.
# NO
var_samples_Y <- apply(samples_Y,1,var)
hist(var_samples_Y,prob=T)
curve(dchisq(x, df = n-1),
      add=T, lwd=2, col="red") 


# Haz el histograma del estadístico (n−1)S2σ2
# ¿La distribución del estadístico (n−1)S2σ2 se puede aproximar a una χ2n−1? Compare el histograma
# y la distribución aproximada.
# SI
stat <- (n-1)*var_samples_Y/sigma2_y;
hist(stat,prob=T)
curve(dchisq(x, df = n-1),
      add=T, lwd=2, col="red")





# La suma del peso de 9 hombres esté entre 700 y 800kg. #0.6339192
# P(700 <= S <= 800)
# P(S <= 800) - P(S <=700)
suma_P_800 <- pnorm(800, mean=n*mu_y, sd=sqrt(n*sigma2_y),lower.tail = TRUE)
suma_P_700 <- pnorm(700, mean=n*mu_y, sd=sqrt(n*sigma2_y),lower.tail = TRUE)
suma_P_800 - suma_P_700

# La media del peso de 9 hombres esté entre 78 y 80kg #0.1554217
# P(78 <= X_bar <= 80)
# P(S <= 80) - P(S <=78)
media_P_80 <- pnorm(80, mean=mu_y, sd=sqrt(sigma2_y/n),lower.tail = TRUE)
media_P_78 <- pnorm(78, mean=mu_y, sd=sqrt(sigma2_y/n),lower.tail = TRUE)
media_P_80 - media_P_78

# La varianza del peso de 9 hombres esté entre 200 y 250 #0.1729378
# P(200 <= S^2 <= 250)
# P(S^2 <= 250) - P(S^2 <=200)
var_P_250 <- pchisq(250*(n-1)/sigma2_y,df=n-1,lower.tail = TRUE)
var_P_200 <- pchisq(200*(n-1)/sigma2_y,df=n-1,lower.tail = TRUE)
var_P_250 - var_P_200
