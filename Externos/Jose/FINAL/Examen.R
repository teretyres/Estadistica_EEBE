rm(list=ls())
mat = c(14.1, 14.5, 15.5, 16.0, 16.0, 16.7, 16.9, 17.1, 17.5, 17.8, 17.8, 18.1, 18.2, 18.3, 18.3, 19.0, 19.2, 19.4, 20.0, 20.0, 20.8, 20.8, 21.0, 21.5, 23.5, 27.5, 27.5, 28.0, 28.3, 30.0, 30.0, 31.6, 31.7, 31.7, 32.5, 33.5, 33.9, 35.0, 35.0, 35.0, 36.7, 40.0, 40.0, 41.3, 41.7, 47.5, 50.0, 51.0, 51.8, 54.4, 55.0, 57.0)
n = 52
mu = 30
hist(mat)
x = sum(mat)/n #mean(mat)
s2 = (sum((mat-x)^2))/(n-1)
s = sqrt(s2)
z = (x-mu)/(s/sqrt(n))
dd

























rm(list=ls())
#Var(X)=E(X^2)-(E(X))^2

##Caso 1: sd conocida, dist normal
#El int es; [mean-Zalpha/2*sd/sqrt(n) ; mean+Zalpha/2*sd/sqrt(n)]
gamma = 0.98 #Nivel de confianza
alpha = 1 - gamma #Nivel de significación
Z = qnorm(1-alpha/2,0,1); Z #Es Z alpha/2, F^(-1)(0.975)=1,96
alphaedos = pnorm(Z,0,1) #F(0.36)=0.64058 --> Para calcular Probabilidades, errores tipo.
alphaprima = (1-alphaedos)*2; alphaprima #Si me dan int y me preguntan alpha
gammaprima = 1-alphaprima; gammaprima #Si me dan int y me preguntan gamma (nivel confianza)

#En hipótesis, se acepta H0 si: 
  #Bilateral: Zalpha/2 <= Zobs <= Zalpha/2; con p=2-2F(Zobs), si p >= alpha
    #H0: mu = mu0, H1: mu != mu0
alpha = 0.05
Z = qnorm(1-alpha/2,0,1); Z #Es Z alpha/2
Zobs = (mean()-mu)/(sd/sqrt(n)) #sd o en su defecto s.
f = pnorm(Zobs,0,1) #Es F(Zobs)
p = 2-2*f

  #Unilateral superior: se acepta H0 si Zobs <= Zalpha, o si p >= alpha
    #H0: mu <=(=) mu0 (valor estándar, de fábrica...), H1: mu > mu0 (valor con muestras) (rechazo arriba, a la dcha)
Z = qnorm(1-alpha,0,1); Z #Es Z alpha
Zobs = (mean()-mu)/(sd/sqrt(n)) #sd o en su defecto s. Para tener el valor crítico de la mean, hacer Zalpha = Zobs
p = 1 - pnorm(Zobs,0,1) #Es p = 1 - F(Zobs)

  #Unilateral inferior: se acepta H0 si Zobs >= Zalpha, o p >= alpha
    #H0: mu >=(=) mu0, H1: mu < mu0 (rechazo por abajo)
Z = qnorm(alpha,0,1); Z #Es Z alpha
Zobs = (mean()-mu)/(sd/sqrt(n)) #sd o en su defecto s
p = pnorm(Zobs,0,1) #Es p = F(Zobs)

##Caso 2: desconocemos la dist, sabemos sd (σ) y n >= 30, estimar con media muestral
  #Procedemos igual que Caso 1


##Caso 3: t student, mu y sd desconocidos, n cualquiera. Usar la var muestral mod s^2 con (n-1) SIEMPRE.
#El int será: [mean-talpha/2*s/sqrt(n) ; mean+talpha/2*s/sqrt(n)]
gamma = 0.99 #Nivel de confianza
alpha = 1 - gamma #Nivel de significación
n = 5 #Tamaño de la muestra, n-1 gl
t = qt(1-alpha/2, n-1); t #Es t alpha/2
aedos = pt(t,n-1,lower.tail=FALSE); aedos #Esto es para hacer lo inverso y saber alpha/2!!
#[mean()-t*s/sqrt(n) ; mean()+t*s/sqrt(n)]
##S es la desviación estándar de la muestra!!! --> Pasarla a s(n-1) para usar en tStudent, s(n) se puede en chi2
##σ es la desviación estándar de la población!!!

#Hipótesis: tobs = (mean()-mu0)/(s/sqrt(n)), con s siendo s(n-1)
  #Unilateral superior: Si tobs < talpha, o p >= alpha, se mantiene H0
alpha = 0.02
n = 11
t = qt(1-alpha,n-1); t #Esta es talpha
tobs = (mean()-mu0)/(s/sqrt(n))
p = 1 - pt(tobs,n-1); p

  #Unilateral inferior: Si tobs > talpha, o p >= alpha, se mantiene H0
t = qt(alpha,n-1); t #Esta es talpha
tobs = (mean()obs-mu0)/(s/sqrt(n))
p = pt(tobs,n-1); p


##Caso 4: se quiere estimar la varianza (σ^2), se desconoce mu y sd, n cualquiera. Chi cuadrado
#Usar Var quasi muestral s^2=Sum(xi-x)^2/(n-1)
#El int para Var es: [(n-1)*s^2/b ; (n-1)*s^2/a]              ##Estas dos s son s^2(n-1) (la calculo con muestra)
#El int para sd es: (sqrt((n-1)*s^2/b) ; sqrt((n-1)*s^2/a))   ##Estas dos s son s^2(n-1) (la calculo con muestra)
#El int para Var es: [n*s^2/b ; n*s^2/a]                      ##Estas dos s son s^2(n) = σ^2 (me la dan)
#a es el lateral inferior, b el superior
gamma = 0.95
n = 15
alpha = 1 - gamma
a = qchisq(alpha/2,n-1,lower.tail=TRUE); a
b = qchisq(alpha/2,n-1,lower.tail=FALSE); b
b = qchisq(1-alpha/2,n-1,lower.tail=TRUE); b
chi2n-1 = (n-1)*s^2/sd^2 #S^2 es s^2(n-1)
#Las b deberían ser iguales

#Var = σ^2, sd = σ
#En hipótesis, se acepta H0 si: (int de aceptación de H0: [aci,acd]) 
  #Bilateral: aci <= chi2obs <= acd; con p=2-2F(Zobs), si p >= alpha
    #H0: Var = Var0, H1: Var != Var0
aci = qchisq(alpha/2,n-1,lower.tail=TRUE) #Es aci, borde inferior
acd = qchisq(alpha/2,n-1,lower.tail=FALSE) #Es acd, borde superior
chi2obs = (n-1)*s^2/sd^2 #sd = σ, s^2 es la s^2(n-1)
f = pnorm(Zobs,0,1) #Es F(Zobs)
p = 2*min{P(chi2n-1<=chi2obs),P(chi2n-1>=chi2obs)}

  #Unilateral superior: se acepta H0 si chi2obs <= acd, o si p >= alpha, (-inf,acd]
    #H0: Var <=(=) Var0, H1: Var > Var0 (rechazo arriba, a la dcha)
acd = qchisq(alpha,n-1,lower.tail=FALSE)
chi2obs = (n-1)*s^2/sd^2 #sd = σ
p = pchisq(chi2obs,n-1,lower.tail=FALSE) #p = P(chi2n-1>=chi2obs)

  #Unilateral inferior: se acepta H0 si chi2obs >= aci, o p >= alpha, [aci,+inf)
    #H0: Var >=(=) Var0, H1: Var < Var0 (rechazo por abajo)
aci = qchisq(alpha,n-1,lower.tail=TRUE)
chi2obs = (n-1)*s^2/sd^2 #sd = σ
p = P(chi2n-1<=chi2obs) #Sigue...
p = 2*(1-pchisq(chi2obs,n-1,lower.tail=TRUE)) #p/2 = P(chi2n-1<=chi2obs)
#E(chi2n-1) = n-1; Var(chi2n-1) = sqrt(2*(n-1))


##Caso 5: Para una proporcion con dist binom, aprox por normal si: 
# n>=30, np>=5, n(1-p)>=5 
#E(mean)=p; σ(mean)=sqrt(p*(1-p)/n)
#El int es [p-zalpha/2*sqrt(p*(1-p)/n) ; p+zalpha/2*sqrt(p*(1-p)/n)]
#zalpha/2 igual que caso 1.
#Para calcular la n, y te dan la long (w) del int, se hace:
n = Z^2/w^2 #Ya que no se usa la p calcuada, sino para la que n es max, p=0.5.

#En hipótesis, se acepta H0 si (como Caso 1): 
  #Bilateral: Zalpha/2 <= Zobs <= Zalpha/2; con p=2-2F(Zobs), si p >= alpha
    #H0: p=p0, H1: p != p0
n = 300
p0 = 0.03
pobs = 12/n
Z = qnorm(1-alpha/2,0,1); Z #Es Z alpha/2
Zobs = (pobs-p0)/sqrt(p0*(1-p0)/n) #Este es el estadístico
f = pnorm(Zobs,0,1) #Es F(Zobs)
p = 2-2*f #Esto es el Valor P, no la probabilidad ni nada que me den

  #Unilateral superior: se acepta H0 si Zobs <= Zalpha, o si p >=(>) alpha
    #H0: mu <= mu0, H1: p > p0 (rechazo arriba, a la dcha)
Z = qnorm(1-alpha,0,1); Z #Es Z alpha
Zobs = (pobs-p0)/sqrt(p0*(1-p0)/n)
p = 1 - pnorm(Zobs,0,1) #Es p = 1 - F(Zobs)
#Esto es el Valor P, no la probabilidad ni nada que me den

  #Unilateral inferior: se acepta H0 si Zobs >= Zalpha, o p >= alpha
    #H0: p >= p0, H1: p < p0 (rechazo por abajo)
Z = qnorm(alpha,0,1); Z #Es Z alpha
Zobs = (pobs-p0)/sqrt(p0*(1-p0)/n)
p = pnorm(Zobs,0,1) #Es p = F(Zobs)
#Esto es el Valor P, no la probabilidad ni nada que me den

#Error tipo 1: aceptar H1 cuando realmente H0 es cierta (falso +)
  #P(Rechazar H0 | H0 cierta) = Alpha
#Error tipo 2: aceptar H0 cuando realmente H1 es cierta (falso -)
  #P(Aceptar H0 | H0 falsa) = Beta ###Hacer con X (mean, media muestral) crítica.
    #Xcritica de calcula con Zalpha y el mu0 anterior, no el q me dice ese apartado.
    #Y calcular Zalpha(nueva) con ese valor Xcritica y el actual mu0.
    #Beta = P(Z<>Zalphanueva|mu=muactual)
  #Lo puedo calcular con lo del valor p. Si tengo Unilat Sup, usar valor p de Unilat Inf.
    #Si tengo Hipótesis Unilat Inf, usar valor p = 1 - pnorm(Zalphanueva).
#Alpha es la probabilidad máxima de cometer error tipo 1
#Beta es la probabilidad máxima de cometer error tipo 2, siendo Potencia = 1 - Beta



