#----VAD----

p -> prob de éxito
n -> nº ensayos
r -> nº éxitos fijado

#Distribución Binomial
f(x)=x/n*p^x*(1-p)^(n-x) #x=0,1,2...
E(X)=n*p
V(X)=n*p*(1-p)

#Distribución geométrica
f(x)=p*(1-p)^(x-1) #x=1,2...
E(X)=1/p
V(X)=(1-p)/p^2

#Distribución Binomial Negativa
f(x)=[x-1;r-1]*p^r*(1-p)^(x-r) #x=r,r+1,r+2...
E(X)=r/p
V(X)=r*(1-p)/p^2

#Distribución hipergeométrica
Población de N éxitos y N-k fracasos
f(x)=[k; n][N-k; n-x]/[N; k] #x=max{0,n+k-N},...,min{k,n}
E(X)=n*p   p=k/N
V(X)=n*p*(1-p)*(N-n)/(N-1)

#Distribución de Poisson
λ -> frecuencia de ocurrencia media
f(x)=e^(-λ)*λ^x/x! #x=0,1,2...
E(X)=λ
V(X)=λ

#----VAC----

#Distribución Uniforme
f(x)=1/(b-a) #x€[a,b]
E(X)=(a+b)/2
V(X)=σ^2=(b-a)^2/12

#Distribución exponencial
f(x)=λ*e^(-λ*x) #x€R+
E(X)=1/λ
V(X)=σ^2=1/λ^2

#Distribución Normal
f(x)=1/(σ*sqrt(2*pi))*e^(-1/2*(x-μ/σ)^2) #x€[a,b]
E(X)=μ
V(X)=σ^2


