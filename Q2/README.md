# Apuntes para el Q2
Los ficheros con apuntes son los siguientes:
- **[Basics](./Basics.R)**
  - Básicos aprendidos en Q1 que puede que sean necesarios para el Q2
- **Modelos discretos**
  - Breve Resumen
  - Como graficar f(x) y F(x)
  - Distr. Binomial
  - Distr. Hipergeométrica
  - Distr. Binomial negativa
  - Distr. de Poisson
- **Modelos contínuos**
  - Distr. Normal
  - Distr. Uniforme contínua
  - Distr. Exponencial
- **[Muestreo](./Muestreo.md)**
  - Apuntes de inferencia estadística
  - Media muestral
  - Suma muestral
  - Desviación típica muestral
  - TLC: Teorema del límite central

# Tutorials
Pequeños tid-bits de información respecto al funcionamiento de aquellos códigos que necesiten explicación.

(PD: Soko solo sube funciones, so para utlizarlas hay q marcarse un cntrl + A y cntrl + Enter; _osea seleccionar todo y ejecutar_ 😁)
 
## Probabilidad Distribución Soko
Realiza calculos de densidad y probabilidad acumulada de:
- **Binomiales** (pbinom_Zk y pbinom2_Zk)
- **Binomiales Negativas** (pnbinom_Zk y pnbinom_Zk)
- **Poisson** (ppois_Zk y ppois2_Zk)
- **Geometricas** (pgeom_Zk y pgeom2_Zk)
- **HyperGeometricas** (phyper_Zk y phyper2_Zk)
  
- **Uniformes** (punif_Zk y punif2_Zk)
- **X^2** (pchisq_Zk y pchisq2_Zk)
- **Normales** (pnorm_Zk y pnorm2_Zk)

### Funcionamiento
#### Densidad y probabilidad acumulada cuando P(X "_" x)
Das a la función pertinente los datos que pertoquen, _(X,n,k,N,p,...)_ y al final del todo tienes la opción de especificar el signo con _signo = "="_ (SI NO PONES LO DEL SIGNO, POR DEFECTO SERÁ UN "=").

**Como dar el signo**

Basta con poner en la ultima posición de los datos que te pide la función el signo de condición entre comillas.

_OPCIONES_: "=",">",">=","<","<=".

_DEFAULT_: "="

#### Probabilidad acumulada entre dos valores P(X1 " " X " " X2)
Same as before, pero ahora le das dos X, (X1 y X2) en ese orden concreto y el signo se expresan ambos con **un solo espacio** en medio.

**Como dar el signo**

_OPCIONES_: "< <", "<= <", "< <=", "<= <=", "> >", ">= >", "> >=",">= >="

_DEFAULT_: "< <"

