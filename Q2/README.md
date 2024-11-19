# Tutorials
Pequeños tid-bits de información respecto al funcionamiento de aquellos códigos que necesiten explicación.

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
