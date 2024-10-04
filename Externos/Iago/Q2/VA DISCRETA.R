#VARIABLE ALEATORIA DISCRETA

    a <- ;

    b <- ;

    h <- ;
    
    fx <- c(    );

x=seq(a,b,by=h);
sumaProb <- sum(fx); 
media <- sum(x*fx); media

    
ini <- 1;
fin <- 1;
probabilidad <- 0;
while (probabilidad<0.5){
  aux=seq(ini,fin,by=1);
  probabilidad <- sum(fx[c(aux)]); 
  mediana <- x[fin]; 
  fin <- fin+1;
}

esp2 <- sum(x^2*fx);
varianza <- esp2-media*media; 

plot(x, fx, type="h", col="red", lwd=3, main="VAD", xlab="X", ylab="f(x)")


sumaProb
media
mediana
varianza



#  PREGUNTA TEORICA

# 1)Si nos definen una n

#      trials = sample(seq(0,n),1000,replace=T,prob=fx)

# 2)Si nos definen un vector x

#      trials = sample(x,1000,replace=T,fx)


