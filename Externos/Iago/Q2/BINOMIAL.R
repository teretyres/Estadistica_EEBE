#BINOMIAL


    probabilidad <- ;

    n1 <-  ;
    valor1 <- ;#exactamente
    
    n2 <- ;
    valor2 <- ;#mayor o igual

    n3 <- ;    

    prob3 <- ;
    
    n_media <-  ;
    

a_mayoroigual <- pbinom(c(valor1-1),size=n1,prob=probabilidad,lower.tail=FALSE); 
a_exactamente <- dbinom(c(valor1),size=n1,prob=probabilidad); 

b_mayoroigual<- pbinom(c(valor2-1),size=n2,prob=probabilidad,lower.tail=FALSE); 

c1  <- qbinom(c(prob3),size=n3,prob=probabilidad,lower.tail=FALSE);
c1=c1-1;
pc1 <- pbinom(c(c1-1),size=n3,prob=probabilidad,lower.tail=FALSE);
c2  <- qbinom(c(prob3),size=n3,prob=probabilidad,lower.tail=FALSE); 
pc2 <- pbinom(c(c2),size=n3,prob=probabilidad,lower.tail=FALSE);

n4 <- 1000000
trials <- rbinom(n4, size=n_media,prob=probabilidad) 
media <- mean(trials);

ini <-  n1*probabilidad-4*sqrt(n1*probabilidad*(1-probabilidad));
fin <-  n1*probabilidad+4*sqrt(n1*probabilidad*(1-probabilidad));
x=seq(floor(ini),floor(fin),by=1);
fx=dbinom(x, size=n1, prob=probabilidad);
plot(x, fx, type="h", col="red", lwd=3, xlab="X", ylab="f(x)")
     
          

a_mayoroigual
a_exactamente
b_mayoroigual
c1
pc1
c2
pc2
media