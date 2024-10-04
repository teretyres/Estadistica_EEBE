
#5 - TABLA DE FRECUENCIAS


#############################################################
#TIPO 1: hay 3 intervalos
#############################################################

datos<-  ;
var<-  ;
a1<-  ;
a2<-  ;
a3<-  ;

x<-datos[,var]
y1<-x[which(x<a1)]
N1<-length(y1)
y2<-x[which(x<a2)]
N2<-length(y2)
y3<-x[which(x<a3)]
N3<-length(y3)
n1<-N1
n2<-N2-N1
n3<-N3-N2
n<-n1+n2+n3
f1<-n1/n
f2<-n2/n
f3<-n3/n
F1<-N1/n
F2<-N2/n
F3<-N3/n
ni<-c(n1,n2,n3); 
Ni<-c(N1,N2,N3); 
fi<-c(f1,f2,f3); 
Fi<-c(F1,F2,F3); 
TablaFrec=cbind(ni,Ni,fi,Fi);
TablaFrec



#############################################################
#TIPO 2: hay 4 intervalos
#############################################################

datos<-  ;
var<-  ;
a1<-  ;
a2<-  ;
a3<-  ;
a4<-  ;

x<-datos[,var]
y1<-x[which(x<a1)]
N1<-length(y1)
y2<-x[which(x<a2)]
N2<-length(y2)
y3<-x[which(x<a3)]
N3<-length(y3)
y4<-x[which(x<a4)]
N4<-length(y4)
n1<-N1
n2<-N2-N1
n3<-N3-N2
n4<-N4-N3
n<-n1+n2+n3+n4
f1<-n1/n
f2<-n2/n
f3<-n3/n
f4<-n4/n
F1<-N1/n
F2<-N2/n
F3<-N3/n
F4<-N4/n
ni<-c(n1,n2,n3,n4); 
Ni<-c(N1,N2,N3,N4); 
fi<-c(f1,f2,f3,f4); 
Fi<-c(F1,F2,F3,F4); 
TablaFrec=cbind(ni,Ni,fi,Fi);
TablaFrec



#############################################################
#TIPO 3: hay 5 intervalos
#############################################################

datos<- ;
var<- ;
a1<- ;
a2<- ;
a3<- ;
a4<- ;
a5<- ;

x<-datos[,var]
y1<-x[which(x<a1)]
N1<-length(y1)
y2<-x[which(x<a2)]
N2<-length(y2)
y3<-x[which(x<a3)]
N3<-length(y3)
y4<-x[which(x<a4)]
N4<-length(y4)
y5<-x[which(x<a5)]
N5<-length(y5)
n1<-N1
n2<-N2-N1
n3<-N3-N2
n4<-N4-N3
n5<-N5-N4
n<-n1+n2+n3+n4+n5
f1<-n1/n
f2<-n2/n
f3<-n3/n
f4<-n4/n
f5<-n5/n
F1<-N1/n
F2<-N2/n
F3<-N3/n
F4<-N4/n
F5<-N5/n
ni<-c(n1,n2,n3,n4,n5); 
Ni<-c(N1,N2,N3,N4,N5); 
fi<-c(f1,f2,f3,f4,f5); 
Fi<-c(F1,F2,F3,F4,F5); 
TablaFrec=cbind(ni,Ni,fi,Fi);
TablaFrec



#############################################################
#TIPO 4: hay 6 intervalos
#############################################################

datos<-  ;
var<-  ;
a1<-  ;
a2<-  ;
a3<-  ;
a4<-  ;
a5<-  ;
a6<-  ;
x<-datos[,var]
y1<-x[which(x<a1)]
N1<-length(y1)
y2<-x[which(x<a2)]
N2<-length(y2)
y3<-x[which(x<a3)]
N3<-length(y3)
y4<-x[which(x<a4)]
N4<-length(y4)
y5<-x[which(x<a5)]
N5<-length(y5)
y6<-x[which(x<a6)]
N6<-length(y6)
n1<-N1
n2<-N2-N1
n3<-N3-N2
n4<-N4-N3
n5<-N5-N4
n6<-N6-N5
n<-n1+n2+n3+n4+n5+n6
f1<-n1/n
f2<-n2/n
f3<-n3/n
f4<-n4/n
f5<-n5/n
f6<-n6/n
F1<-N1/n
F2<-N2/n
F3<-N3/n
F4<-N4/n
F5<-N5/n
F6<-N6/n
ni<-c(n1,n2,n3,n4,n5,n6); 
Ni<-c(N1,N2,N3,N4,N5,N6); 
fi<-c(f1,f2,f3,f4,f5,f6); 
Fi<-c(F1,F2,F3,F4,F5,F6); 
TablaFrec=cbind(ni,Ni,fi,Fi);
TablaFrec




#############################################################
#TIPO 5: NO hay intervalos
#############################################################

########
#PASO 1:
########
datos<-mtcars;
var<-2;
x=datos[,var]
t=table(x)
l=length(t)
t

########
#PASO 2:
########
ni<-c(  )
Ni<-0
n=0
for (i in 1:l) {
  n=n+ni[i]
  Ni[i]=n
}
fi<-ni/n; 
Fi<-Ni/n;
TablaFrec=cbind(ni,Ni,fi,Fi);
TablaFrec

