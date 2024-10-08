altura<- c(178, 181, 168, 183, 164, 181, 174, 176, 174, 176, 181, 168, 164, 174, 171)
peso<- c(82, 89, 68, 91, 65, 80, 79, 81, 80, 79, 82, 69, 67, 80, 78)

plot(altura, peso)
x<- altura
y<- peso
xbar<- mean(x)
ybar<- mean(y)
m<- (sum((x-xbar)*(y-ybar)))/(sum((x-xbar)^2)) #derivada por minimos cuadrados
b<- ybar-m*xbar
m
b
mod<- lm(y~x) #modelo lineal
summary(mod)
0.8576^2
1.1533*173-122.8993
ypredicted<-m*x+b
x=(y-b)/m
(72+122.8993)/1.1533
Rsq <- (sum(((ypredicted-ybar)^2)))/(sum(((y-ybar)^2))) #coeficiente de determinacion
R <- sqrt(Rsq) #coeficiente de correlacion
Rsq
