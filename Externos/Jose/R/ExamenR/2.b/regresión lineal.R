attach(anscombe)
X = x1[c(1:11)]
X
Y = y1[c(1:11)]
Y
model1 = lm(Y~X)
summary(model1)
b = model1$coef[1]
b
m = model1$coef[2]
m
si = m*8.5+b
si
plot(X,Y)