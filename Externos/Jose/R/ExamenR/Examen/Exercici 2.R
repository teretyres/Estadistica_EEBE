X = c(2,7,22,27,27,29,29,32,43,51,54,56,59,66,80)
Y = c(80,98,186,210,219,217,243,247,341,415,368,445,457,451,513)

x_val = 101

plot(X,Y, xlim=c(0,115), ylim=c(50,700))
lines(X,Y,type='l')

model = lm(Y~X)
summary(model)

y_val=coef(model)[1]+coef(model)[2]*x_val

points(x_val,y_val)

y_val