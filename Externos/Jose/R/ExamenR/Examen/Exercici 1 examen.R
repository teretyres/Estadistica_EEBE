X = c(13,17,23,32,36,38,45,45,57,59,66,66,69,75,78)
Y = c(306,338,373,412,426,433,453,453,481,485,498,498,503,513,518)

plot(X,Y, xlim=c(0,100), ylim=c(300,600))
lines(X,Y,type='l')

x_val = 96

model = lm(Y~log(X))
summary(model)

y_val = coef(model)[1] + coef(model)[2]*log(x_val)

points(x_val,y_val)

print(paste("Y:",y_val))

y_val