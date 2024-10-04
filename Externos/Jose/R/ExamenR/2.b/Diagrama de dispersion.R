altura = c(178, 181, 168, 183, 164, 181, 174, 176, 174, 176, 181, 168, 164, 174, 171)
peso = c(82, 89, 68, 91, 65, 80, 79, 81, 80, 79, 82, 69, 67, 80, 78)
model1 = lm(peso~altura)
plot(altura,peso, pch=19, col="red")
abline(model1,col="blue",lwd=3)
summary(model1)
m = model1$coef[1]
b = model1$coef[2]
m
b
