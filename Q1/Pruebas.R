#read.table(file="air.txt")
read.table(file="air.txt", header=TRUE, dec=",", na.strings='NA')
X<- c(-8, 5, 2, -8, 9, 5, 2, -3, 1, -1, 4, -4, 9, 3, -9, 7, 0, -7, 8, -4, 1, 7, -6, 4, 5, -9, -2, -8, 5, -5)
X
data.frame(x=X)
head(X,1)
