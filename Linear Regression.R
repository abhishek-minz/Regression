set.seed(2)
x <- rnorm(n, mean=meanx, sd=sdx)
y <- a+b*x+Error(1)
m <- lm(y~x)
plot(y~x, main=corr.title(x, y))
abline(m, col='red')

#Checking integration with git
z<-c(1,2)