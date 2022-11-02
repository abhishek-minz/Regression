#n is the sample size for the regression analysis
n<-50
a<-20
b<-5

meanx <- 20
sdx <- 5
xlim <- c(meanx-abs(b)*sdx, meanx+abs(b)*sdx)

meany <- a+b*meanx
sdy <- abs(b)*meanx
ylim <- c(meany-abs(b)*sdy, meany+abs(b)*sdy)

#Error is necessary in the regression equation

Error <- function(k)
    {
    rnorm(n, mean=0, sd=k*abs(b))
    }

corr.title <- function(x, y)
{
    paste('Correlation r=', round(cor(x,y),3), 
        '  r²=', round(cor(x,y)^2,3), sep='') 
}
#Here r is equal to 0.98

set.seed(2)
x <- rnorm(n, mean=meanx, sd=sdx)
y <- a+b*x+Error(1)
m <- lm(y~x)
plot(y~x, main=corr.title(x, y))
abline(m, col='red')

summary(m)
#par(mfrow=c(2,2))
#plot(lm(y~x))
