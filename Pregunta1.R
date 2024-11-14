#poblacion X <- N(mu,sigma^2) ej1 (cables)
mu <- 93.5
sigma <- 5.7
curve(dnorm(x,mu,sigma),xlim=c(80,120),col='red')
#a)
set.seed(123)
rnorm(4,mu,sigma)
Y <- function(i) {sum(rnorm(4,mu,sigma))}
Y(2)

Y100000 <- sapply(1:100000, Y)
Y100000
hist(Y100000)
mean(Y100000)
#teoricamente
4*mu

#b)
Y <- function(i) {sum(rnorm(100,mu,sigma))}
Y500000 <- sapply(1:500000,Y)
var(Y500000)
100*sigma^2

hist(Y500000,freq=FALSE)
curve(dnorm(x,100*mu,sqrt(100)*sigma), add=TRUE,col='red')

#c)
1-pnorm(103,mu,sigma)

#d)
Xbar <- function(i) {mean(rnorm(4,mu,sigma))}
Xbar500000 <- sapply(1:500000,Xbar)
hist(Xbar500000,freq=FALSE)
mean(Xbar500000>98)
#teoricamente
1-pnorm(98,mu,sigma/sqrt(4))
hist(Xbar500000,freq=FALSE)
curve(dnorm(x,mu,sigma/sqrt(4)),add=TRUE,col='red')

#e)el enunciado quiere decir 32 y no 98
Ssq <- function(i) {var(rnorm(100,mu,sigma))}
Ssq500000 <- sapply(1:500000,Ssq)
hist(Ssq500000,freq=FALSE)
mean(Ssq500000>32)
#teoricamente
hist(Ssq500000*(100-1)/sigma^2,freq=FALSE)
32*(100-1)/sigma^2
curve(dchisq(x,100-1),col='red',add=TRUE)
1-pchisq(32*(100-1)/sigma^2,100-1)

mean(Ssq500000>32)
