
nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71
dat <- data.frame(
  x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), 
                     nosim), 1, cfunc, 10),
        apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), 
                     nosim), 1, cfunc, 20),
        apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), 
                     nosim), 1, cfunc, 30)
  ),
  size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)




#lambdavals <- seq(0.005, 0.10, by = .01); nosim <- 1000
lambdavals <- 0.5

t <- 20
coverage <- function(lambda) {sapply(lambda, function(lambda){
  lhats <- rpois(nosim, lambda=lambda *t)/t
  ll <- lhats - qnorm(.975) * sqrt(lhats/t )
  ul <- lhats + qnorm(.975) * sqrt(lhats/t)
  mean(ll < lambda & ul > lambda)
})
}

n=40
coverage1 <- function(lambda) {sapply(lambda, function(lambda){
  lhats <- mean(rexp(n, lambda))
  ll <- lhats - qnorm(.975) * (lhats/sqrt(n))
  ul <- lhats + qnorm(.975) * (lhats/sqrt(n))
  (ll < (1/lambda) &  (1/lambda) < ul)
})
}

dat <- data.frame(lambda=seq(0.1,.9,.1),
                  coverage=sapply(seq(0.1,.9,.1),function(y) mean(sapply(1:1000,function(z) coverage1(y)))))

coverage2 <- function(n1) {sapply(lambda, function(lambda){
  lhats <- mean(rexp(n1, lambda))
  ll <- lhats - qnorm(.975) * (lhats/sqrt(n1))
  ul <- lhats + qnorm(.975) * (lhats/sqrt(n1))
  (ll < (1/lambda) &  (1/lambda) < ul)
})
}

dat1 <- data.frame(n=seq(5,100,5),
                  coverage=sapply(seq(5,100,5),function(y) mean(sapply(1:1000,function(z) coverage2(y)))))

ggplot(dat1,aes(x=n,y=coverage))+geom_line()+geom_point()+coord_cartesian(ylim=c(0,1))+scale_y_continuous(labels=percent)


#i am not sure anymore what I am supposd to be simulating, the mean of the dist or lambda
# in the binomila example it was calculating a franction and it seemd the same on the poisson
# ineed to watch this again

dat1 <- data.frame(x=seq(5,100,5))
dat1$y=coverage(dat1$x)
ggplot(data=NULL,aes(x=seq(1,100,5),y=coverage(seq(1,100,5))))+geom_line()
