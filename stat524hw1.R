gamma.mles <- function(sample){
  i <- 1
  smean <- mean(sample)
  s <- log(((prod(sample^(1/length(sample))))/smean))
  k0 <- 3.5
  repeat{
    k <- k0 - (digamma(k0)-log(k0)-s)/(trigamma(k0)-(1/k0))
    if(abs(k-k0) <= .00000001 || i >= 100){break}
    k0 <- k
    i <- i+1
  }
  beta = smean/k0
  print(c(k0,beta))
}

gamma.mles(c(7.1,3.6,4.1,2.9,4.2,3.1,5.6))
