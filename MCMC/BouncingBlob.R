
simTPM <- function(P=matrix(.5,nrow=2,ncol=2), x0=1, n=10) {
  x <- integer(10)
  x[1] <- x0
  for(i in 2:n) {
    x[i] <- sample(1:ncol(P), size=1, prob=P[x[i-1],])
  }
  x
}

library(expm)
PowerUpTPM <- function(P=matrix(.5,nrow=2,ncol=2), n=40) {
  if(ncol(P) != nrow(P)) stop("P must be a square matrix and it is not")
  P %^% n
}

# here is the bouncing blob transition probability matrix (tpm):
BB <- matrix(c(
  .2,.2,.2,.2,.2,
  .2,.3,.5,0,0,
  0,.3,.4,.3,0,
  0,0,.5,.3,.2,
  .2,.2,.2,.2,.2), byrow=T, ncol=5)


# simulate from the bouncing blob example
# plot some results and look at them compared to
# the limiting distribution of the tpm
x <- simTPM(BB, n=1000)
plot(x, type="l", lwd=.1)
table(x)/length(x)
PowerUpTPM(BB, 100)


