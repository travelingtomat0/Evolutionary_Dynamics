library(deSolve)

params <- c()
my.atol <- 1e-06

l <- -5
K <- 3

times <- (0:100)/25

sdiffeqns <- function(t, s, params) {
  sd1 <- l * s[1] * (K-s[1])
  list(c(sd1))
}

initconds <- 0 - 1e-06
out0m = lsoda(initconds, times, sdiffeqns, rtol=1e-10, atol=my.atol)

initconds <- 0 +  1e-06
out0p = lsoda(initconds, times, sdiffeqns, rtol=1e-10, atol=my.atol)

initconds <- 1 -  1e-06
out1m = lsoda(initconds, times, sdiffeqns, rtol=1e-10, atol=my.atol)

initconds <- 1 +  1e-06
out1p = lsoda(initconds, times, sdiffeqns, rtol=1e-10, atol=my.atol)

print("Plotting...")
svg("numerical_integration2.svg")
plot(out0p, xlab="time", ylab="x", main="(K=3, l=-5)", col="dodgerblue",
     lty=2, lwd=2)#, ylim=c(-2,4), xlim=c(0,4))
lines(out0m, col="#ff8c00", lty = 3, lwd = 3)
lines(out1m, col="#68228b", lty = 2, lwd = 2)
lines(out1p, col="#cd2626", lty = 3, lwd = 3)
