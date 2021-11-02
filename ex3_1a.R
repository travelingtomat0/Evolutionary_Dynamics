library(ggplot2)

N <- c(10, 100, 1000000)

I <- N/2

first <- TRUE
for(j in c(1, 2, 3)) {
  res <- c()
  n <- N[j]
  i <- I[j]

  print("i, N:")
  print(c(I[j], n))

  V_1 <- ((2*i)/n) * (1-i/n)
  for(t in 1:100) {
    tmp <- V_1*(1-(1-2/n^2)^t)/(2/n^2)
    res <- c(res, tmp)
    print(c(t, tmp))
  }

  t <- 1
  res2 <- c()
  for(n in 1:10000) {
    i <- n/2
    V_1 <- ((2*i)/n) * (1-i/n)
    #print((1-(1-2/n^2)^t)/(2/n^2))
    tmp <- V_1*(1-(1-2/n^2)^t)/(2/n^2)
    res2 <- c(res2, tmp)
  }

  if(first) {
    plotting <- data.frame(res)
    plotting2 <- data.frame(res2)
    first <- FALSE
  } else {
    plotting <- cbind(plotting, res)
    plotting2 <- cbind(plotting2, res2)
  }
}

xs = 1:100
#print(plotting)
svg("test_plot.svg")
plot(x= xs, y=plotting[,3], xlab="time", ylab="f", main="", col="dodgerblue",
     lty=2, lwd=2)

xs <- seq_along(plotting2[, 3])
#print(plotting)

#print(plotting2)

svg("test_plot_2.svg")
plot(x=xs, y=plotting2[,2], xlab="time", ylab="f", main="", col="dodgerblue",
     lty=2, lwd=2)
lines(x = xs, y = plotting2[,2])


### -------------------------------------

condv <- function(t, i, n) {
  V_1 <- ((2*i)/n) * (1-i/n)
  return(V_1*(1-(1-2/n^2)^t)/(2/n^2))
}

i <- 1
df <- data.frame(integer(100))
for(n in c(10, 100, 10000, 10000)) {
  res <- c()
  for(t in 1:100) {
    res <- c(res, condv(t, i, n))
  }
  df <- cbind(df, res)
}

df <- df[,-c(1)]

ms <- c()
for(i in c(1, 2, 3, 4)) {
  ms <- c(ms, mean(df[,i]))
}

print(df)

xs <- 1:100
svg("test_plot_3.svg")
plot(x=xs, y=df[,1], xlab="t", ylab="Var[X(t)|X(0)=i=1]", main="", col="dodgerblue", type='l')
lines(x = xs, y = df[,2], col='green')
lines(x = xs, y = df[,3], col='red')
lines(x = xs, y = df[,4], col='purple')
legend('top',inset=0.05,c("N=10","N=100", "N=1 000", "N=10 000"),lty=1,col=c("dodgerblue","green", "red", "purple"),title="Graph type")

print(ms)